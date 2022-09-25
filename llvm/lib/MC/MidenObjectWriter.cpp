//===- lib/MC/MidenObjectWriter.cpp - Miden File Writer ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements Miden object file writer information.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/Miden.h"
#include "llvm/BinaryFormat/MidenTraits.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCMidenObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSectionMiden.h"
#include "llvm/MC/MCSymbolMiden.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/LEB128.h"
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "mc"

namespace {

// When we create the indirect function table we start at 1, so that there is
// and empty slot at 0 and therefore calling a null function pointer will trap.
static const uint32_t InitialTableOffset = 1;

// For patching purposes, we need to remember where each section starts, both
// for patching up the section size field, and for patching up references to
// locations within the section.
struct SectionBookkeeping {
  // Where the size of the section is written.
  uint64_t SizeOffset;
  // Where the section header ends (without custom section name).
  uint64_t PayloadOffset;
  // Where the contents of the section starts.
  uint64_t ContentsOffset;
  uint32_t Index;
};

// A miden data segment.  A miden binary contains only a single data section
// but that can contain many segments, each with their own virtual location
// in memory.  Each MCSection data created by llvm is modeled as its own
// miden data segment.
struct MidenDataSegment {
  MCSectionMiden *Section;
  StringRef Name;
  uint32_t InitFlags;
  uint64_t Offset;
  uint32_t Alignment;
  uint32_t LinkingFlags;
  SmallVector<char, 4> Data;
};

// A miden function to be written into the function section.
struct MidenFunction {
  uint32_t SigIndex;
  const MCSymbolMiden *Sym;
};

// A miden global to be written into the global section.
struct MidenGlobal {
  miden::MidenGlobalType Type;
  uint64_t InitialValue;
};

// Information about a single item which is part of a COMDAT.  For each data
// segment or function which is in the COMDAT, there is a corresponding
// MidenComdatEntry.
struct MidenComdatEntry {
  unsigned Kind;
  uint32_t Index;
};

// Information about a single relocation.
struct MidenRelocationEntry {
  uint64_t Offset;                   // Where is the relocation.
  const MCSymbolMiden *Symbol;        // The symbol to relocate with.
  int64_t Addend;                    // A value to add to the symbol.
  unsigned Type;                     // The type of the relocation.
  const MCSectionMiden *FixupSection; // The section the relocation is targeting.

  MidenRelocationEntry(uint64_t Offset, const MCSymbolMiden *Symbol,
                      int64_t Addend, unsigned Type,
                      const MCSectionMiden *FixupSection)
      : Offset(Offset), Symbol(Symbol), Addend(Addend), Type(Type),
        FixupSection(FixupSection) {}

  bool hasAddend() const { return miden::relocTypeHasAddend(Type); }

  void print(raw_ostream &Out) const {
    Out << miden::relocTypetoString(Type) << " Off=" << Offset
        << ", Sym=" << *Symbol << ", Addend=" << Addend
        << ", FixupSection=" << FixupSection->getName();
  }

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const { print(dbgs()); }
#endif
};

static const uint32_t InvalidIndex = -1;

struct MidenCustomSection {

  StringRef Name;
  MCSectionMiden *Section;

  uint32_t OutputContentsOffset = 0;
  uint32_t OutputIndex = InvalidIndex;

  MidenCustomSection(StringRef Name, MCSectionMiden *Section)
      : Name(Name), Section(Section) {}
};

#if !defined(NDEBUG)
raw_ostream &operator<<(raw_ostream &OS, const MidenRelocationEntry &Rel) {
  Rel.print(OS);
  return OS;
}
#endif

// Write Value as an (unsigned) LEB value at offset Offset in Stream, padded
// to allow patching.
template <typename T, int W>
void writePatchableULEB(raw_pwrite_stream &Stream, T Value, uint64_t Offset) {
  uint8_t Buffer[W];
  unsigned SizeLen = encodeULEB128(Value, Buffer, W);
  assert(SizeLen == W);
  Stream.pwrite((char *)Buffer, SizeLen, Offset);
}

// Write Value as an signed LEB value at offset Offset in Stream, padded
// to allow patching.
template <typename T, int W>
void writePatchableSLEB(raw_pwrite_stream &Stream, T Value, uint64_t Offset) {
  uint8_t Buffer[W];
  unsigned SizeLen = encodeSLEB128(Value, Buffer, W);
  assert(SizeLen == W);
  Stream.pwrite((char *)Buffer, SizeLen, Offset);
}

static void writePatchableU32(raw_pwrite_stream &Stream, uint32_t Value,
                              uint64_t Offset) {
  writePatchableULEB<uint32_t, 5>(Stream, Value, Offset);
}

static void writePatchableS32(raw_pwrite_stream &Stream, int32_t Value,
                              uint64_t Offset) {
  writePatchableSLEB<int32_t, 5>(Stream, Value, Offset);
}

static void writePatchableU64(raw_pwrite_stream &Stream, uint64_t Value,
                              uint64_t Offset) {
  writePatchableSLEB<uint64_t, 10>(Stream, Value, Offset);
}

static void writePatchableS64(raw_pwrite_stream &Stream, int64_t Value,
                              uint64_t Offset) {
  writePatchableSLEB<int64_t, 10>(Stream, Value, Offset);
}

// Write Value as a plain integer value at offset Offset in Stream.
static void patchI32(raw_pwrite_stream &Stream, uint32_t Value,
                     uint64_t Offset) {
  uint8_t Buffer[4];
  support::endian::write32le(Buffer, Value);
  Stream.pwrite((char *)Buffer, sizeof(Buffer), Offset);
}

static void patchI64(raw_pwrite_stream &Stream, uint64_t Value,
                     uint64_t Offset) {
  uint8_t Buffer[8];
  support::endian::write64le(Buffer, Value);
  Stream.pwrite((char *)Buffer, sizeof(Buffer), Offset);
}

bool isDwoSection(const MCSection &Sec) {
  return Sec.getName().endswith(".dwo");
}

class MidenObjectWriter : public MCObjectWriter {
  support::endian::Writer *W;

  /// The target specific Miden writer instance.
  std::unique_ptr<MCMidenObjectTargetWriter> TargetObjectWriter;

  // Relocations for fixing up references in the code section.
  std::vector<MidenRelocationEntry> CodeRelocations;
  // Relocations for fixing up references in the data section.
  std::vector<MidenRelocationEntry> DataRelocations;

  // Index values to use for fixing up call_indirect type indices.
  // Maps function symbols to the index of the type of the function
  DenseMap<const MCSymbolMiden *, uint32_t> TypeIndices;
  // Maps function symbols to the table element index space. Used
  // for TABLE_INDEX relocation types (i.e. address taken functions).
  DenseMap<const MCSymbolMiden *, uint32_t> TableIndices;
  // Maps function/global/table symbols to the
  // function/global/table/tag/section index space.
  DenseMap<const MCSymbolMiden *, uint32_t> MidenIndices;
  DenseMap<const MCSymbolMiden *, uint32_t> GOTIndices;
  // Maps data symbols to the Miden segment and offset/size with the segment.
  DenseMap<const MCSymbolMiden *, miden::MidenDataReference> DataLocations;

  // Stores output data (index, relocations, content offset) for custom
  // section.
  std::vector<MidenCustomSection> CustomSections;
  std::unique_ptr<MidenCustomSection> ProducersSection;
  std::unique_ptr<MidenCustomSection> TargetFeaturesSection;
  // Relocations for fixing up references in the custom sections.
  DenseMap<const MCSectionMiden *, std::vector<MidenRelocationEntry>>
      CustomSectionsRelocations;

  // Map from section to defining function symbol.
  DenseMap<const MCSection *, const MCSymbol *> SectionFunctions;

  DenseMap<miden::MidenSignature, uint32_t> SignatureIndices;
  SmallVector<miden::MidenSignature, 4> Signatures;
  SmallVector<MidenDataSegment, 4> DataSegments;
  unsigned NumFunctionImports = 0;
  unsigned NumGlobalImports = 0;
  unsigned NumTableImports = 0;
  unsigned NumTagImports = 0;
  uint32_t SectionCount = 0;

  enum class DwoMode {
    AllSections,
    NonDwoOnly,
    DwoOnly,
  };
  bool IsSplitDwarf = false;
  raw_pwrite_stream *OS = nullptr;
  raw_pwrite_stream *DwoOS = nullptr;

  // TargetObjectWriter wranppers.
  bool is64Bit() const { return TargetObjectWriter->is64Bit(); }
  bool isEmscripten() const { return TargetObjectWriter->isEmscripten(); }

  void startSection(SectionBookkeeping &Section, unsigned SectionId);
  void startCustomSection(SectionBookkeeping &Section, StringRef Name);
  void endSection(SectionBookkeeping &Section);

public:
  MidenObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                   raw_pwrite_stream &OS_)
      : TargetObjectWriter(std::move(MOTW)), OS(&OS_) {}

  MidenObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                   raw_pwrite_stream &OS_, raw_pwrite_stream &DwoOS_)
      : TargetObjectWriter(std::move(MOTW)), IsSplitDwarf(true), OS(&OS_),
        DwoOS(&DwoOS_) {}

private:
  void reset() override {
    CodeRelocations.clear();
    DataRelocations.clear();
    TypeIndices.clear();
    MidenIndices.clear();
    GOTIndices.clear();
    TableIndices.clear();
    DataLocations.clear();
    CustomSections.clear();
    ProducersSection.reset();
    TargetFeaturesSection.reset();
    CustomSectionsRelocations.clear();
    SignatureIndices.clear();
    Signatures.clear();
    DataSegments.clear();
    SectionFunctions.clear();
    NumFunctionImports = 0;
    NumGlobalImports = 0;
    NumTableImports = 0;
    MCObjectWriter::reset();
  }

  void writeHeader(const MCAssembler &Asm);

  void recordRelocation(MCAssembler &Asm, const MCAsmLayout &Layout,
                        const MCFragment *Fragment, const MCFixup &Fixup,
                        MCValue Target, uint64_t &FixedValue) override;

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;
  void prepareImports(SmallVectorImpl<miden::MidenImport> &Imports,
                      MCAssembler &Asm, const MCAsmLayout &Layout);
  uint64_t writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;

  uint64_t writeOneObject(MCAssembler &Asm, const MCAsmLayout &Layout,
                          DwoMode Mode);

  void writeString(const StringRef Str) {
    encodeULEB128(Str.size(), W->OS);
    W->OS << Str;
  }

  void writeStringWithAlignment(const StringRef Str, unsigned Alignment);

  void writeI32(int32_t val) {
    char Buffer[4];
    support::endian::write32le(Buffer, val);
    W->OS.write(Buffer, sizeof(Buffer));
  }

  void writeI64(int64_t val) {
    char Buffer[8];
    support::endian::write64le(Buffer, val);
    W->OS.write(Buffer, sizeof(Buffer));
  }

  void writeValueType(miden::ValType Ty) { W->OS << static_cast<char>(Ty); }

  void writeTypeSection(ArrayRef<miden::MidenSignature> Signatures);
  void writeImportSection(ArrayRef<miden::MidenImport> Imports, uint64_t DataSize,
                          uint32_t NumElements);
  void writeFunctionSection(ArrayRef<MidenFunction> Functions);
  void writeExportSection(ArrayRef<miden::MidenExport> Exports);
  void writeElemSection(const MCSymbolMiden *IndirectFunctionTable,
                        ArrayRef<uint32_t> TableElems);
  void writeDataCountSection();
  uint32_t writeCodeSection(const MCAssembler &Asm, const MCAsmLayout &Layout,
                            ArrayRef<MidenFunction> Functions);
  uint32_t writeDataSection(const MCAsmLayout &Layout);
  void writeTagSection(ArrayRef<uint32_t> TagTypes);
  void writeGlobalSection(ArrayRef<miden::MidenGlobal> Globals);
  void writeTableSection(ArrayRef<miden::MidenTable> Tables);
  void writeRelocSection(uint32_t SectionIndex, StringRef Name,
                         std::vector<MidenRelocationEntry> &Relocations);
  void writeLinkingMetaDataSection(
      ArrayRef<miden::MidenSymbolInfo> SymbolInfos,
      ArrayRef<std::pair<uint16_t, uint32_t>> InitFuncs,
      const std::map<StringRef, std::vector<MidenComdatEntry>> &Comdats);
  void writeCustomSection(MidenCustomSection &CustomSection,
                          const MCAssembler &Asm, const MCAsmLayout &Layout);
  void writeCustomRelocSections();

  uint64_t getProvisionalValue(const MidenRelocationEntry &RelEntry,
                               const MCAsmLayout &Layout);
  void applyRelocations(ArrayRef<MidenRelocationEntry> Relocations,
                        uint64_t ContentsOffset, const MCAsmLayout &Layout);

  uint32_t getRelocationIndexValue(const MidenRelocationEntry &RelEntry);
  uint32_t getFunctionType(const MCSymbolMiden &Symbol);
  uint32_t getTagType(const MCSymbolMiden &Symbol);
  void registerFunctionType(const MCSymbolMiden &Symbol);
  void registerTagType(const MCSymbolMiden &Symbol);
};

} // end anonymous namespace

// Write out a section header and a patchable section size field.
void MidenObjectWriter::startSection(SectionBookkeeping &Section,
                                    unsigned SectionId) {
  LLVM_DEBUG(dbgs() << "startSection " << SectionId << "\n");
  W->OS << char(SectionId);

  Section.SizeOffset = W->OS.tell();

  // The section size. We don't know the size yet, so reserve enough space
  // for any 32-bit value; we'll patch it later.
  encodeULEB128(0, W->OS, 5);

  // The position where the section starts, for measuring its size.
  Section.ContentsOffset = W->OS.tell();
  Section.PayloadOffset = W->OS.tell();
  Section.Index = SectionCount++;
}

// Write a string with extra paddings for trailing alignment
// TODO: support alignment at asm and llvm level?
void MidenObjectWriter::writeStringWithAlignment(const StringRef Str,
                                                unsigned Alignment) {

  // Calculate the encoded size of str length and add pads based on it and
  // alignment.
  raw_null_ostream NullOS;
  uint64_t StrSizeLength = encodeULEB128(Str.size(), NullOS);
  uint64_t Offset = W->OS.tell() + StrSizeLength + Str.size();
  uint64_t Paddings = offsetToAlignment(Offset, Align(Alignment));
  Offset += Paddings;

  // LEB128 greater than 5 bytes is invalid
  assert((StrSizeLength + Paddings) <= 5 && "too long string to align");

  encodeSLEB128(Str.size(), W->OS, StrSizeLength + Paddings);
  W->OS << Str;

  assert(W->OS.tell() == Offset && "invalid padding");
}

void MidenObjectWriter::startCustomSection(SectionBookkeeping &Section,
                                          StringRef Name) {
  LLVM_DEBUG(dbgs() << "startCustomSection " << Name << "\n");
  startSection(Section, miden::MIDEN_SEC_CUSTOM);

  // The position where the section header ends, for measuring its size.
  Section.PayloadOffset = W->OS.tell();

  // Custom sections in miden also have a string identifier.
  if (Name != "__clangast") {
    writeString(Name);
  } else {
    // The on-disk hashtable in clangast needs to be aligned by 4 bytes.
    writeStringWithAlignment(Name, 4);
  }

  // The position where the custom section starts.
  Section.ContentsOffset = W->OS.tell();
}

// Now that the section is complete and we know how big it is, patch up the
// section size field at the start of the section.
void MidenObjectWriter::endSection(SectionBookkeeping &Section) {
  uint64_t Size = W->OS.tell();
  // /dev/null doesn't support seek/tell and can report offset of 0.
  // Simply skip this patching in that case.
  if (!Size)
    return;

  Size -= Section.PayloadOffset;
  if (uint32_t(Size) != Size)
    report_fatal_error("section size does not fit in a uint32_t");

  LLVM_DEBUG(dbgs() << "endSection size=" << Size << "\n");

  // Write the final section size to the payload_len field, which follows
  // the section id byte.
  writePatchableU32(static_cast<raw_pwrite_stream &>(W->OS), Size,
                    Section.SizeOffset);
}

// Emit the Miden header.
void MidenObjectWriter::writeHeader(const MCAssembler &Asm) {
  W->OS.write(miden::MidenMagic, sizeof(miden::MidenMagic));
  W->write<uint32_t>(miden::MidenVersion);
}

void MidenObjectWriter::executePostLayoutBinding(MCAssembler &Asm,
                                                const MCAsmLayout &Layout) {
  // Some compilation units require the indirect function table to be present
  // but don't explicitly reference it.  This is the case for call_indirect
  // without the reference-types feature, and also function bitcasts in all
  // cases.  In those cases the __indirect_function_table has the
  // MIDEN_SYMBOL_NO_STRIP attribute.  Here we make sure this symbol makes it to
  // the assembler, if needed.
  if (auto *Sym = Asm.getContext().lookupSymbol("__indirect_function_table")) {
    const auto *MidenSym = static_cast<const MCSymbolMiden *>(Sym);
    if (MidenSym->isNoStrip())
      Asm.registerSymbol(*Sym);
  }

  // Build a map of sections to the function that defines them, for use
  // in recordRelocation.
  for (const MCSymbol &S : Asm.symbols()) {
    const auto &WS = static_cast<const MCSymbolMiden &>(S);
    if (WS.isDefined() && WS.isFunction() && !WS.isVariable()) {
      const auto &Sec = static_cast<const MCSectionMiden &>(S.getSection());
      auto Pair = SectionFunctions.insert(std::make_pair(&Sec, &S));
      if (!Pair.second)
        report_fatal_error("section already has a defining function: " +
                           Sec.getName());
    }
  }
}

void MidenObjectWriter::recordRelocation(MCAssembler &Asm,
                                        const MCAsmLayout &Layout,
                                        const MCFragment *Fragment,
                                        const MCFixup &Fixup, MCValue Target,
                                        uint64_t &FixedValue) {
  // The Miden backend should never generate FKF_IsPCRel fixups
  assert(!(Asm.getBackend().getFixupKindInfo(Fixup.getKind()).Flags &
           MCFixupKindInfo::FKF_IsPCRel));

  const auto &FixupSection = cast<MCSectionMiden>(*Fragment->getParent());
  uint64_t C = Target.getConstant();
  uint64_t FixupOffset = Layout.getFragmentOffset(Fragment) + Fixup.getOffset();
  MCContext &Ctx = Asm.getContext();
  bool IsLocRel = false;

  if (const MCSymbolRefExpr *RefB = Target.getSymB()) {

    const auto &SymB = cast<MCSymbolMiden>(RefB->getSymbol());

    if (FixupSection.getKind().isText()) {
      Ctx.reportError(Fixup.getLoc(),
                      Twine("symbol '") + SymB.getName() +
                          "' unsupported subtraction expression used in "
                          "relocation in code section.");
      return;
    }

    if (SymB.isUndefined()) {
      Ctx.reportError(Fixup.getLoc(),
                      Twine("symbol '") + SymB.getName() +
                          "' can not be undefined in a subtraction expression");
      return;
    }
    const MCSection &SecB = SymB.getSection();
    if (&SecB != &FixupSection) {
      Ctx.reportError(Fixup.getLoc(),
                      Twine("symbol '") + SymB.getName() +
                          "' can not be placed in a different section");
      return;
    }
    IsLocRel = true;
    C += FixupOffset - Layout.getSymbolOffset(SymB);
  }

  // We either rejected the fixup or folded B into C at this point.
  const MCSymbolRefExpr *RefA = Target.getSymA();
  const auto *SymA = cast<MCSymbolMiden>(&RefA->getSymbol());

  // The .init_array isn't translated as data, so don't do relocations in it.
  if (FixupSection.getName().startswith(".init_array")) {
    SymA->setUsedInInitArray();
    return;
  }

  if (SymA->isVariable()) {
    const MCExpr *Expr = SymA->getVariableValue();
    if (const auto *Inner = dyn_cast<MCSymbolRefExpr>(Expr))
      if (Inner->getKind() == MCSymbolRefExpr::VK_WEAKREF)
        llvm_unreachable("weakref used in reloc not yet implemented");
  }

  // Put any constant offset in an addend. Offsets can be negative, and
  // LLVM expects wrapping, in contrast to miden's immediates which can't
  // be negative and don't wrap.
  FixedValue = 0;

  unsigned Type =
      TargetObjectWriter->getRelocType(Target, Fixup, FixupSection, IsLocRel);

  // Absolute offset within a section or a function.
  // Currently only supported for for metadata sections.
  // See: test/MC/Miden/blockaddress.ll
  if ((Type == miden::R_MIDEN_FUNCTION_OFFSET_I32 ||
       Type == miden::R_MIDEN_FUNCTION_OFFSET_I64 ||
       Type == miden::R_MIDEN_SECTION_OFFSET_I32) &&
      SymA->isDefined()) {
    // SymA can be a temp data symbol that represents a function (in which case
    // it needs to be replaced by the section symbol), [XXX and it apparently
    // later gets changed again to a func symbol?] or it can be a real
    // function symbol, in which case it can be left as-is.

    if (!FixupSection.getKind().isMetadata())
      report_fatal_error("relocations for function or section offsets are "
                         "only supported in metadata sections");

    const MCSymbol *SectionSymbol = nullptr;
    const MCSection &SecA = SymA->getSection();
    if (SecA.getKind().isText()) {
      auto SecSymIt = SectionFunctions.find(&SecA);
      if (SecSymIt == SectionFunctions.end())
        report_fatal_error("section doesn\'t have defining symbol");
      SectionSymbol = SecSymIt->second;
    } else {
      SectionSymbol = SecA.getBeginSymbol();
    }
    if (!SectionSymbol)
      report_fatal_error("section symbol is required for relocation");

    C += Layout.getSymbolOffset(*SymA);
    SymA = cast<MCSymbolMiden>(SectionSymbol);
  }

  if (Type == miden::R_MIDEN_TABLE_INDEX_REL_SLEB ||
      Type == miden::R_MIDEN_TABLE_INDEX_REL_SLEB64 ||
      Type == miden::R_MIDEN_TABLE_INDEX_SLEB ||
      Type == miden::R_MIDEN_TABLE_INDEX_SLEB64 ||
      Type == miden::R_MIDEN_TABLE_INDEX_I32 ||
      Type == miden::R_MIDEN_TABLE_INDEX_I64) {
    // TABLE_INDEX relocs implicitly use the default indirect function table.
    // We require the function table to have already been defined.
    auto TableName = "__indirect_function_table";
    MCSymbolMiden *Sym = cast_or_null<MCSymbolMiden>(Ctx.lookupSymbol(TableName));
    if (!Sym) {
      report_fatal_error("missing indirect function table symbol");
    } else {
      if (!Sym->isFunctionTable())
        report_fatal_error("__indirect_function_table symbol has wrong type");
      // Ensure that __indirect_function_table reaches the output.
      Sym->setNoStrip();
      Asm.registerSymbol(*Sym);
    }
  }

  // Relocation other than R_MIDEN_TYPE_INDEX_LEB are required to be
  // against a named symbol.
  if (Type != miden::R_MIDEN_TYPE_INDEX_LEB) {
    if (SymA->getName().empty())
      report_fatal_error("relocations against un-named temporaries are not yet "
                         "supported by miden");

    SymA->setUsedInReloc();
  }

  switch (RefA->getKind()) {
  case MCSymbolRefExpr::VK_GOT:
  case MCSymbolRefExpr::VK_MIDEN_GOT_TLS:
    SymA->setUsedInGOT();
    break;
  default:
    break;
  }

  MidenRelocationEntry Rec(FixupOffset, SymA, C, Type, &FixupSection);
  LLVM_DEBUG(dbgs() << "MidenReloc: " << Rec << "\n");

  if (FixupSection.isMidenData()) {
    DataRelocations.push_back(Rec);
  } else if (FixupSection.getKind().isText()) {
    CodeRelocations.push_back(Rec);
  } else if (FixupSection.getKind().isMetadata()) {
    CustomSectionsRelocations[&FixupSection].push_back(Rec);
  } else {
    llvm_unreachable("unexpected section type");
  }
}

// Compute a value to write into the code at the location covered
// by RelEntry. This value isn't used by the static linker; it just serves
// to make the object format more readable and more likely to be directly
// useable.
uint64_t
MidenObjectWriter::getProvisionalValue(const MidenRelocationEntry &RelEntry,
                                      const MCAsmLayout &Layout) {
  if ((RelEntry.Type == miden::R_MIDEN_GLOBAL_INDEX_LEB ||
       RelEntry.Type == miden::R_MIDEN_GLOBAL_INDEX_I32) &&
      !RelEntry.Symbol->isGlobal()) {
    assert(GOTIndices.count(RelEntry.Symbol) > 0 && "symbol not found in GOT index space");
    return GOTIndices[RelEntry.Symbol];
  }

  switch (RelEntry.Type) {
  case miden::R_MIDEN_TABLE_INDEX_REL_SLEB:
  case miden::R_MIDEN_TABLE_INDEX_REL_SLEB64:
  case miden::R_MIDEN_TABLE_INDEX_SLEB:
  case miden::R_MIDEN_TABLE_INDEX_SLEB64:
  case miden::R_MIDEN_TABLE_INDEX_I32:
  case miden::R_MIDEN_TABLE_INDEX_I64: {
    // Provisional value is table address of the resolved symbol itself
    const MCSymbolMiden *Base =
        cast<MCSymbolMiden>(Layout.getBaseSymbol(*RelEntry.Symbol));
    assert(Base->isFunction());
    if (RelEntry.Type == miden::R_MIDEN_TABLE_INDEX_REL_SLEB ||
        RelEntry.Type == miden::R_MIDEN_TABLE_INDEX_REL_SLEB64)
      return TableIndices[Base] - InitialTableOffset;
    else
      return TableIndices[Base];
  }
  case miden::R_MIDEN_TYPE_INDEX_LEB:
    // Provisional value is same as the index
    return getRelocationIndexValue(RelEntry);
  case miden::R_MIDEN_FUNCTION_INDEX_LEB:
  case miden::R_MIDEN_GLOBAL_INDEX_LEB:
  case miden::R_MIDEN_GLOBAL_INDEX_I32:
  case miden::R_MIDEN_TAG_INDEX_LEB:
  case miden::R_MIDEN_TABLE_NUMBER_LEB:
    // Provisional value is function/global/tag Miden index
    assert(MidenIndices.count(RelEntry.Symbol) > 0 && "symbol not found in miden index space");
    return MidenIndices[RelEntry.Symbol];
  case miden::R_MIDEN_FUNCTION_OFFSET_I32:
  case miden::R_MIDEN_FUNCTION_OFFSET_I64:
  case miden::R_MIDEN_SECTION_OFFSET_I32: {
    if (!RelEntry.Symbol->isDefined())
      return 0;
    const auto &Section =
        static_cast<const MCSectionMiden &>(RelEntry.Symbol->getSection());
    return Section.getSectionOffset() + RelEntry.Addend;
  }
  case miden::R_MIDEN_MEMORY_ADDR_LEB:
  case miden::R_MIDEN_MEMORY_ADDR_LEB64:
  case miden::R_MIDEN_MEMORY_ADDR_SLEB:
  case miden::R_MIDEN_MEMORY_ADDR_SLEB64:
  case miden::R_MIDEN_MEMORY_ADDR_REL_SLEB:
  case miden::R_MIDEN_MEMORY_ADDR_REL_SLEB64:
  case miden::R_MIDEN_MEMORY_ADDR_I32:
  case miden::R_MIDEN_MEMORY_ADDR_I64:
  case miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB:
  case miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB64:
  case miden::R_MIDEN_MEMORY_ADDR_LOCREL_I32: {
    // Provisional value is address of the global plus the offset
    // For undefined symbols, use zero
    if (!RelEntry.Symbol->isDefined())
      return 0;
    const miden::MidenDataReference &SymRef = DataLocations[RelEntry.Symbol];
    const MidenDataSegment &Segment = DataSegments[SymRef.Segment];
    // Ignore overflow. LLVM allows address arithmetic to silently wrap.
    return Segment.Offset + SymRef.Offset + RelEntry.Addend;
  }
  default:
    llvm_unreachable("invalid relocation type");
  }
}

static void addData(SmallVectorImpl<char> &DataBytes,
                    MCSectionMiden &DataSection) {
  LLVM_DEBUG(errs() << "addData: " << DataSection.getName() << "\n");

  DataBytes.resize(alignTo(DataBytes.size(), DataSection.getAlignment()));

  for (const MCFragment &Frag : DataSection) {
    if (Frag.hasInstructions())
      report_fatal_error("only data supported in data sections");

    if (auto *Align = dyn_cast<MCAlignFragment>(&Frag)) {
      if (Align->getValueSize() != 1)
        report_fatal_error("only byte values supported for alignment");
      // If nops are requested, use zeros, as this is the data section.
      uint8_t Value = Align->hasEmitNops() ? 0 : Align->getValue();
      uint64_t Size =
          std::min<uint64_t>(alignTo(DataBytes.size(), Align->getAlignment()),
                             DataBytes.size() + Align->getMaxBytesToEmit());
      DataBytes.resize(Size, Value);
    } else if (auto *Fill = dyn_cast<MCFillFragment>(&Frag)) {
      int64_t NumValues;
      if (!Fill->getNumValues().evaluateAsAbsolute(NumValues))
        llvm_unreachable("The fill should be an assembler constant");
      DataBytes.insert(DataBytes.end(), Fill->getValueSize() * NumValues,
                       Fill->getValue());
    } else if (auto *LEB = dyn_cast<MCLEBFragment>(&Frag)) {
      const SmallVectorImpl<char> &Contents = LEB->getContents();
      llvm::append_range(DataBytes, Contents);
    } else {
      const auto &DataFrag = cast<MCDataFragment>(Frag);
      const SmallVectorImpl<char> &Contents = DataFrag.getContents();
      llvm::append_range(DataBytes, Contents);
    }
  }

  LLVM_DEBUG(dbgs() << "addData -> " << DataBytes.size() << "\n");
}

uint32_t
MidenObjectWriter::getRelocationIndexValue(const MidenRelocationEntry &RelEntry) {
  if (RelEntry.Type == miden::R_MIDEN_TYPE_INDEX_LEB) {
    if (!TypeIndices.count(RelEntry.Symbol))
      report_fatal_error("symbol not found in type index space: " +
                         RelEntry.Symbol->getName());
    return TypeIndices[RelEntry.Symbol];
  }

  return RelEntry.Symbol->getIndex();
}

// Apply the portions of the relocation records that we can handle ourselves
// directly.
void MidenObjectWriter::applyRelocations(
    ArrayRef<MidenRelocationEntry> Relocations, uint64_t ContentsOffset,
    const MCAsmLayout &Layout) {
  auto &Stream = static_cast<raw_pwrite_stream &>(W->OS);
  for (const MidenRelocationEntry &RelEntry : Relocations) {
    uint64_t Offset = ContentsOffset +
                      RelEntry.FixupSection->getSectionOffset() +
                      RelEntry.Offset;

    LLVM_DEBUG(dbgs() << "applyRelocation: " << RelEntry << "\n");
    uint64_t Value = getProvisionalValue(RelEntry, Layout);

    switch (RelEntry.Type) {
    case miden::R_MIDEN_FUNCTION_INDEX_LEB:
    case miden::R_MIDEN_TYPE_INDEX_LEB:
    case miden::R_MIDEN_GLOBAL_INDEX_LEB:
    case miden::R_MIDEN_MEMORY_ADDR_LEB:
    case miden::R_MIDEN_TAG_INDEX_LEB:
    case miden::R_MIDEN_TABLE_NUMBER_LEB:
      writePatchableU32(Stream, Value, Offset);
      break;
    case miden::R_MIDEN_MEMORY_ADDR_LEB64:
      writePatchableU64(Stream, Value, Offset);
      break;
    case miden::R_MIDEN_TABLE_INDEX_I32:
    case miden::R_MIDEN_MEMORY_ADDR_I32:
    case miden::R_MIDEN_FUNCTION_OFFSET_I32:
    case miden::R_MIDEN_SECTION_OFFSET_I32:
    case miden::R_MIDEN_GLOBAL_INDEX_I32:
    case miden::R_MIDEN_MEMORY_ADDR_LOCREL_I32:
      patchI32(Stream, Value, Offset);
      break;
    case miden::R_MIDEN_TABLE_INDEX_I64:
    case miden::R_MIDEN_MEMORY_ADDR_I64:
    case miden::R_MIDEN_FUNCTION_OFFSET_I64:
      patchI64(Stream, Value, Offset);
      break;
    case miden::R_MIDEN_TABLE_INDEX_SLEB:
    case miden::R_MIDEN_TABLE_INDEX_REL_SLEB:
    case miden::R_MIDEN_MEMORY_ADDR_SLEB:
    case miden::R_MIDEN_MEMORY_ADDR_REL_SLEB:
    case miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB:
      writePatchableS32(Stream, Value, Offset);
      break;
    case miden::R_MIDEN_TABLE_INDEX_SLEB64:
    case miden::R_MIDEN_TABLE_INDEX_REL_SLEB64:
    case miden::R_MIDEN_MEMORY_ADDR_SLEB64:
    case miden::R_MIDEN_MEMORY_ADDR_REL_SLEB64:
    case miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB64:
      writePatchableS64(Stream, Value, Offset);
      break;
    default:
      llvm_unreachable("invalid relocation type");
    }
  }
}

void MidenObjectWriter::writeTypeSection(
    ArrayRef<miden::MidenSignature> Signatures) {
  if (Signatures.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_TYPE);

  encodeULEB128(Signatures.size(), W->OS);

  for (const miden::MidenSignature &Sig : Signatures) {
    W->OS << char(miden::MIDEN_TYPE_FUNC);
    encodeULEB128(Sig.Params.size(), W->OS);
    for (miden::ValType Ty : Sig.Params)
      writeValueType(Ty);
    encodeULEB128(Sig.Returns.size(), W->OS);
    for (miden::ValType Ty : Sig.Returns)
      writeValueType(Ty);
  }

  endSection(Section);
}

void MidenObjectWriter::writeImportSection(ArrayRef<miden::MidenImport> Imports,
                                          uint64_t DataSize,
                                          uint32_t NumElements) {
  if (Imports.empty())
    return;

  uint64_t NumPages = (DataSize + miden::MidenPageSize - 1) / miden::MidenPageSize;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_IMPORT);

  encodeULEB128(Imports.size(), W->OS);
  for (const miden::MidenImport &Import : Imports) {
    writeString(Import.Module);
    writeString(Import.Field);
    W->OS << char(Import.Kind);

    switch (Import.Kind) {
    case miden::MIDEN_EXTERNAL_FUNCTION:
      encodeULEB128(Import.SigIndex, W->OS);
      break;
    case miden::MIDEN_EXTERNAL_GLOBAL:
      W->OS << char(Import.Global.Type);
      W->OS << char(Import.Global.Mutable ? 1 : 0);
      break;
    case miden::MIDEN_EXTERNAL_MEMORY:
      encodeULEB128(Import.Memory.Flags, W->OS);
      encodeULEB128(NumPages, W->OS); // initial
      break;
    case miden::MIDEN_EXTERNAL_TABLE:
      W->OS << char(Import.Table.ElemType);
      encodeULEB128(0, W->OS);           // flags
      encodeULEB128(NumElements, W->OS); // initial
      break;
    case miden::MIDEN_EXTERNAL_TAG:
      W->OS << char(0); // Reserved 'attribute' field
      encodeULEB128(Import.SigIndex, W->OS);
      break;
    default:
      llvm_unreachable("unsupported import kind");
    }
  }

  endSection(Section);
}

void MidenObjectWriter::writeFunctionSection(ArrayRef<MidenFunction> Functions) {
  if (Functions.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_FUNCTION);

  encodeULEB128(Functions.size(), W->OS);
  for (const MidenFunction &Func : Functions)
    encodeULEB128(Func.SigIndex, W->OS);

  endSection(Section);
}

void MidenObjectWriter::writeTagSection(ArrayRef<uint32_t> TagTypes) {
  if (TagTypes.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_TAG);

  encodeULEB128(TagTypes.size(), W->OS);
  for (uint32_t Index : TagTypes) {
    W->OS << char(0); // Reserved 'attribute' field
    encodeULEB128(Index, W->OS);
  }

  endSection(Section);
}

void MidenObjectWriter::writeGlobalSection(ArrayRef<miden::MidenGlobal> Globals) {
  if (Globals.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_GLOBAL);

  encodeULEB128(Globals.size(), W->OS);
  for (const miden::MidenGlobal &Global : Globals) {
    encodeULEB128(Global.Type.Type, W->OS);
    W->OS << char(Global.Type.Mutable);
    if (Global.InitExpr.Extended) {
      llvm_unreachable("extected init expressions not supported");
    } else {
      W->OS << char(Global.InitExpr.Inst.Opcode);
      switch (Global.Type.Type) {
      case miden::MIDEN_TYPE_I32:
        encodeSLEB128(0, W->OS);
        break;
      case miden::MIDEN_TYPE_I64:
        encodeSLEB128(0, W->OS);
        break;
      case miden::MIDEN_TYPE_F32:
        writeI32(0);
        break;
      case miden::MIDEN_TYPE_F64:
        writeI64(0);
        break;
      case miden::MIDEN_TYPE_EXTERNREF:
        writeValueType(miden::ValType::EXTERNREF);
        break;
      default:
        llvm_unreachable("unexpected type");
      }
    }
    W->OS << char(miden::MIDEN_OPCODE_END);
  }

  endSection(Section);
}

void MidenObjectWriter::writeTableSection(ArrayRef<miden::MidenTable> Tables) {
  if (Tables.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_TABLE);

  encodeULEB128(Tables.size(), W->OS);
  for (const miden::MidenTable &Table : Tables) {
    encodeULEB128(Table.Type.ElemType, W->OS);
    encodeULEB128(Table.Type.Limits.Flags, W->OS);
    encodeULEB128(Table.Type.Limits.Minimum, W->OS);
    if (Table.Type.Limits.Flags & miden::MIDEN_LIMITS_FLAG_HAS_MAX)
      encodeULEB128(Table.Type.Limits.Maximum, W->OS);
  }
  endSection(Section);
}

void MidenObjectWriter::writeExportSection(ArrayRef<miden::MidenExport> Exports) {
  if (Exports.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_EXPORT);

  encodeULEB128(Exports.size(), W->OS);
  for (const miden::MidenExport &Export : Exports) {
    writeString(Export.Name);
    W->OS << char(Export.Kind);
    encodeULEB128(Export.Index, W->OS);
  }

  endSection(Section);
}

void MidenObjectWriter::writeElemSection(
    const MCSymbolMiden *IndirectFunctionTable, ArrayRef<uint32_t> TableElems) {
  if (TableElems.empty())
    return;

  assert(IndirectFunctionTable);

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_ELEM);

  encodeULEB128(1, W->OS); // number of "segments"

  assert(MidenIndices.count(IndirectFunctionTable));
  uint32_t TableNumber = MidenIndices.find(IndirectFunctionTable)->second;
  uint32_t Flags = 0;
  if (TableNumber)
    Flags |= miden::MIDEN_ELEM_SEGMENT_HAS_TABLE_NUMBER;
  encodeULEB128(Flags, W->OS);
  if (Flags & miden::MIDEN_ELEM_SEGMENT_HAS_TABLE_NUMBER)
    encodeULEB128(TableNumber, W->OS); // the table number

  // init expr for starting offset
  W->OS << char(miden::MIDEN_OPCODE_I32_CONST);
  encodeSLEB128(InitialTableOffset, W->OS);
  W->OS << char(miden::MIDEN_OPCODE_END);

  if (Flags & miden::MIDEN_ELEM_SEGMENT_MASK_HAS_ELEM_KIND) {
    // We only write active function table initializers, for which the elem kind
    // is specified to be written as 0x00 and interpreted to mean "funcref".
    const uint8_t ElemKind = 0;
    W->OS << ElemKind;
  }

  encodeULEB128(TableElems.size(), W->OS);
  for (uint32_t Elem : TableElems)
    encodeULEB128(Elem, W->OS);

  endSection(Section);
}

void MidenObjectWriter::writeDataCountSection() {
  if (DataSegments.empty())
    return;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_DATACOUNT);
  encodeULEB128(DataSegments.size(), W->OS);
  endSection(Section);
}

uint32_t MidenObjectWriter::writeCodeSection(const MCAssembler &Asm,
                                            const MCAsmLayout &Layout,
                                            ArrayRef<MidenFunction> Functions) {
  if (Functions.empty())
    return 0;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_CODE);

  encodeULEB128(Functions.size(), W->OS);

  for (const MidenFunction &Func : Functions) {
    auto &FuncSection = static_cast<MCSectionMiden &>(Func.Sym->getSection());

    int64_t Size = 0;
    if (!Func.Sym->getSize()->evaluateAsAbsolute(Size, Layout))
      report_fatal_error(".size expression must be evaluatable");

    encodeULEB128(Size, W->OS);
    FuncSection.setSectionOffset(W->OS.tell() - Section.ContentsOffset);
    Asm.writeSectionData(W->OS, &FuncSection, Layout);
  }

  // Apply fixups.
  applyRelocations(CodeRelocations, Section.ContentsOffset, Layout);

  endSection(Section);
  return Section.Index;
}

uint32_t MidenObjectWriter::writeDataSection(const MCAsmLayout &Layout) {
  if (DataSegments.empty())
    return 0;

  SectionBookkeeping Section;
  startSection(Section, miden::MIDEN_SEC_DATA);

  encodeULEB128(DataSegments.size(), W->OS); // count

  for (const MidenDataSegment &Segment : DataSegments) {
    encodeULEB128(Segment.InitFlags, W->OS); // flags
    if (Segment.InitFlags & miden::MIDEN_DATA_SEGMENT_HAS_MEMINDEX)
      encodeULEB128(0, W->OS); // memory index
    if ((Segment.InitFlags & miden::MIDEN_DATA_SEGMENT_IS_PASSIVE) == 0) {
      W->OS << char(is64Bit() ? miden::MIDEN_OPCODE_I64_CONST
                              : miden::MIDEN_OPCODE_I32_CONST);
      encodeSLEB128(Segment.Offset, W->OS); // offset
      W->OS << char(miden::MIDEN_OPCODE_END);
    }
    encodeULEB128(Segment.Data.size(), W->OS); // size
    Segment.Section->setSectionOffset(W->OS.tell() - Section.ContentsOffset);
    W->OS << Segment.Data; // data
  }

  // Apply fixups.
  applyRelocations(DataRelocations, Section.ContentsOffset, Layout);

  endSection(Section);
  return Section.Index;
}

void MidenObjectWriter::writeRelocSection(
    uint32_t SectionIndex, StringRef Name,
    std::vector<MidenRelocationEntry> &Relocs) {
  // See: https://github.com/Miden/tool-conventions/blob/main/Linking.md
  // for descriptions of the reloc sections.

  if (Relocs.empty())
    return;

  // First, ensure the relocations are sorted in offset order.  In general they
  // should already be sorted since `recordRelocation` is called in offset
  // order, but for the code section we combine many MC sections into single
  // miden section, and this order is determined by the order of Asm.Symbols()
  // not the sections order.
  llvm::stable_sort(
      Relocs, [](const MidenRelocationEntry &A, const MidenRelocationEntry &B) {
        return (A.Offset + A.FixupSection->getSectionOffset()) <
               (B.Offset + B.FixupSection->getSectionOffset());
      });

  SectionBookkeeping Section;
  startCustomSection(Section, std::string("reloc.") + Name.str());

  encodeULEB128(SectionIndex, W->OS);
  encodeULEB128(Relocs.size(), W->OS);
  for (const MidenRelocationEntry &RelEntry : Relocs) {
    uint64_t Offset =
        RelEntry.Offset + RelEntry.FixupSection->getSectionOffset();
    uint32_t Index = getRelocationIndexValue(RelEntry);

    W->OS << char(RelEntry.Type);
    encodeULEB128(Offset, W->OS);
    encodeULEB128(Index, W->OS);
    if (RelEntry.hasAddend())
      encodeSLEB128(RelEntry.Addend, W->OS);
  }

  endSection(Section);
}

void MidenObjectWriter::writeCustomRelocSections() {
  for (const auto &Sec : CustomSections) {
    auto &Relocations = CustomSectionsRelocations[Sec.Section];
    writeRelocSection(Sec.OutputIndex, Sec.Name, Relocations);
  }
}

void MidenObjectWriter::writeLinkingMetaDataSection(
    ArrayRef<miden::MidenSymbolInfo> SymbolInfos,
    ArrayRef<std::pair<uint16_t, uint32_t>> InitFuncs,
    const std::map<StringRef, std::vector<MidenComdatEntry>> &Comdats) {
  SectionBookkeeping Section;
  startCustomSection(Section, "linking");
  encodeULEB128(miden::MidenMetadataVersion, W->OS);

  SectionBookkeeping SubSection;
  if (SymbolInfos.size() != 0) {
    startSection(SubSection, miden::MIDEN_SYMBOL_TABLE);
    encodeULEB128(SymbolInfos.size(), W->OS);
    for (const miden::MidenSymbolInfo &Sym : SymbolInfos) {
      encodeULEB128(Sym.Kind, W->OS);
      encodeULEB128(Sym.Flags, W->OS);
      switch (Sym.Kind) {
      case miden::MIDEN_SYMBOL_TYPE_FUNCTION:
      case miden::MIDEN_SYMBOL_TYPE_GLOBAL:
      case miden::MIDEN_SYMBOL_TYPE_TAG:
      case miden::MIDEN_SYMBOL_TYPE_TABLE:
        encodeULEB128(Sym.ElementIndex, W->OS);
        if ((Sym.Flags & miden::MIDEN_SYMBOL_UNDEFINED) == 0 ||
            (Sym.Flags & miden::MIDEN_SYMBOL_EXPLICIT_NAME) != 0)
          writeString(Sym.Name);
        break;
      case miden::MIDEN_SYMBOL_TYPE_DATA:
        writeString(Sym.Name);
        if ((Sym.Flags & miden::MIDEN_SYMBOL_UNDEFINED) == 0) {
          encodeULEB128(Sym.DataRef.Segment, W->OS);
          encodeULEB128(Sym.DataRef.Offset, W->OS);
          encodeULEB128(Sym.DataRef.Size, W->OS);
        }
        break;
      case miden::MIDEN_SYMBOL_TYPE_SECTION: {
        const uint32_t SectionIndex =
            CustomSections[Sym.ElementIndex].OutputIndex;
        encodeULEB128(SectionIndex, W->OS);
        break;
      }
      default:
        llvm_unreachable("unexpected kind");
      }
    }
    endSection(SubSection);
  }

  if (DataSegments.size()) {
    startSection(SubSection, miden::MIDEN_SEGMENT_INFO);
    encodeULEB128(DataSegments.size(), W->OS);
    for (const MidenDataSegment &Segment : DataSegments) {
      writeString(Segment.Name);
      encodeULEB128(Segment.Alignment, W->OS);
      encodeULEB128(Segment.LinkingFlags, W->OS);
    }
    endSection(SubSection);
  }

  if (!InitFuncs.empty()) {
    startSection(SubSection, miden::MIDEN_INIT_FUNCS);
    encodeULEB128(InitFuncs.size(), W->OS);
    for (auto &StartFunc : InitFuncs) {
      encodeULEB128(StartFunc.first, W->OS);  // priority
      encodeULEB128(StartFunc.second, W->OS); // function index
    }
    endSection(SubSection);
  }

  if (Comdats.size()) {
    startSection(SubSection, miden::MIDEN_COMDAT_INFO);
    encodeULEB128(Comdats.size(), W->OS);
    for (const auto &C : Comdats) {
      writeString(C.first);
      encodeULEB128(0, W->OS); // flags for future use
      encodeULEB128(C.second.size(), W->OS);
      for (const MidenComdatEntry &Entry : C.second) {
        encodeULEB128(Entry.Kind, W->OS);
        encodeULEB128(Entry.Index, W->OS);
      }
    }
    endSection(SubSection);
  }

  endSection(Section);
}

void MidenObjectWriter::writeCustomSection(MidenCustomSection &CustomSection,
                                          const MCAssembler &Asm,
                                          const MCAsmLayout &Layout) {
  SectionBookkeeping Section;
  auto *Sec = CustomSection.Section;
  startCustomSection(Section, CustomSection.Name);

  Sec->setSectionOffset(W->OS.tell() - Section.ContentsOffset);
  Asm.writeSectionData(W->OS, Sec, Layout);

  CustomSection.OutputContentsOffset = Section.ContentsOffset;
  CustomSection.OutputIndex = Section.Index;

  endSection(Section);

  // Apply fixups.
  auto &Relocations = CustomSectionsRelocations[CustomSection.Section];
  applyRelocations(Relocations, CustomSection.OutputContentsOffset, Layout);
}

uint32_t MidenObjectWriter::getFunctionType(const MCSymbolMiden &Symbol) {
  assert(Symbol.isFunction());
  assert(TypeIndices.count(&Symbol));
  return TypeIndices[&Symbol];
}

uint32_t MidenObjectWriter::getTagType(const MCSymbolMiden &Symbol) {
  assert(Symbol.isTag());
  assert(TypeIndices.count(&Symbol));
  return TypeIndices[&Symbol];
}

void MidenObjectWriter::registerFunctionType(const MCSymbolMiden &Symbol) {
  assert(Symbol.isFunction());

  miden::MidenSignature S;

  if (auto *Sig = Symbol.getSignature()) {
    S.Returns = Sig->Returns;
    S.Params = Sig->Params;
  }

  auto Pair = SignatureIndices.insert(std::make_pair(S, Signatures.size()));
  if (Pair.second)
    Signatures.push_back(S);
  TypeIndices[&Symbol] = Pair.first->second;

  LLVM_DEBUG(dbgs() << "registerFunctionType: " << Symbol
                    << " new:" << Pair.second << "\n");
  LLVM_DEBUG(dbgs() << "  -> type index: " << Pair.first->second << "\n");
}

void MidenObjectWriter::registerTagType(const MCSymbolMiden &Symbol) {
  assert(Symbol.isTag());

  // TODO Currently we don't generate imported exceptions, but if we do, we
  // should have a way of infering types of imported exceptions.
  miden::MidenSignature S;
  if (auto *Sig = Symbol.getSignature()) {
    S.Returns = Sig->Returns;
    S.Params = Sig->Params;
  }

  auto Pair = SignatureIndices.insert(std::make_pair(S, Signatures.size()));
  if (Pair.second)
    Signatures.push_back(S);
  TypeIndices[&Symbol] = Pair.first->second;

  LLVM_DEBUG(dbgs() << "registerTagType: " << Symbol << " new:" << Pair.second
                    << "\n");
  LLVM_DEBUG(dbgs() << "  -> type index: " << Pair.first->second << "\n");
}

static bool isInSymtab(const MCSymbolMiden &Sym) {
  if (Sym.isUsedInReloc() || Sym.isUsedInInitArray())
    return true;

  if (Sym.isComdat() && !Sym.isDefined())
    return false;

  if (Sym.isTemporary())
    return false;

  if (Sym.isSection())
    return false;

  if (Sym.omitFromLinkingSection())
    return false;

  return true;
}

void MidenObjectWriter::prepareImports(
    SmallVectorImpl<miden::MidenImport> &Imports, MCAssembler &Asm,
    const MCAsmLayout &Layout) {
  // For now, always emit the memory import, since loads and stores are not
  // valid without it. In the future, we could perhaps be more clever and omit
  // it if there are no loads or stores.
  miden::MidenImport MemImport;
  MemImport.Module = "env";
  MemImport.Field = "__linear_memory";
  MemImport.Kind = miden::MIDEN_EXTERNAL_MEMORY;
  MemImport.Memory.Flags = is64Bit() ? miden::MIDEN_LIMITS_FLAG_IS_64
                                     : miden::MIDEN_LIMITS_FLAG_NONE;
  Imports.push_back(MemImport);

  // Populate SignatureIndices, and Imports and MidenIndices for undefined
  // symbols.  This must be done before populating MidenIndices for defined
  // symbols.
  for (const MCSymbol &S : Asm.symbols()) {
    const auto &WS = static_cast<const MCSymbolMiden &>(S);

    // Register types for all functions, including those with private linkage
    // (because miden always needs a type signature).
    if (WS.isFunction()) {
      const auto *BS = Layout.getBaseSymbol(S);
      if (!BS)
        report_fatal_error(Twine(S.getName()) +
                           ": absolute addressing not supported!");
      registerFunctionType(*cast<MCSymbolMiden>(BS));
    }

    if (WS.isTag())
      registerTagType(WS);

    if (WS.isTemporary())
      continue;

    // If the symbol is not defined in this translation unit, import it.
    if (!WS.isDefined() && !WS.isComdat()) {
      if (WS.isFunction()) {
        miden::MidenImport Import;
        Import.Module = WS.getImportModule();
        Import.Field = WS.getImportName();
        Import.Kind = miden::MIDEN_EXTERNAL_FUNCTION;
        Import.SigIndex = getFunctionType(WS);
        Imports.push_back(Import);
        assert(MidenIndices.count(&WS) == 0);
        MidenIndices[&WS] = NumFunctionImports++;
      } else if (WS.isGlobal()) {
        if (WS.isWeak())
          report_fatal_error("undefined global symbol cannot be weak");

        miden::MidenImport Import;
        Import.Field = WS.getImportName();
        Import.Kind = miden::MIDEN_EXTERNAL_GLOBAL;
        Import.Module = WS.getImportModule();
        Import.Global = WS.getGlobalType();
        Imports.push_back(Import);
        assert(MidenIndices.count(&WS) == 0);
        MidenIndices[&WS] = NumGlobalImports++;
      } else if (WS.isTag()) {
        if (WS.isWeak())
          report_fatal_error("undefined tag symbol cannot be weak");

        miden::MidenImport Import;
        Import.Module = WS.getImportModule();
        Import.Field = WS.getImportName();
        Import.Kind = miden::MIDEN_EXTERNAL_TAG;
        Import.SigIndex = getTagType(WS);
        Imports.push_back(Import);
        assert(MidenIndices.count(&WS) == 0);
        MidenIndices[&WS] = NumTagImports++;
      } else if (WS.isTable()) {
        if (WS.isWeak())
          report_fatal_error("undefined table symbol cannot be weak");

        miden::MidenImport Import;
        Import.Module = WS.getImportModule();
        Import.Field = WS.getImportName();
        Import.Kind = miden::MIDEN_EXTERNAL_TABLE;
        Import.Table = WS.getTableType();
        Imports.push_back(Import);
        assert(MidenIndices.count(&WS) == 0);
        MidenIndices[&WS] = NumTableImports++;
      }
    }
  }

  // Add imports for GOT globals
  for (const MCSymbol &S : Asm.symbols()) {
    const auto &WS = static_cast<const MCSymbolMiden &>(S);
    if (WS.isUsedInGOT()) {
      miden::MidenImport Import;
      if (WS.isFunction())
        Import.Module = "GOT.func";
      else
        Import.Module = "GOT.mem";
      Import.Field = WS.getName();
      Import.Kind = miden::MIDEN_EXTERNAL_GLOBAL;
      Import.Global = {miden::MIDEN_TYPE_I32, true};
      Imports.push_back(Import);
      assert(GOTIndices.count(&WS) == 0);
      GOTIndices[&WS] = NumGlobalImports++;
    }
  }
}

uint64_t MidenObjectWriter::writeObject(MCAssembler &Asm,
                                       const MCAsmLayout &Layout) {
  support::endian::Writer MainWriter(*OS, support::little);
  W = &MainWriter;
  if (IsSplitDwarf) {
    uint64_t TotalSize = writeOneObject(Asm, Layout, DwoMode::NonDwoOnly);
    assert(DwoOS);
    support::endian::Writer DwoWriter(*DwoOS, support::little);
    W = &DwoWriter;
    return TotalSize + writeOneObject(Asm, Layout, DwoMode::DwoOnly);
  } else {
    return writeOneObject(Asm, Layout, DwoMode::AllSections);
  }
}

uint64_t MidenObjectWriter::writeOneObject(MCAssembler &Asm,
                                          const MCAsmLayout &Layout,
                                          DwoMode Mode) {
  uint64_t StartOffset = W->OS.tell();
  SectionCount = 0;
  CustomSections.clear();

  LLVM_DEBUG(dbgs() << "MidenObjectWriter::writeObject\n");

  // Collect information from the available symbols.
  SmallVector<MidenFunction, 4> Functions;
  SmallVector<uint32_t, 4> TableElems;
  SmallVector<miden::MidenImport, 4> Imports;
  SmallVector<miden::MidenExport, 4> Exports;
  SmallVector<uint32_t, 2> TagTypes;
  SmallVector<miden::MidenGlobal, 1> Globals;
  SmallVector<miden::MidenTable, 1> Tables;
  SmallVector<miden::MidenSymbolInfo, 4> SymbolInfos;
  SmallVector<std::pair<uint16_t, uint32_t>, 2> InitFuncs;
  std::map<StringRef, std::vector<MidenComdatEntry>> Comdats;
  uint64_t DataSize = 0;
  if (Mode != DwoMode::DwoOnly) {
    prepareImports(Imports, Asm, Layout);
  }

  // Populate DataSegments and CustomSections, which must be done before
  // populating DataLocations.
  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionMiden &>(Sec);
    StringRef SectionName = Section.getName();

    if (Mode == DwoMode::NonDwoOnly && isDwoSection(Sec))
      continue;
    if (Mode == DwoMode::DwoOnly && !isDwoSection(Sec))
      continue;

    LLVM_DEBUG(dbgs() << "Processing Section " << SectionName << "  group "
                      << Section.getGroup() << "\n";);

    // .init_array sections are handled specially elsewhere.
    if (SectionName.startswith(".init_array"))
      continue;

    // Code is handled separately
    if (Section.getKind().isText())
      continue;

    if (Section.isMidenData()) {
      uint32_t SegmentIndex = DataSegments.size();
      DataSize = alignTo(DataSize, Section.getAlignment());
      DataSegments.emplace_back();
      MidenDataSegment &Segment = DataSegments.back();
      Segment.Name = SectionName;
      Segment.InitFlags = Section.getPassive()
                              ? (uint32_t)miden::MIDEN_DATA_SEGMENT_IS_PASSIVE
                              : 0;
      Segment.Offset = DataSize;
      Segment.Section = &Section;
      addData(Segment.Data, Section);
      Segment.Alignment = Log2_32(Section.getAlignment());
      Segment.LinkingFlags = Section.getSegmentFlags();
      DataSize += Segment.Data.size();
      Section.setSegmentIndex(SegmentIndex);

      if (const MCSymbolMiden *C = Section.getGroup()) {
        Comdats[C->getName()].emplace_back(
            MidenComdatEntry{miden::MIDEN_COMDAT_DATA, SegmentIndex});
      }
    } else {
      // Create custom sections
      assert(Sec.getKind().isMetadata());

      StringRef Name = SectionName;

      // For user-defined custom sections, strip the prefix
      if (Name.startswith(".custom_section."))
        Name = Name.substr(strlen(".custom_section."));

      MCSymbol *Begin = Sec.getBeginSymbol();
      if (Begin) {
        assert(MidenIndices.count(cast<MCSymbolMiden>(Begin)) == 0);
        MidenIndices[cast<MCSymbolMiden>(Begin)] = CustomSections.size();
      }

      // Separate out the producers and target features sections
      if (Name == "producers") {
        ProducersSection = std::make_unique<MidenCustomSection>(Name, &Section);
        continue;
      }
      if (Name == "target_features") {
        TargetFeaturesSection =
            std::make_unique<MidenCustomSection>(Name, &Section);
        continue;
      }

      // Custom sections can also belong to COMDAT groups. In this case the
      // decriptor's "index" field is the section index (in the final object
      // file), but that is not known until after layout, so it must be fixed up
      // later
      if (const MCSymbolMiden *C = Section.getGroup()) {
        Comdats[C->getName()].emplace_back(
            MidenComdatEntry{miden::MIDEN_COMDAT_SECTION,
                            static_cast<uint32_t>(CustomSections.size())});
      }

      CustomSections.emplace_back(Name, &Section);
    }
  }

  if (Mode != DwoMode::DwoOnly) {
    // Populate MidenIndices and DataLocations for defined symbols.
    for (const MCSymbol &S : Asm.symbols()) {
      // Ignore unnamed temporary symbols, which aren't ever exported, imported,
      // or used in relocations.
      if (S.isTemporary() && S.getName().empty())
        continue;

      const auto &WS = static_cast<const MCSymbolMiden &>(S);
      LLVM_DEBUG(
          dbgs() << "MCSymbol: "
                 << toString(WS.getType().value_or(miden::MIDEN_SYMBOL_TYPE_DATA))
                 << " '" << S << "'"
                 << " isDefined=" << S.isDefined() << " isExternal="
                 << S.isExternal() << " isTemporary=" << S.isTemporary()
                 << " isWeak=" << WS.isWeak() << " isHidden=" << WS.isHidden()
                 << " isVariable=" << WS.isVariable() << "\n");

      if (WS.isVariable())
        continue;
      if (WS.isComdat() && !WS.isDefined())
        continue;

      if (WS.isFunction()) {
        unsigned Index;
        if (WS.isDefined()) {
          if (WS.getOffset() != 0)
            report_fatal_error(
                "function sections must contain one function each");

          if (WS.getSize() == nullptr)
            report_fatal_error(
                "function symbols must have a size set with .size");

          // A definition. Write out the function body.
          Index = NumFunctionImports + Functions.size();
          MidenFunction Func;
          Func.SigIndex = getFunctionType(WS);
          Func.Sym = &WS;
          assert(MidenIndices.count(&WS) == 0);
          MidenIndices[&WS] = Index;
          Functions.push_back(Func);

          auto &Section = static_cast<MCSectionMiden &>(WS.getSection());
          if (const MCSymbolMiden *C = Section.getGroup()) {
            Comdats[C->getName()].emplace_back(
                MidenComdatEntry{miden::MIDEN_COMDAT_FUNCTION, Index});
          }

          if (WS.hasExportName()) {
            miden::MidenExport Export;
            Export.Name = WS.getExportName();
            Export.Kind = miden::MIDEN_EXTERNAL_FUNCTION;
            Export.Index = Index;
            Exports.push_back(Export);
          }
        } else {
          // An import; the index was assigned above.
          Index = MidenIndices.find(&WS)->second;
        }

        LLVM_DEBUG(dbgs() << "  -> function index: " << Index << "\n");

      } else if (WS.isData()) {
        if (!isInSymtab(WS))
          continue;

        if (!WS.isDefined()) {
          LLVM_DEBUG(dbgs() << "  -> segment index: -1"
                            << "\n");
          continue;
        }

        if (!WS.getSize())
          report_fatal_error("data symbols must have a size set with .size: " +
                             WS.getName());

        int64_t Size = 0;
        if (!WS.getSize()->evaluateAsAbsolute(Size, Layout))
          report_fatal_error(".size expression must be evaluatable");

        auto &DataSection = static_cast<MCSectionMiden &>(WS.getSection());
        if (!DataSection.isMidenData())
          report_fatal_error("data symbols must live in a data section: " +
                             WS.getName());

        // For each data symbol, export it in the symtab as a reference to the
        // corresponding Miden data segment.
        miden::MidenDataReference Ref = miden::MidenDataReference{
            DataSection.getSegmentIndex(), Layout.getSymbolOffset(WS),
            static_cast<uint64_t>(Size)};
        assert(DataLocations.count(&WS) == 0);
        DataLocations[&WS] = Ref;
        LLVM_DEBUG(dbgs() << "  -> segment index: " << Ref.Segment << "\n");

      } else if (WS.isGlobal()) {
        // A "true" Miden global (currently just __stack_pointer)
        if (WS.isDefined()) {
          miden::MidenGlobal Global;
          Global.Type = WS.getGlobalType();
          Global.Index = NumGlobalImports + Globals.size();
          Global.InitExpr.Extended = false;
          switch (Global.Type.Type) {
          case miden::MIDEN_TYPE_I32:
            Global.InitExpr.Inst.Opcode = miden::MIDEN_OPCODE_I32_CONST;
            break;
          case miden::MIDEN_TYPE_I64:
            Global.InitExpr.Inst.Opcode = miden::MIDEN_OPCODE_I64_CONST;
            break;
          case miden::MIDEN_TYPE_F32:
            Global.InitExpr.Inst.Opcode = miden::MIDEN_OPCODE_F32_CONST;
            break;
          case miden::MIDEN_TYPE_F64:
            Global.InitExpr.Inst.Opcode = miden::MIDEN_OPCODE_F64_CONST;
            break;
          case miden::MIDEN_TYPE_EXTERNREF:
            Global.InitExpr.Inst.Opcode = miden::MIDEN_OPCODE_REF_NULL;
            break;
          default:
            llvm_unreachable("unexpected type");
          }
          assert(MidenIndices.count(&WS) == 0);
          MidenIndices[&WS] = Global.Index;
          Globals.push_back(Global);
        } else {
          // An import; the index was assigned above
          LLVM_DEBUG(dbgs() << "  -> global index: "
                            << MidenIndices.find(&WS)->second << "\n");
        }
      } else if (WS.isTable()) {
        if (WS.isDefined()) {
          miden::MidenTable Table;
          Table.Index = NumTableImports + Tables.size();
          Table.Type = WS.getTableType();
          assert(MidenIndices.count(&WS) == 0);
          MidenIndices[&WS] = Table.Index;
          Tables.push_back(Table);
        }
        LLVM_DEBUG(dbgs() << " -> table index: "
                          << MidenIndices.find(&WS)->second << "\n");
      } else if (WS.isTag()) {
        // C++ exception symbol (__cpp_exception) or longjmp symbol
        // (__c_longjmp)
        unsigned Index;
        if (WS.isDefined()) {
          Index = NumTagImports + TagTypes.size();
          uint32_t SigIndex = getTagType(WS);
          assert(MidenIndices.count(&WS) == 0);
          MidenIndices[&WS] = Index;
          TagTypes.push_back(SigIndex);
        } else {
          // An import; the index was assigned above.
          assert(MidenIndices.count(&WS) > 0);
        }
        LLVM_DEBUG(dbgs() << "  -> tag index: " << MidenIndices.find(&WS)->second
                          << "\n");

      } else {
        assert(WS.isSection());
      }
    }

    // Populate MidenIndices and DataLocations for aliased symbols.  We need to
    // process these in a separate pass because we need to have processed the
    // target of the alias before the alias itself and the symbols are not
    // necessarily ordered in this way.
    for (const MCSymbol &S : Asm.symbols()) {
      if (!S.isVariable())
        continue;

      assert(S.isDefined());

      const auto *BS = Layout.getBaseSymbol(S);
      if (!BS)
        report_fatal_error(Twine(S.getName()) +
                           ": absolute addressing not supported!");
      const MCSymbolMiden *Base = cast<MCSymbolMiden>(BS);

      // Find the target symbol of this weak alias and export that index
      const auto &WS = static_cast<const MCSymbolMiden &>(S);
      LLVM_DEBUG(dbgs() << WS.getName() << ": weak alias of '" << *Base
                        << "'\n");

      if (Base->isFunction()) {
        assert(MidenIndices.count(Base) > 0);
        uint32_t MidenIndex = MidenIndices.find(Base)->second;
        assert(MidenIndices.count(&WS) == 0);
        MidenIndices[&WS] = MidenIndex;
        LLVM_DEBUG(dbgs() << "  -> index:" << MidenIndex << "\n");
      } else if (Base->isData()) {
        auto &DataSection = static_cast<MCSectionMiden &>(WS.getSection());
        uint64_t Offset = Layout.getSymbolOffset(S);
        int64_t Size = 0;
        // For data symbol alias we use the size of the base symbol as the
        // size of the alias.  When an offset from the base is involved this
        // can result in a offset + size goes past the end of the data section
        // which out object format doesn't support.  So we must clamp it.
        if (!Base->getSize()->evaluateAsAbsolute(Size, Layout))
          report_fatal_error(".size expression must be evaluatable");
        const MidenDataSegment &Segment =
            DataSegments[DataSection.getSegmentIndex()];
        Size =
            std::min(static_cast<uint64_t>(Size), Segment.Data.size() - Offset);
        miden::MidenDataReference Ref = miden::MidenDataReference{
            DataSection.getSegmentIndex(),
            static_cast<uint32_t>(Layout.getSymbolOffset(S)),
            static_cast<uint32_t>(Size)};
        DataLocations[&WS] = Ref;
        LLVM_DEBUG(dbgs() << "  -> index:" << Ref.Segment << "\n");
      } else {
        report_fatal_error("don't yet support global/tag aliases");
      }
    }
  }

  // Finally, populate the symbol table itself, in its "natural" order.
  for (const MCSymbol &S : Asm.symbols()) {
    const auto &WS = static_cast<const MCSymbolMiden &>(S);
    if (!isInSymtab(WS)) {
      WS.setIndex(InvalidIndex);
      continue;
    }
    LLVM_DEBUG(dbgs() << "adding to symtab: " << WS << "\n");

    uint32_t Flags = 0;
    if (WS.isWeak())
      Flags |= miden::MIDEN_SYMBOL_BINDING_WEAK;
    if (WS.isHidden())
      Flags |= miden::MIDEN_SYMBOL_VISIBILITY_HIDDEN;
    if (!WS.isExternal() && WS.isDefined())
      Flags |= miden::MIDEN_SYMBOL_BINDING_LOCAL;
    if (WS.isUndefined())
      Flags |= miden::MIDEN_SYMBOL_UNDEFINED;
    if (WS.isNoStrip()) {
      Flags |= miden::MIDEN_SYMBOL_NO_STRIP;
      if (isEmscripten()) {
        Flags |= miden::MIDEN_SYMBOL_EXPORTED;
      }
    }
    if (WS.hasImportName())
      Flags |= miden::MIDEN_SYMBOL_EXPLICIT_NAME;
    if (WS.hasExportName())
      Flags |= miden::MIDEN_SYMBOL_EXPORTED;
    if (WS.isTLS())
      Flags |= miden::MIDEN_SYMBOL_TLS;

    miden::MidenSymbolInfo Info;
    Info.Name = WS.getName();
    Info.Kind = WS.getType().value_or(miden::MIDEN_SYMBOL_TYPE_DATA);
    Info.Flags = Flags;
    if (!WS.isData()) {
      assert(MidenIndices.count(&WS) > 0);
      Info.ElementIndex = MidenIndices.find(&WS)->second;
    } else if (WS.isDefined()) {
      assert(DataLocations.count(&WS) > 0);
      Info.DataRef = DataLocations.find(&WS)->second;
    }
    WS.setIndex(SymbolInfos.size());
    SymbolInfos.emplace_back(Info);
  }

  {
    auto HandleReloc = [&](const MidenRelocationEntry &Rel) {
      // Functions referenced by a relocation need to put in the table.  This is
      // purely to make the object file's provisional values readable, and is
      // ignored by the linker, which re-calculates the relocations itself.
      if (Rel.Type != miden::R_MIDEN_TABLE_INDEX_I32 &&
          Rel.Type != miden::R_MIDEN_TABLE_INDEX_I64 &&
          Rel.Type != miden::R_MIDEN_TABLE_INDEX_SLEB &&
          Rel.Type != miden::R_MIDEN_TABLE_INDEX_SLEB64 &&
          Rel.Type != miden::R_MIDEN_TABLE_INDEX_REL_SLEB &&
          Rel.Type != miden::R_MIDEN_TABLE_INDEX_REL_SLEB64)
        return;
      assert(Rel.Symbol->isFunction());
      const MCSymbolMiden *Base =
          cast<MCSymbolMiden>(Layout.getBaseSymbol(*Rel.Symbol));
      uint32_t FunctionIndex = MidenIndices.find(Base)->second;
      uint32_t TableIndex = TableElems.size() + InitialTableOffset;
      if (TableIndices.try_emplace(Base, TableIndex).second) {
        LLVM_DEBUG(dbgs() << "  -> adding " << Base->getName()
                          << " to table: " << TableIndex << "\n");
        TableElems.push_back(FunctionIndex);
        registerFunctionType(*Base);
      }
    };

    for (const MidenRelocationEntry &RelEntry : CodeRelocations)
      HandleReloc(RelEntry);
    for (const MidenRelocationEntry &RelEntry : DataRelocations)
      HandleReloc(RelEntry);
  }

  // Translate .init_array section contents into start functions.
  for (const MCSection &S : Asm) {
    const auto &WS = static_cast<const MCSectionMiden &>(S);
    if (WS.getName().startswith(".fini_array"))
      report_fatal_error(".fini_array sections are unsupported");
    if (!WS.getName().startswith(".init_array"))
      continue;
    if (WS.getFragmentList().empty())
      continue;

    // init_array is expected to contain a single non-empty data fragment
    if (WS.getFragmentList().size() != 3)
      report_fatal_error("only one .init_array section fragment supported");

    auto IT = WS.begin();
    const MCFragment &EmptyFrag = *IT;
    if (EmptyFrag.getKind() != MCFragment::FT_Data)
      report_fatal_error(".init_array section should be aligned");

    IT = std::next(IT);
    const MCFragment &AlignFrag = *IT;
    if (AlignFrag.getKind() != MCFragment::FT_Align)
      report_fatal_error(".init_array section should be aligned");
    if (cast<MCAlignFragment>(AlignFrag).getAlignment() !=
        Align(is64Bit() ? 8 : 4))
      report_fatal_error(".init_array section should be aligned for pointers");

    const MCFragment &Frag = *std::next(IT);
    if (Frag.hasInstructions() || Frag.getKind() != MCFragment::FT_Data)
      report_fatal_error("only data supported in .init_array section");

    uint16_t Priority = UINT16_MAX;
    unsigned PrefixLength = strlen(".init_array");
    if (WS.getName().size() > PrefixLength) {
      if (WS.getName()[PrefixLength] != '.')
        report_fatal_error(
            ".init_array section priority should start with '.'");
      if (WS.getName().substr(PrefixLength + 1).getAsInteger(10, Priority))
        report_fatal_error("invalid .init_array section priority");
    }
    const auto &DataFrag = cast<MCDataFragment>(Frag);
    const SmallVectorImpl<char> &Contents = DataFrag.getContents();
    for (const uint8_t *
             P = (const uint8_t *)Contents.data(),
            *End = (const uint8_t *)Contents.data() + Contents.size();
         P != End; ++P) {
      if (*P != 0)
        report_fatal_error("non-symbolic data in .init_array section");
    }
    for (const MCFixup &Fixup : DataFrag.getFixups()) {
      assert(Fixup.getKind() ==
             MCFixup::getKindForSize(is64Bit() ? 8 : 4, false));
      const MCExpr *Expr = Fixup.getValue();
      auto *SymRef = dyn_cast<MCSymbolRefExpr>(Expr);
      if (!SymRef)
        report_fatal_error("fixups in .init_array should be symbol references");
      const auto &TargetSym = cast<const MCSymbolMiden>(SymRef->getSymbol());
      if (TargetSym.getIndex() == InvalidIndex)
        report_fatal_error("symbols in .init_array should exist in symtab");
      if (!TargetSym.isFunction())
        report_fatal_error("symbols in .init_array should be for functions");
      InitFuncs.push_back(
          std::make_pair(Priority, TargetSym.getIndex()));
    }
  }

  // Write out the Miden header.
  writeHeader(Asm);

  uint32_t CodeSectionIndex, DataSectionIndex;
  if (Mode != DwoMode::DwoOnly) {
    writeTypeSection(Signatures);
    writeImportSection(Imports, DataSize, TableElems.size());
    writeFunctionSection(Functions);
    writeTableSection(Tables);
    // Skip the "memory" section; we import the memory instead.
    writeTagSection(TagTypes);
    writeGlobalSection(Globals);
    writeExportSection(Exports);
    const MCSymbol *IndirectFunctionTable =
        Asm.getContext().lookupSymbol("__indirect_function_table");
    writeElemSection(cast_or_null<const MCSymbolMiden>(IndirectFunctionTable),
                     TableElems);
    writeDataCountSection();

    CodeSectionIndex = writeCodeSection(Asm, Layout, Functions);
    DataSectionIndex = writeDataSection(Layout);
  }

  // The Sections in the COMDAT list have placeholder indices (their index among
  // custom sections, rather than among all sections). Fix them up here.
  for (auto &Group : Comdats) {
    for (auto &Entry : Group.second) {
      if (Entry.Kind == miden::MIDEN_COMDAT_SECTION) {
        Entry.Index += SectionCount;
      }
    }
  }
  for (auto &CustomSection : CustomSections)
    writeCustomSection(CustomSection, Asm, Layout);

  if (Mode != DwoMode::DwoOnly) {
    writeLinkingMetaDataSection(SymbolInfos, InitFuncs, Comdats);

    writeRelocSection(CodeSectionIndex, "CODE", CodeRelocations);
    writeRelocSection(DataSectionIndex, "DATA", DataRelocations);
  }
  writeCustomRelocSections();
  if (ProducersSection)
    writeCustomSection(*ProducersSection, Asm, Layout);
  if (TargetFeaturesSection)
    writeCustomSection(*TargetFeaturesSection, Asm, Layout);

  // TODO: Translate the .comment section to the output.
  return W->OS.tell() - StartOffset;
}

std::unique_ptr<MCObjectWriter>
llvm::createMidenObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                             raw_pwrite_stream &OS) {
  return std::make_unique<MidenObjectWriter>(std::move(MOTW), OS);
}

std::unique_ptr<MCObjectWriter>
llvm::createMidenDwoObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                                raw_pwrite_stream &OS,
                                raw_pwrite_stream &DwoOS) {
  return std::make_unique<MidenObjectWriter>(std::move(MOTW), OS, DwoOS);
}
