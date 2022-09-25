//===-- MidenMidenObjectWriter.cpp - Miden Miden Writer ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file handles Miden-specific object emission, converting LLVM's
/// internal fixups into the appropriate relocations.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/MidenFixupKinds.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "llvm/BinaryFormat/Miden.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCMidenObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSectionMiden.h"
#include "llvm/MC/MCSymbolMiden.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class MidenMidenObjectWriter final : public MCMidenObjectTargetWriter {
public:
  explicit MidenMidenObjectWriter(bool Is64Bit, bool IsEmscripten);

private:
  unsigned getRelocType(const MCValue &Target, const MCFixup &Fixup,
                        const MCSectionMiden &FixupSection,
                        bool IsLocRel) const override;
};
} // end anonymous namespace

MidenMidenObjectWriter::MidenMidenObjectWriter(bool Is64Bit, bool IsEmscripten)
    : MCMidenObjectTargetWriter(Is64Bit, IsEmscripten) {}

static const MCSection *getTargetSection(const MCExpr *Expr) {
  if (auto SyExp = dyn_cast<MCSymbolRefExpr>(Expr)) {
    if (SyExp->getSymbol().isInSection())
      return &SyExp->getSymbol().getSection();
    return nullptr;
  }

  if (auto BinOp = dyn_cast<MCBinaryExpr>(Expr)) {
    auto SectionLHS = getTargetSection(BinOp->getLHS());
    auto SectionRHS = getTargetSection(BinOp->getRHS());
    return SectionLHS == SectionRHS ? nullptr : SectionLHS;
  }

  if (auto UnOp = dyn_cast<MCUnaryExpr>(Expr))
    return getTargetSection(UnOp->getSubExpr());

  return nullptr;
}

unsigned MidenMidenObjectWriter::getRelocType(
    const MCValue &Target, const MCFixup &Fixup,
    const MCSectionMiden &FixupSection, bool IsLocRel) const {
  const MCSymbolRefExpr *RefA = Target.getSymA();
  assert(RefA);
  auto &SymA = cast<MCSymbolMiden>(RefA->getSymbol());

  MCSymbolRefExpr::VariantKind Modifier = Target.getAccessVariant();

  switch (Modifier) {
  case MCSymbolRefExpr::VK_GOT:
  case MCSymbolRefExpr::VK_MIDEN_GOT_TLS:
    return miden::R_MIDEN_GLOBAL_INDEX_LEB;
  case MCSymbolRefExpr::VK_MIDEN_TBREL:
    assert(SymA.isFunction());
    return is64Bit() ? miden::R_MIDEN_TABLE_INDEX_REL_SLEB64
                     : miden::R_MIDEN_TABLE_INDEX_REL_SLEB;
  case MCSymbolRefExpr::VK_MIDEN_TLSREL:
    return is64Bit() ? miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB64
                     : miden::R_MIDEN_MEMORY_ADDR_TLS_SLEB;
  case MCSymbolRefExpr::VK_MIDEN_MBREL:
    assert(SymA.isData());
    return is64Bit() ? miden::R_MIDEN_MEMORY_ADDR_REL_SLEB64
                     : miden::R_MIDEN_MEMORY_ADDR_REL_SLEB;
  case MCSymbolRefExpr::VK_MIDEN_TYPEINDEX:
    return miden::R_MIDEN_TYPE_INDEX_LEB;
  case MCSymbolRefExpr::VK_None:
    break;
  default:
    report_fatal_error("unknown VariantKind");
    break;
  }

  switch (unsigned(Fixup.getKind())) {
  case Miden::fixup_sleb128_i32:
    if (SymA.isFunction())
      return miden::R_MIDEN_TABLE_INDEX_SLEB;
    return miden::R_MIDEN_MEMORY_ADDR_SLEB;
  case Miden::fixup_sleb128_i64:
    if (SymA.isFunction())
      return miden::R_MIDEN_TABLE_INDEX_SLEB64;
    return miden::R_MIDEN_MEMORY_ADDR_SLEB64;
  case Miden::fixup_uleb128_i32:
    if (SymA.isGlobal())
      return miden::R_MIDEN_GLOBAL_INDEX_LEB;
    if (SymA.isFunction())
      return miden::R_MIDEN_FUNCTION_INDEX_LEB;
    if (SymA.isTag())
      return miden::R_MIDEN_TAG_INDEX_LEB;
    if (SymA.isTable())
      return miden::R_MIDEN_TABLE_NUMBER_LEB;
    return miden::R_MIDEN_MEMORY_ADDR_LEB;
  case Miden::fixup_uleb128_i64:
    assert(SymA.isData());
    return miden::R_MIDEN_MEMORY_ADDR_LEB64;
  case FK_Data_4:
    if (SymA.isFunction()) {
      if (FixupSection.getKind().isMetadata())
        return miden::R_MIDEN_FUNCTION_OFFSET_I32;
      assert(FixupSection.isMidenData());
      return miden::R_MIDEN_TABLE_INDEX_I32;
    }
    if (SymA.isGlobal())
      return miden::R_MIDEN_GLOBAL_INDEX_I32;
    if (auto Section = static_cast<const MCSectionMiden *>(
            getTargetSection(Fixup.getValue()))) {
      if (Section->getKind().isText())
        return miden::R_MIDEN_FUNCTION_OFFSET_I32;
      else if (!Section->isMidenData())
        return miden::R_MIDEN_SECTION_OFFSET_I32;
    }
    return IsLocRel ? miden::R_MIDEN_MEMORY_ADDR_LOCREL_I32
                    : miden::R_MIDEN_MEMORY_ADDR_I32;
  case FK_Data_8:
    if (SymA.isFunction()) {
      if (FixupSection.getKind().isMetadata())
        return miden::R_MIDEN_FUNCTION_OFFSET_I64;
      return miden::R_MIDEN_TABLE_INDEX_I64;
    }
    if (SymA.isGlobal())
      llvm_unreachable("unimplemented R_MIDEN_GLOBAL_INDEX_I64");
    if (auto Section = static_cast<const MCSectionMiden *>(
            getTargetSection(Fixup.getValue()))) {
      if (Section->getKind().isText())
        return miden::R_MIDEN_FUNCTION_OFFSET_I64;
      else if (!Section->isMidenData())
        llvm_unreachable("unimplemented R_MIDEN_SECTION_OFFSET_I64");
    }
    assert(SymA.isData());
    return miden::R_MIDEN_MEMORY_ADDR_I64;
  default:
    llvm_unreachable("unimplemented fixup kind");
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createMidenMidenObjectWriter(bool Is64Bit, bool IsEmscripten) {
  return std::make_unique<MidenMidenObjectWriter>(Is64Bit, IsEmscripten);
}
