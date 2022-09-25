//===- MCSymbolMiden.h -  ----------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_MC_MCSYMBOLMIDEN_H
#define LLVM_MC_MCSYMBOLMIDEN_H

#include "llvm/BinaryFormat/Miden.h"
#include "llvm/MC/MCSymbol.h"

namespace llvm {

class MCSymbolMiden : public MCSymbol {
  Optional<miden::MidenSymbolType> Type;
  bool IsWeak = false;
  bool IsHidden = false;
  bool IsComdat = false;
  bool OmitFromLinkingSection = false;
  mutable bool IsUsedInInitArray = false;
  mutable bool IsUsedInGOT = false;
  Optional<StringRef> ImportModule;
  Optional<StringRef> ImportName;
  Optional<StringRef> ExportName;
  miden::MidenSignature *Signature = nullptr;
  Optional<miden::MidenGlobalType> GlobalType;
  Optional<miden::MidenTableType> TableType;

  /// An expression describing how to calculate the size of a symbol. If a
  /// symbol has no size this field will be NULL.
  const MCExpr *SymbolSize = nullptr;

public:
  MCSymbolMiden(const StringMapEntry<bool> *Name, bool isTemporary)
      : MCSymbol(SymbolKindMiden, Name, isTemporary) {}
  static bool classof(const MCSymbol *S) { return S->isMiden(); }

  const MCExpr *getSize() const { return SymbolSize; }
  void setSize(const MCExpr *SS) { SymbolSize = SS; }

  bool isFunction() const { return Type == miden::MIDEN_SYMBOL_TYPE_FUNCTION; }
  // Data is the default value if not set.
  bool isData() const { return !Type || Type == miden::MIDEN_SYMBOL_TYPE_DATA; }
  bool isGlobal() const { return Type == miden::MIDEN_SYMBOL_TYPE_GLOBAL; }
  bool isTable() const { return Type == miden::MIDEN_SYMBOL_TYPE_TABLE; }
  bool isSection() const { return Type == miden::MIDEN_SYMBOL_TYPE_SECTION; }
  bool isTag() const { return Type == miden::MIDEN_SYMBOL_TYPE_TAG; }

  Optional<miden::MidenSymbolType> getType() const { return Type; }

  void setType(miden::MidenSymbolType type) { Type = type; }

  bool isExported() const { return getFlags() & miden::MIDEN_SYMBOL_EXPORTED; }
  void setExported() const {
    modifyFlags(miden::MIDEN_SYMBOL_EXPORTED, miden::MIDEN_SYMBOL_EXPORTED);
  }

  bool isNoStrip() const { return getFlags() & miden::MIDEN_SYMBOL_NO_STRIP; }
  void setNoStrip() const {
    modifyFlags(miden::MIDEN_SYMBOL_NO_STRIP, miden::MIDEN_SYMBOL_NO_STRIP);
  }

  bool isTLS() const { return getFlags() & miden::MIDEN_SYMBOL_TLS; }
  void setTLS() const {
    modifyFlags(miden::MIDEN_SYMBOL_TLS, miden::MIDEN_SYMBOL_TLS);
  }

  bool isWeak() const { return IsWeak; }
  void setWeak(bool isWeak) { IsWeak = isWeak; }

  bool isHidden() const { return IsHidden; }
  void setHidden(bool isHidden) { IsHidden = isHidden; }

  bool isComdat() const { return IsComdat; }
  void setComdat(bool isComdat) { IsComdat = isComdat; }

  // miden-ld understands a finite set of symbol types.  This flag allows the
  // compiler to avoid emitting symbol table entries that would confuse the
  // linker, unless the user specifically requests the feature.
  bool omitFromLinkingSection() const { return OmitFromLinkingSection; }
  void setOmitFromLinkingSection() { OmitFromLinkingSection = true; }

  bool hasImportModule() const { return ImportModule.has_value(); }
  StringRef getImportModule() const {
    if (ImportModule)
      return ImportModule.value();
    // Use a default module name of "env" for now, for compatibility with
    // existing tools.
    // TODO(sbc): Find a way to specify a default value in the object format
    // without picking a hardcoded value like this.
    return "env";
  }
  void setImportModule(StringRef Name) { ImportModule = Name; }

  bool hasImportName() const { return ImportName.has_value(); }
  StringRef getImportName() const {
    if (ImportName)
      return ImportName.value();
    return getName();
  }
  void setImportName(StringRef Name) { ImportName = Name; }

  bool hasExportName() const { return ExportName.has_value(); }
  StringRef getExportName() const { return ExportName.value(); }
  void setExportName(StringRef Name) { ExportName = Name; }

  bool isFunctionTable() const {
    return isTable() && hasTableType() &&
           getTableType().ElemType == miden::MIDEN_TYPE_FUNCREF;
  }
  void setFunctionTable() {
    setType(miden::MIDEN_SYMBOL_TYPE_TABLE);
    setTableType(miden::ValType::FUNCREF);
  }

  void setUsedInGOT() const { IsUsedInGOT = true; }
  bool isUsedInGOT() const { return IsUsedInGOT; }

  void setUsedInInitArray() const { IsUsedInInitArray = true; }
  bool isUsedInInitArray() const { return IsUsedInInitArray; }

  const miden::MidenSignature *getSignature() const { return Signature; }
  void setSignature(miden::MidenSignature *Sig) { Signature = Sig; }

  const miden::MidenGlobalType &getGlobalType() const {
    assert(GlobalType);
    return GlobalType.value();
  }
  void setGlobalType(miden::MidenGlobalType GT) { GlobalType = GT; }

  bool hasTableType() const { return TableType.has_value(); }
  const miden::MidenTableType &getTableType() const {
    assert(hasTableType());
    return TableType.value();
  }
  void setTableType(miden::MidenTableType TT) { TableType = TT; }
  void setTableType(miden::ValType VT) {
    // Declare a table with element type VT and no limits (min size 0, no max
    // size).
    miden::MidenLimits Limits = {miden::MIDEN_LIMITS_FLAG_NONE, 0, 0};
    setTableType({uint8_t(VT), Limits});
  }
};

} // end namespace llvm

#endif // LLVM_MC_MCSYMBOLMIDEN_H
