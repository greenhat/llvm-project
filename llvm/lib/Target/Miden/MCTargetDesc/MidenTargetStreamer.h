//==-- MidenTargetStreamer.h - Miden Target Streamer -*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file declares Miden-specific target streamer classes.
/// These are for implementing support for target-specific assembly directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENTARGETSTREAMER_H
#define LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENTARGETSTREAMER_H

#include "llvm/BinaryFormat/Miden.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/MachineValueType.h"

namespace llvm {

class MCSymbolMiden;
class formatted_raw_ostream;

/// Miden-specific streamer interface, to implement support
/// Miden-specific assembly directives.
class MidenTargetStreamer : public MCTargetStreamer {
public:
  explicit MidenTargetStreamer(MCStreamer &S);

  /// .local
  virtual void emitLocal(ArrayRef<miden::ValType> Types) = 0;
  /// .functype
  virtual void emitFunctionType(const MCSymbolMiden *Sym) = 0;
  /// .indidx
  virtual void emitIndIdx(const MCExpr *Value) = 0;
  /// .globaltype
  virtual void emitGlobalType(const MCSymbolMiden *Sym) = 0;
  /// .tabletype
  virtual void emitTableType(const MCSymbolMiden *Sym) = 0;
  /// .tagtype
  virtual void emitTagType(const MCSymbolMiden *Sym) = 0;
  /// .import_module
  virtual void emitImportModule(const MCSymbolMiden *Sym,
                                StringRef ImportModule) = 0;
  /// .import_name
  virtual void emitImportName(const MCSymbolMiden *Sym,
                              StringRef ImportName) = 0;
  /// .export_name
  virtual void emitExportName(const MCSymbolMiden *Sym,
                              StringRef ExportName) = 0;

protected:
  void emitValueType(miden::ValType Type);
};

/// This part is for ascii assembly output
class MidenTargetAsmStreamer final : public MidenTargetStreamer {
  formatted_raw_ostream &OS;

public:
  MidenTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);

  void emitLocal(ArrayRef<miden::ValType> Types) override;
  void emitFunctionType(const MCSymbolMiden *Sym) override;
  void emitIndIdx(const MCExpr *Value) override;
  void emitGlobalType(const MCSymbolMiden *Sym) override;
  void emitTableType(const MCSymbolMiden *Sym) override;
  void emitTagType(const MCSymbolMiden *Sym) override;
  void emitImportModule(const MCSymbolMiden *Sym,
                        StringRef ImportModule) override;
  void emitImportName(const MCSymbolMiden *Sym, StringRef ImportName) override;
  void emitExportName(const MCSymbolMiden *Sym, StringRef ExportName) override;
};

/// This part is for Miden object output
class MidenTargetMidenStreamer final : public MidenTargetStreamer {
public:
  explicit MidenTargetMidenStreamer(MCStreamer &S);

  void emitLocal(ArrayRef<miden::ValType> Types) override;
  void emitFunctionType(const MCSymbolMiden *Sym) override {}
  void emitIndIdx(const MCExpr *Value) override;
  void emitGlobalType(const MCSymbolMiden *Sym) override {}
  void emitTableType(const MCSymbolMiden *Sym) override {}
  void emitTagType(const MCSymbolMiden *Sym) override {}
  void emitImportModule(const MCSymbolMiden *Sym,
                        StringRef ImportModule) override {}
  void emitImportName(const MCSymbolMiden *Sym, StringRef ImportName) override {
  }
  void emitExportName(const MCSymbolMiden *Sym, StringRef ExportName) override {
  }
};

/// This part is for null output
class MidenTargetNullStreamer final : public MidenTargetStreamer {
public:
  explicit MidenTargetNullStreamer(MCStreamer &S) : MidenTargetStreamer(S) {}

  void emitLocal(ArrayRef<miden::ValType>) override {}
  void emitFunctionType(const MCSymbolMiden *) override {}
  void emitIndIdx(const MCExpr *) override {}
  void emitGlobalType(const MCSymbolMiden *) override {}
  void emitTableType(const MCSymbolMiden *) override {}
  void emitTagType(const MCSymbolMiden *) override {}
  void emitImportModule(const MCSymbolMiden *, StringRef) override {}
  void emitImportName(const MCSymbolMiden *, StringRef) override {}
  void emitExportName(const MCSymbolMiden *, StringRef) override {}
};

} // end namespace llvm

#endif
