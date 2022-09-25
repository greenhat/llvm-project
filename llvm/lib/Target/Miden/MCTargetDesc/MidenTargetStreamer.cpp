//==-- MidenTargetStreamer.cpp - Miden Target Streamer Methods --=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines Miden-specific target streamer classes.
/// These are for implementing support for target-specific assembly directives.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/MidenTargetStreamer.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "Utils/MidenTypeUtilities.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionMiden.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbolMiden.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

MidenTargetStreamer::MidenTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void MidenTargetStreamer::emitValueType(miden::ValType Type) {
  Streamer.emitIntValue(uint8_t(Type), 1);
}

MidenTargetAsmStreamer::MidenTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : MidenTargetStreamer(S), OS(OS) {}

MidenTargetMidenStreamer::MidenTargetMidenStreamer(MCStreamer &S)
    : MidenTargetStreamer(S) {}

static void printTypes(formatted_raw_ostream &OS,
                       ArrayRef<miden::ValType> Types) {
  bool First = true;
  for (auto Type : Types) {
    if (First)
      First = false;
    else
      OS << ", ";
    OS << Miden::typeToString(Type);
  }
  OS << '\n';
}

void MidenTargetAsmStreamer::emitLocal(ArrayRef<miden::ValType> Types) {
  if (!Types.empty()) {
    OS << "\t.local  \t";
    printTypes(OS, Types);
  }
}

void MidenTargetAsmStreamer::emitFunctionType(const MCSymbolMiden *Sym) {
  assert(Sym->isFunction());
  OS << "\t.functype\t" << Sym->getName() << " ";
  OS << Miden::signatureToString(Sym->getSignature());
  OS << "\n";
}

void MidenTargetAsmStreamer::emitGlobalType(const MCSymbolMiden *Sym) {
  assert(Sym->isGlobal());
  OS << "\t.globaltype\t" << Sym->getName() << ", "
     << Miden::typeToString(
            static_cast<miden::ValType>(Sym->getGlobalType().Type));
  if (!Sym->getGlobalType().Mutable)
    OS << ", immutable";
  OS << '\n';
}

void MidenTargetAsmStreamer::emitTableType(const MCSymbolMiden *Sym) {
  assert(Sym->isTable());
  const miden::MidenTableType &Type = Sym->getTableType();
  OS << "\t.tabletype\t" << Sym->getName() << ", "
     << Miden::typeToString(static_cast<miden::ValType>(Type.ElemType));
  bool HasMaximum = Type.Limits.Flags & miden::MIDEN_LIMITS_FLAG_HAS_MAX;
  if (Type.Limits.Minimum != 0 || HasMaximum) {
    OS << ", " << Type.Limits.Minimum;
    if (HasMaximum)
      OS << ", " << Type.Limits.Maximum;
  }
  OS << '\n';
}

void MidenTargetAsmStreamer::emitTagType(const MCSymbolMiden *Sym) {
  assert(Sym->isTag());
  OS << "\t.tagtype\t" << Sym->getName() << " ";
  OS << Miden::typeListToString(Sym->getSignature()->Params);
  OS << "\n";
}

void MidenTargetAsmStreamer::emitImportModule(const MCSymbolMiden *Sym,
                                              StringRef ImportModule) {
  OS << "\t.import_module\t" << Sym->getName() << ", " << ImportModule << '\n';
}

void MidenTargetAsmStreamer::emitImportName(const MCSymbolMiden *Sym,
                                            StringRef ImportName) {
  OS << "\t.import_name\t" << Sym->getName() << ", " << ImportName << '\n';
}

void MidenTargetAsmStreamer::emitExportName(const MCSymbolMiden *Sym,
                                            StringRef ExportName) {
  OS << "\t.export_name\t" << Sym->getName() << ", " << ExportName << '\n';
}

void MidenTargetAsmStreamer::emitIndIdx(const MCExpr *Value) {
  OS << "\t.indidx  \t" << *Value << '\n';
}

void MidenTargetMidenStreamer::emitLocal(ArrayRef<miden::ValType> Types) {
  SmallVector<std::pair<miden::ValType, uint32_t>, 4> Grouped;
  for (auto Type : Types) {
    if (Grouped.empty() || Grouped.back().first != Type)
      Grouped.push_back(std::make_pair(Type, 1));
    else
      ++Grouped.back().second;
  }

  Streamer.emitULEB128IntValue(Grouped.size());
  for (auto Pair : Grouped) {
    Streamer.emitULEB128IntValue(Pair.second);
    emitValueType(Pair.first);
  }
}

void MidenTargetMidenStreamer::emitIndIdx(const MCExpr *Value) {
  llvm_unreachable(".indidx encoding not yet implemented");
}
