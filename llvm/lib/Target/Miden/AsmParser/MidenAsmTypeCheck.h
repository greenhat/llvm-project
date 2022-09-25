//==- MidenAsmTypeCheck.h - Assembler for Miden -*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file is part of the Miden Assembler.
///
/// It contains code to translate a parsed .s file into MCInsts.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_ASMPARSER_TYPECHECK_H
#define LLVM_LIB_TARGET_MIDEN_ASMPARSER_TYPECHECK_H

#include "llvm/BinaryFormat/Miden.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCSymbol.h"

namespace llvm {

class MidenAsmTypeCheck final {
  MCAsmParser &Parser;
  const MCInstrInfo &MII;

  SmallVector<miden::ValType, 8> Stack;
  SmallVector<miden::ValType, 16> LocalTypes;
  SmallVector<miden::ValType, 4> ReturnTypes;
  miden::MidenSignature LastSig;
  bool TypeErrorThisFunction = false;
  bool Unreachable = false;
  bool is64;

  void dumpTypeStack(Twine Msg);
  bool typeError(SMLoc ErrorLoc, const Twine &Msg);
  bool popType(SMLoc ErrorLoc, Optional<miden::ValType> EVT);
  bool popRefType(SMLoc ErrorLoc);
  bool getLocal(SMLoc ErrorLoc, const MCInst &Inst, miden::ValType &Type);
  bool checkEnd(SMLoc ErrorLoc, bool PopVals = false);
  bool checkSig(SMLoc ErrorLoc, const miden::MidenSignature &Sig);
  bool getSymRef(SMLoc ErrorLoc, const MCInst &Inst,
                 const MCSymbolRefExpr *&SymRef);
  bool getGlobal(SMLoc ErrorLoc, const MCInst &Inst, miden::ValType &Type);
  bool getTable(SMLoc ErrorLoc, const MCInst &Inst, miden::ValType &Type);

public:
  MidenAsmTypeCheck(MCAsmParser &Parser, const MCInstrInfo &MII, bool is64);

  void funcDecl(const miden::MidenSignature &Sig);
  void localDecl(const SmallVector<miden::ValType, 4> &Locals);
  void setLastSig(const miden::MidenSignature &Sig) { LastSig = Sig; }
  bool endOfFunction(SMLoc ErrorLoc);
  bool typeCheck(SMLoc ErrorLoc, const MCInst &Inst, OperandVector &Operands);

  void Clear() {
    Stack.clear();
    LocalTypes.clear();
    ReturnTypes.clear();
    TypeErrorThisFunction = false;
    Unreachable = false;
  }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_MIDEN_ASMPARSER_TYPECHECK_H
