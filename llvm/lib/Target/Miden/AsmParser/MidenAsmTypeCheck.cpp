//==- MidenAsmTypeCheck.cpp - Assembler for Miden -*- C++ -*-==//
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

#include "AsmParser/MidenAsmTypeCheck.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "MCTargetDesc/MidenTargetStreamer.h"
#include "Miden.h"
#include "TargetInfo/MidenTargetInfo.h"
#include "Utils/MidenTypeUtilities.h"
#include "Utils/MidenUtilities.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCSectionMiden.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolMiden.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/SourceMgr.h"

using namespace llvm;

#define DEBUG_TYPE "miden-asm-parser"

extern StringRef GetMnemonic(unsigned Opc);

namespace llvm {

MidenAsmTypeCheck::MidenAsmTypeCheck(MCAsmParser &Parser,
                                     const MCInstrInfo &MII, bool is64)
    : Parser(Parser), MII(MII), is64(is64) {}

void MidenAsmTypeCheck::funcDecl(const miden::MidenSignature &Sig) {
  LocalTypes.assign(Sig.Params.begin(), Sig.Params.end());
  ReturnTypes.assign(Sig.Returns.begin(), Sig.Returns.end());
}

void MidenAsmTypeCheck::localDecl(
    const SmallVector<miden::ValType, 4> &Locals) {
  LocalTypes.insert(LocalTypes.end(), Locals.begin(), Locals.end());
}

void MidenAsmTypeCheck::dumpTypeStack(Twine Msg) {
  LLVM_DEBUG({
    std::string s;
    for (auto VT : Stack) {
      s += Miden::typeToString(VT);
      s += " ";
    }
    dbgs() << Msg << s << '\n';
  });
}

bool MidenAsmTypeCheck::typeError(SMLoc ErrorLoc, const Twine &Msg) {
  // Once you get one type error in a function, it will likely trigger more
  // which are mostly not helpful.
  if (TypeErrorThisFunction)
    return true;
  // If we're currently in unreachable code, we surpress errors as well.
  if (Unreachable)
    return true;
  TypeErrorThisFunction = true;
  dumpTypeStack("current stack: ");
  return Parser.Error(ErrorLoc, Msg);
}

bool MidenAsmTypeCheck::popType(SMLoc ErrorLoc, Optional<miden::ValType> EVT) {
  if (Stack.empty()) {
    return typeError(ErrorLoc,
                     EVT ? StringRef("empty stack while popping ") +
                               Miden::typeToString(EVT.value())
                         : StringRef("empty stack while popping value"));
  }
  auto PVT = Stack.pop_back_val();
  if (EVT && EVT.value() != PVT) {
    return typeError(ErrorLoc, StringRef("popped ") + Miden::typeToString(PVT) +
                                   ", expected " +
                                   Miden::typeToString(EVT.value()));
  }
  return false;
}

bool MidenAsmTypeCheck::popRefType(SMLoc ErrorLoc) {
  if (Stack.empty()) {
    return typeError(ErrorLoc, StringRef("empty stack while popping reftype"));
  }
  auto PVT = Stack.pop_back_val();
  if (!Miden::isRefType(PVT)) {
    return typeError(ErrorLoc, StringRef("popped ") + Miden::typeToString(PVT) +
                                   ", expected reftype");
  }
  return false;
}

bool MidenAsmTypeCheck::getLocal(SMLoc ErrorLoc, const MCInst &Inst,
                                 miden::ValType &Type) {
  auto Local = static_cast<size_t>(Inst.getOperand(0).getImm());
  if (Local >= LocalTypes.size())
    return typeError(ErrorLoc, StringRef("no local type specified for index ") +
                                   std::to_string(Local));
  Type = LocalTypes[Local];
  return false;
}

bool MidenAsmTypeCheck::checkEnd(SMLoc ErrorLoc, bool PopVals) {
  if (LastSig.Returns.size() > Stack.size())
    return typeError(ErrorLoc, "end: insufficient values on the type stack");

  if (PopVals) {
    for (auto VT : llvm::reverse(LastSig.Returns)) {
      if (popType(ErrorLoc, VT))
        return true;
    }
    return false;
  }

  for (size_t i = 0; i < LastSig.Returns.size(); i++) {
    auto EVT = LastSig.Returns[i];
    auto PVT = Stack[Stack.size() - LastSig.Returns.size() + i];
    if (PVT != EVT)
      return typeError(ErrorLoc, StringRef("end got ") +
                                     Miden::typeToString(PVT) + ", expected " +
                                     Miden::typeToString(EVT));
  }
  return false;
}

bool MidenAsmTypeCheck::checkSig(SMLoc ErrorLoc,
                                 const miden::MidenSignature &Sig) {
  for (auto VT : llvm::reverse(Sig.Params))
    if (popType(ErrorLoc, VT))
      return true;
  Stack.insert(Stack.end(), Sig.Returns.begin(), Sig.Returns.end());
  return false;
}

bool MidenAsmTypeCheck::getSymRef(SMLoc ErrorLoc, const MCInst &Inst,
                                  const MCSymbolRefExpr *&SymRef) {
  auto Op = Inst.getOperand(0);
  if (!Op.isExpr())
    return typeError(ErrorLoc, StringRef("expected expression operand"));
  SymRef = dyn_cast<MCSymbolRefExpr>(Op.getExpr());
  if (!SymRef)
    return typeError(ErrorLoc, StringRef("expected symbol operand"));
  return false;
}

bool MidenAsmTypeCheck::getGlobal(SMLoc ErrorLoc, const MCInst &Inst,
                                  miden::ValType &Type) {
  const MCSymbolRefExpr *SymRef;
  if (getSymRef(ErrorLoc, Inst, SymRef))
    return true;
  auto MidenSym = cast<MCSymbolMiden>(&SymRef->getSymbol());
  switch (MidenSym->getType().value_or(miden::MIDEN_SYMBOL_TYPE_DATA)) {
  case miden::MIDEN_SYMBOL_TYPE_GLOBAL:
    Type = static_cast<miden::ValType>(MidenSym->getGlobalType().Type);
    break;
  case miden::MIDEN_SYMBOL_TYPE_FUNCTION:
  case miden::MIDEN_SYMBOL_TYPE_DATA:
    switch (SymRef->getKind()) {
    case MCSymbolRefExpr::VK_GOT:
    case MCSymbolRefExpr::VK_MIDEN_GOT_TLS:
      Type = is64 ? miden::ValType::I64 : miden::ValType::I32;
      return false;
    default:
      break;
    }
    LLVM_FALLTHROUGH;
  default:
    return typeError(ErrorLoc, StringRef("symbol ") + MidenSym->getName() +
                                   " missing .globaltype");
  }
  return false;
}

bool MidenAsmTypeCheck::getTable(SMLoc ErrorLoc, const MCInst &Inst,
                                 miden::ValType &Type) {
  const MCSymbolRefExpr *SymRef;
  if (getSymRef(ErrorLoc, Inst, SymRef))
    return true;
  auto MidenSym = cast<MCSymbolMiden>(&SymRef->getSymbol());
  if (MidenSym->getType().value_or(miden::MIDEN_SYMBOL_TYPE_DATA) !=
      miden::MIDEN_SYMBOL_TYPE_TABLE)
    return typeError(ErrorLoc, StringRef("symbol ") + MidenSym->getName() +
                                   " missing .tabletype");
  Type = static_cast<miden::ValType>(MidenSym->getTableType().ElemType);
  return false;
}

bool MidenAsmTypeCheck::endOfFunction(SMLoc ErrorLoc) {
  // Check the return types.
  for (auto RVT : llvm::reverse(ReturnTypes)) {
    if (popType(ErrorLoc, RVT))
      return true;
  }
  if (!Stack.empty()) {
    return typeError(ErrorLoc, std::to_string(Stack.size()) +
                                   " superfluous return values");
  }
  Unreachable = true;
  return false;
}

bool MidenAsmTypeCheck::typeCheck(SMLoc ErrorLoc, const MCInst &Inst,
                                  OperandVector &Operands) {
  auto Opc = Inst.getOpcode();
  auto Name = GetMnemonic(Opc);
  dumpTypeStack("typechecking " + Name + ": ");
  miden::ValType Type;
  if (Name == "local.get") {
    if (getLocal(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    Stack.push_back(Type);
  } else if (Name == "local.set") {
    if (getLocal(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, Type))
      return true;
  } else if (Name == "local.tee") {
    if (getLocal(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, Type))
      return true;
    Stack.push_back(Type);
  } else if (Name == "global.get") {
    if (getGlobal(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    Stack.push_back(Type);
  } else if (Name == "global.set") {
    if (getGlobal(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, Type))
      return true;
  } else if (Name == "table.get") {
    if (getTable(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, miden::ValType::I32))
      return true;
    Stack.push_back(Type);
  } else if (Name == "table.set") {
    if (getTable(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, Type))
      return true;
    if (popType(ErrorLoc, miden::ValType::I32))
      return true;
  } else if (Name == "table.fill") {
    if (getTable(Operands[1]->getStartLoc(), Inst, Type))
      return true;
    if (popType(ErrorLoc, miden::ValType::I32))
      return true;
    if (popType(ErrorLoc, Type))
      return true;
    if (popType(ErrorLoc, miden::ValType::I32))
      return true;
  } else if (Name == "drop") {
    if (popType(ErrorLoc, {}))
      return true;
  } else if (Name == "end_block" || Name == "end_loop" || Name == "end_if" ||
             Name == "else" || Name == "end_try") {
    if (checkEnd(ErrorLoc, Name == "else"))
      return true;
    if (Name == "end_block")
      Unreachable = false;
  } else if (Name == "return") {
    if (endOfFunction(ErrorLoc))
      return true;
  } else if (Name == "call_indirect" || Name == "return_call_indirect") {
    // Function value.
    if (popType(ErrorLoc, miden::ValType::I32))
      return true;
    if (checkSig(ErrorLoc, LastSig))
      return true;
    if (Name == "return_call_indirect" && endOfFunction(ErrorLoc))
      return true;
  } else if (Name == "call" || Name == "return_call") {
    const MCSymbolRefExpr *SymRef;
    if (getSymRef(Operands[1]->getStartLoc(), Inst, SymRef))
      return true;
    auto MidenSym = cast<MCSymbolMiden>(&SymRef->getSymbol());
    auto Sig = MidenSym->getSignature();
    if (!Sig || MidenSym->getType() != miden::MIDEN_SYMBOL_TYPE_FUNCTION)
      return typeError(Operands[1]->getStartLoc(), StringRef("symbol ") +
                                                       MidenSym->getName() +
                                                       " missing .functype");
    if (checkSig(ErrorLoc, *Sig))
      return true;
    if (Name == "return_call" && endOfFunction(ErrorLoc))
      return true;
  } else if (Name == "catch") {
    const MCSymbolRefExpr *SymRef;
    if (getSymRef(Operands[1]->getStartLoc(), Inst, SymRef))
      return true;
    const auto *MidenSym = cast<MCSymbolMiden>(&SymRef->getSymbol());
    const auto *Sig = MidenSym->getSignature();
    if (!Sig || MidenSym->getType() != miden::MIDEN_SYMBOL_TYPE_TAG)
      return typeError(Operands[1]->getStartLoc(), StringRef("symbol ") +
                                                       MidenSym->getName() +
                                                       " missing .tagtype");
    // catch instruction pushes values whose types are specified in the tag's
    // "params" part
    Stack.insert(Stack.end(), Sig->Params.begin(), Sig->Params.end());
  } else if (Name == "unreachable") {
    Unreachable = true;
  } else if (Name == "ref.is_null") {
    if (popRefType(ErrorLoc))
      return true;
    Stack.push_back(miden::ValType::I32);
  } else {
    // The current instruction is a stack instruction which doesn't have
    // explicit operands that indicate push/pop types, so we get those from
    // the register version of the same instruction.
    auto RegOpc = Miden::getRegisterOpcode(Opc);
    assert(RegOpc != -1 && "Failed to get register version of MC instruction");
    const auto &II = MII.get(RegOpc);
    // First pop all the uses off the stack and check them.
    for (unsigned I = II.getNumOperands(); I > II.getNumDefs(); I--) {
      const auto &Op = II.OpInfo[I - 1];
      if (Op.OperandType == MCOI::OPERAND_REGISTER) {
        auto VT = Miden::regClassToValType(Op.RegClass);
        if (popType(ErrorLoc, VT))
          return true;
      }
    }
    // Now push all the defs onto the stack.
    for (unsigned I = 0; I < II.getNumDefs(); I++) {
      const auto &Op = II.OpInfo[I];
      assert(Op.OperandType == MCOI::OPERAND_REGISTER && "Register expected");
      auto VT = Miden::regClassToValType(Op.RegClass);
      Stack.push_back(VT);
    }
  }
  return false;
}

} // end namespace llvm
