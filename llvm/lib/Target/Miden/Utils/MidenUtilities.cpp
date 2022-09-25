//===-- MidenUtilities.cpp - Miden Utility Functions ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements several utility functions for Miden.
///
//===----------------------------------------------------------------------===//

#include "MidenUtilities.h"
#include "MidenMachineFunctionInfo.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/MC/MCContext.h"
using namespace llvm;

// Exception handling & setjmp-longjmp handling related options. These are
// defined here to be shared between Miden and its subdirectories.

// Emscripten's asm.js-style exception handling
cl::opt<bool> Miden::MidenEnableEmEH(
    "enable-emscripten-cxx-exceptions",
    cl::desc("Miden Emscripten-style exception handling"), cl::init(false));
// Emscripten's asm.js-style setjmp/longjmp handling
cl::opt<bool> Miden::MidenEnableEmSjLj(
    "enable-emscripten-sjlj",
    cl::desc("Miden Emscripten-style setjmp/longjmp handling"),
    cl::init(false));
// Exception handling using miden EH instructions
cl::opt<bool> Miden::MidenEnableEH("miden-enable-eh",
                                   cl::desc("Miden exception handling"),
                                   cl::init(false));
// setjmp/longjmp handling using miden EH instrutions
cl::opt<bool> Miden::MidenEnableSjLj("miden-enable-sjlj",
                                     cl::desc("Miden setjmp/longjmp handling"),
                                     cl::init(false));

// Function names in libc++abi and libunwind
const char *const Miden::CxaBeginCatchFn = "__cxa_begin_catch";
const char *const Miden::CxaRethrowFn = "__cxa_rethrow";
const char *const Miden::StdTerminateFn = "_ZSt9terminatev";
const char *const Miden::PersonalityWrapperFn = "_Unwind_Miden_CallPersonality";

/// Test whether MI is a child of some other node in an expression tree.
bool Miden::isChild(const MachineInstr &MI, const MidenFunctionInfo &MFI) {
  if (MI.getNumOperands() == 0)
    return false;
  const MachineOperand &MO = MI.getOperand(0);
  if (!MO.isReg() || MO.isImplicit() || !MO.isDef())
    return false;
  Register Reg = MO.getReg();
  return Register::isVirtualRegister(Reg) && MFI.isVRegStackified(Reg);
}

bool Miden::mayThrow(const MachineInstr &MI) {
  switch (MI.getOpcode()) {
  case Miden::THROW:
  case Miden::THROW_S:
  case Miden::RETHROW:
  case Miden::RETHROW_S:
    return true;
  }
  if (isCallIndirect(MI.getOpcode()))
    return true;
  if (!MI.isCall())
    return false;

  const MachineOperand &MO = getCalleeOp(MI);
  assert(MO.isGlobal() || MO.isSymbol());

  if (MO.isSymbol()) {
    // Some intrinsics are lowered to calls to external symbols, which are then
    // lowered to calls to library functions. Most of libcalls don't throw, but
    // we only list some of them here now.
    // TODO Consider adding 'nounwind' info in TargetLowering::CallLoweringInfo
    // instead for more accurate info.
    const char *Name = MO.getSymbolName();
    if (strcmp(Name, "memcpy") == 0 || strcmp(Name, "memmove") == 0 ||
        strcmp(Name, "memset") == 0)
      return false;
    return true;
  }

  const auto *F = dyn_cast<Function>(MO.getGlobal());
  if (!F)
    return true;
  if (F->doesNotThrow())
    return false;
  // These functions never throw
  if (F->getName() == CxaBeginCatchFn || F->getName() == PersonalityWrapperFn ||
      F->getName() == StdTerminateFn)
    return false;

  // TODO Can we exclude call instructions that are marked as 'nounwind' in the
  // original LLVm IR? (Even when the callee may throw)
  return true;
}

const MachineOperand &Miden::getCalleeOp(const MachineInstr &MI) {
  switch (MI.getOpcode()) {
  case Miden::CALL:
  case Miden::CALL_S:
  case Miden::RET_CALL:
  case Miden::RET_CALL_S:
    return MI.getOperand(MI.getNumExplicitDefs());
  case Miden::CALL_INDIRECT:
  case Miden::CALL_INDIRECT_S:
  case Miden::RET_CALL_INDIRECT:
  case Miden::RET_CALL_INDIRECT_S:
    return MI.getOperand(MI.getNumExplicitOperands() - 1);
  default:
    llvm_unreachable("Not a call instruction");
  }
}

MCSymbolMiden *
Miden::getOrCreateFunctionTableSymbol(MCContext &Ctx,
                                      const MidenSubtarget *Subtarget) {
  StringRef Name = "__indirect_function_table";
  MCSymbolMiden *Sym = cast_or_null<MCSymbolMiden>(Ctx.lookupSymbol(Name));
  if (Sym) {
    if (!Sym->isFunctionTable())
      Ctx.reportError(SMLoc(), "symbol is not a miden funcref table");
  } else {
    Sym = cast<MCSymbolMiden>(Ctx.getOrCreateSymbol(Name));
    Sym->setFunctionTable();
    // The default function table is synthesized by the linker.
    Sym->setUndefined();
  }
  // MVP object files can't have symtab entries for tables.
  if (!(Subtarget && Subtarget->hasReferenceTypes()))
    Sym->setOmitFromLinkingSection();
  return Sym;
}

MCSymbolMiden *
Miden::getOrCreateFuncrefCallTableSymbol(MCContext &Ctx,
                                         const MidenSubtarget *Subtarget) {
  StringRef Name = "__funcref_call_table";
  MCSymbolMiden *Sym = cast_or_null<MCSymbolMiden>(Ctx.lookupSymbol(Name));
  if (Sym) {
    if (!Sym->isFunctionTable())
      Ctx.reportError(SMLoc(), "symbol is not a miden funcref table");
  } else {
    Sym = cast<MCSymbolMiden>(Ctx.getOrCreateSymbol(Name));

    // Setting Weak ensure only one table is left after linking when multiple
    // modules define the table.
    Sym->setWeak(true);

    miden::MidenLimits Limits = {0, 1, 1};
    miden::MidenTableType TableType = {miden::MIDEN_TYPE_FUNCREF, Limits};
    Sym->setType(miden::MIDEN_SYMBOL_TYPE_TABLE);
    Sym->setTableType(TableType);
  }
  // MVP object files can't have symtab entries for tables.
  if (!(Subtarget && Subtarget->hasReferenceTypes()))
    Sym->setOmitFromLinkingSection();
  return Sym;
}

// Find a catch instruction from an EH pad.
MachineInstr *Miden::findCatch(MachineBasicBlock *EHPad) {
  assert(EHPad->isEHPad());
  auto Pos = EHPad->begin();
  // Skip any label or debug instructions. Also skip 'end' marker instructions
  // that may exist after marker placement in CFGStackify.
  while (Pos != EHPad->end() &&
         (Pos->isLabel() || Pos->isDebugInstr() || isMarker(Pos->getOpcode())))
    Pos++;
  if (Pos != EHPad->end() && Miden::isCatch(Pos->getOpcode()))
    return &*Pos;
  return nullptr;
}

unsigned Miden::getCopyOpcodeForRegClass(const TargetRegisterClass *RC) {
  assert(RC != nullptr);
  switch (RC->getID()) {
  case Miden::I32RegClassID:
    return Miden::COPY_I32;
  case Miden::I64RegClassID:
    return Miden::COPY_I64;
  case Miden::F32RegClassID:
    return Miden::COPY_F32;
  case Miden::F64RegClassID:
    return Miden::COPY_F64;
  case Miden::V128RegClassID:
    return Miden::COPY_V128;
  case Miden::FUNCREFRegClassID:
    return Miden::COPY_FUNCREF;
  case Miden::EXTERNREFRegClassID:
    return Miden::COPY_EXTERNREF;
  default:
    llvm_unreachable("Unexpected register class");
  }
}
