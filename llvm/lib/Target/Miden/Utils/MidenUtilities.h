//===-- MidenUtilities - Miden Utility Functions ---*- C++ -*-====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the Miden-specific
/// utility functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_UTILS_MIDENUTILITIES_H
#define LLVM_LIB_TARGET_MIDEN_UTILS_MIDENUTILITIES_H

#include "llvm/Support/CommandLine.h"

namespace llvm {

class MachineBasicBlock;
class MachineInstr;
class MachineOperand;
class MCContext;
class MCSymbolMiden;
class TargetRegisterClass;
class MidenFunctionInfo;
class MidenSubtarget;

namespace Miden {

bool isChild(const MachineInstr &MI, const MidenFunctionInfo &MFI);
bool mayThrow(const MachineInstr &MI);

// Exception handling / setjmp-longjmp handling command-line options
extern cl::opt<bool> MidenEnableEmEH;   // asm.js-style EH
extern cl::opt<bool> MidenEnableEmSjLj; // asm.js-style SjLJ
extern cl::opt<bool> MidenEnableEH;     // EH using Miden EH instructions
extern cl::opt<bool> MidenEnableSjLj;   // SjLj using Miden EH instructions

// Exception-related function names
extern const char *const ClangCallTerminateFn;
extern const char *const CxaBeginCatchFn;
extern const char *const CxaRethrowFn;
extern const char *const StdTerminateFn;
extern const char *const PersonalityWrapperFn;

/// Returns the operand number of a callee, assuming the argument is a call
/// instruction.
const MachineOperand &getCalleeOp(const MachineInstr &MI);

/// Returns the __indirect_function_table, for use in call_indirect and in
/// function bitcasts.
MCSymbolMiden *getOrCreateFunctionTableSymbol(MCContext &Ctx,
                                              const MidenSubtarget *Subtarget);

/// Returns the __funcref_call_table, for use in funcref calls when lowered to
/// table.set + call_indirect.
MCSymbolMiden *
getOrCreateFuncrefCallTableSymbol(MCContext &Ctx,
                                  const MidenSubtarget *Subtarget);

/// Find a catch instruction from an EH pad. Returns null if no catch
/// instruction found or the catch is in an invalid location.
MachineInstr *findCatch(MachineBasicBlock *EHPad);

/// Returns the appropriate copy opcode for the given register class.
unsigned getCopyOpcodeForRegClass(const TargetRegisterClass *RC);

} // end namespace Miden

} // end namespace llvm

#endif
