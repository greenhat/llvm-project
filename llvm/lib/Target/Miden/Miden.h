//===-- Miden.h - Top-level interface for Miden  ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the entry points for global functions defined in
/// the LLVM Miden back-end.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MIDEN_H
#define LLVM_LIB_TARGET_MIDEN_MIDEN_H

#include "llvm/PassRegistry.h"
#include "llvm/Support/CodeGen.h"

namespace llvm {

class MidenTargetMachine;
class ModulePass;
class FunctionPass;

// LLVM IR passes.
ModulePass *createMidenLowerEmscriptenEHSjLj();
ModulePass *createMidenAddMissingPrototypes();
ModulePass *createMidenFixFunctionBitcasts();
FunctionPass *createMidenOptimizeReturned();
FunctionPass *createMidenLowerRefTypesIntPtrConv();

// ISel and immediate followup passes.
FunctionPass *createMidenISelDag(MidenTargetMachine &TM,
                                 CodeGenOpt::Level OptLevel);
FunctionPass *createMidenArgumentMove();
FunctionPass *createMidenSetP2AlignOperands();

// Late passes.
FunctionPass *createMidenReplacePhysRegs();
FunctionPass *createMidenNullifyDebugValueLists();
FunctionPass *createMidenOptimizeLiveIntervals();
FunctionPass *createMidenMemIntrinsicResults();
FunctionPass *createMidenRegStackify();
FunctionPass *createMidenRegColoring();
FunctionPass *createMidenFixBrTableDefaults();
FunctionPass *createMidenFixIrreducibleControlFlow();
FunctionPass *createMidenLateEHPrepare();
FunctionPass *createMidenCFGSort();
FunctionPass *createMidenCFGStackify();
FunctionPass *createMidenExplicitLocals();
FunctionPass *createMidenLowerBrUnless();
FunctionPass *createMidenRegNumbering();
FunctionPass *createMidenDebugFixup();
FunctionPass *createMidenPeephole();
ModulePass *createMidenMCLowerPrePass();

// PassRegistry initialization declarations.
void initializeMidenAddMissingPrototypesPass(PassRegistry &);
void initializeMidenLowerEmscriptenEHSjLjPass(PassRegistry &);
void initializeFixFunctionBitcastsPass(PassRegistry &);
void initializeOptimizeReturnedPass(PassRegistry &);
void initializeMidenArgumentMovePass(PassRegistry &);
void initializeMidenSetP2AlignOperandsPass(PassRegistry &);
void initializeMidenReplacePhysRegsPass(PassRegistry &);
void initializeMidenNullifyDebugValueListsPass(PassRegistry &);
void initializeMidenOptimizeLiveIntervalsPass(PassRegistry &);
void initializeMidenMemIntrinsicResultsPass(PassRegistry &);
void initializeMidenRegStackifyPass(PassRegistry &);
void initializeMidenRegColoringPass(PassRegistry &);
void initializeMidenFixBrTableDefaultsPass(PassRegistry &);
void initializeMidenFixIrreducibleControlFlowPass(PassRegistry &);
void initializeMidenLateEHPreparePass(PassRegistry &);
void initializeMidenExceptionInfoPass(PassRegistry &);
void initializeMidenCFGSortPass(PassRegistry &);
void initializeMidenCFGStackifyPass(PassRegistry &);
void initializeMidenExplicitLocalsPass(PassRegistry &);
void initializeMidenLowerBrUnlessPass(PassRegistry &);
void initializeMidenRegNumberingPass(PassRegistry &);
void initializeMidenDebugFixupPass(PassRegistry &);
void initializeMidenPeepholePass(PassRegistry &);
void initializeMidenMCLowerPrePassPass(PassRegistry &);
void initializeMidenLowerRefTypesIntPtrConvPass(PassRegistry &);

namespace Miden {
enum TargetIndex {
  // Followed by a local index (ULEB).
  TI_LOCAL,
  // Followed by an absolute global index (ULEB). DEPRECATED.
  TI_GLOBAL_FIXED,
  // Followed by the index from the bottom of the Miden stack.
  TI_OPERAND_STACK,
  // Followed by a compilation unit relative global index (uint32_t)
  // that will have an associated relocation.
  TI_GLOBAL_RELOC,
  // Like TI_LOCAL, but indicates an indirect value (e.g. byval arg
  // passed by pointer).
  TI_LOCAL_INDIRECT
};
} // end namespace Miden

} // end namespace llvm

#endif
