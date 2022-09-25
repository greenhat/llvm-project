//=== MidenLowerRefTypesIntPtrConv.cpp -
//                     Lower IntToPtr and PtrToInt on Reference Types   ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Lowers IntToPtr and PtrToInt instructions on reference types to
/// Trap instructions since they have been allowed to operate
/// on non-integral pointers.
///
//===----------------------------------------------------------------------===//

#include "Miden.h"
#include "MidenSubtarget.h"
#include "Utils/MidenTypeUtilities.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Pass.h"

using namespace llvm;

#define DEBUG_TYPE "miden-lower-reftypes-intptr-conv"

namespace {
class MidenLowerRefTypesIntPtrConv final : public FunctionPass {
  StringRef getPassName() const override {
    return "Miden Lower RefTypes Int-Ptr Conversions";
  }

  bool runOnFunction(Function &MF) override;

public:
  static char ID; // Pass identification
  MidenLowerRefTypesIntPtrConv() : FunctionPass(ID) {}
};
} // end anonymous namespace

char MidenLowerRefTypesIntPtrConv::ID = 0;
INITIALIZE_PASS(MidenLowerRefTypesIntPtrConv, DEBUG_TYPE,
                "Miden Lower RefTypes Int-Ptr Conversions", false, false)

FunctionPass *llvm::createMidenLowerRefTypesIntPtrConv() {
  return new MidenLowerRefTypesIntPtrConv();
}

bool MidenLowerRefTypesIntPtrConv::runOnFunction(Function &F) {
  LLVM_DEBUG(dbgs() << "********** Lower RefTypes IntPtr Convs **********\n"
                       "********** Function: "
                    << F.getName() << '\n');

  // This function will check for uses of ptrtoint and inttoptr on reference
  // types and replace them with a trap instruction.
  //
  // We replace the instruction by a trap instruction
  // and its uses by null in the case of inttoptr and 0 in the
  // case of ptrtoint.
  std::set<Instruction *> worklist;

  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
    PtrToIntInst *PTI = dyn_cast<PtrToIntInst>(&*I);
    IntToPtrInst *ITP = dyn_cast<IntToPtrInst>(&*I);
    if (!(PTI && Miden::isRefType(PTI->getPointerOperand()->getType())) &&
        !(ITP && Miden::isRefType(ITP->getDestTy())))
      continue;

    UndefValue *U = UndefValue::get(I->getType());
    I->replaceAllUsesWith(U);

    Function *TrapIntrin =
        Intrinsic::getDeclaration(F.getParent(), Intrinsic::debugtrap);
    CallInst::Create(TrapIntrin, {}, "", &*I);

    worklist.insert(&*I);
  }

  // erase each instruction replaced by trap
  for (Instruction *I : worklist)
    I->eraseFromParent();

  return !worklist.empty();
}
