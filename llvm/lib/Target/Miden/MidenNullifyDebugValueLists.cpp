//=== MidenNullifyDebugValueLists.cpp - Nullify DBG_VALUE_LISTs   ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Nullify DBG_VALUE_LISTs instructions as a temporary measure before we
/// implement DBG_VALUE_LIST handling in MidenDebugValueManager.
/// See https://bugs.llvm.org/show_bug.cgi?id=50361.
/// TODO Correctly handle DBG_VALUE_LISTs
///
//===----------------------------------------------------------------------===//

#include "Miden.h"
#include "MidenSubtarget.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
using namespace llvm;

#define DEBUG_TYPE "miden-nullify-dbg-value-lists"

namespace {
class MidenNullifyDebugValueLists final : public MachineFunctionPass {
  StringRef getPassName() const override {
    return "Miden Nullify DBG_VALUE_LISTs";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID; // Pass identification, replacement for typeid
  MidenNullifyDebugValueLists() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char MidenNullifyDebugValueLists::ID = 0;
INITIALIZE_PASS(MidenNullifyDebugValueLists, DEBUG_TYPE,
                "Miden Nullify DBG_VALUE_LISTs", false, false)

FunctionPass *llvm::createMidenNullifyDebugValueLists() {
  return new MidenNullifyDebugValueLists();
}

bool MidenNullifyDebugValueLists::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** Nullify DBG_VALUE_LISTs **********\n"
                       "********** Function: "
                    << MF.getName() << '\n');
  const auto &TII = *MF.getSubtarget<MidenSubtarget>().getInstrInfo();
  SmallVector<MachineInstr *, 2> DbgValueLists;
  for (auto &MBB : MF)
    for (auto &MI : MBB)
      if (MI.getOpcode() == TargetOpcode::DBG_VALUE_LIST)
        DbgValueLists.push_back(&MI);

  // Our backend, including MidenDebugValueManager, currently cannot
  // handle DBG_VALUE_LISTs correctly. So this converts DBG_VALUE_LISTs to
  // "DBG_VALUE $noreg", which will appear as "optimized out".
  for (auto *DVL : DbgValueLists) {
    BuildMI(*DVL->getParent(), DVL, DVL->getDebugLoc(),
            TII.get(TargetOpcode::DBG_VALUE), false, Register(),
            DVL->getOperand(0).getMetadata(), DVL->getOperand(1).getMetadata());
    DVL->eraseFromParent();
  }

  return !DbgValueLists.empty();
}
