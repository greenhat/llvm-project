//===-- MidenPeephole.cpp - Miden Peephole Optimiztions -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Late peephole optimizations for Miden.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "Miden.h"
#include "MidenMachineFunctionInfo.h"
#include "MidenSubtarget.h"
#include "Utils/MidenUtilities.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
using namespace llvm;

#define DEBUG_TYPE "miden-peephole"

static cl::opt<bool> DisableMidenFallthroughReturnOpt(
    "disable-miden-fallthrough-return-opt", cl::Hidden,
    cl::desc("Miden: Disable fallthrough-return optimizations."),
    cl::init(false));

namespace {
class MidenPeephole final : public MachineFunctionPass {
  StringRef getPassName() const override {
    return "Miden late peephole optimizer";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addRequired<TargetLibraryInfoWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID;
  MidenPeephole() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char MidenPeephole::ID = 0;
INITIALIZE_PASS(MidenPeephole, DEBUG_TYPE, "Miden peephole optimizations",
                false, false)

FunctionPass *llvm::createMidenPeephole() { return new MidenPeephole(); }

/// If desirable, rewrite NewReg to a drop register.
static bool maybeRewriteToDrop(unsigned OldReg, unsigned NewReg,
                               MachineOperand &MO, MidenFunctionInfo &MFI,
                               MachineRegisterInfo &MRI) {
  bool Changed = false;
  if (OldReg == NewReg) {
    Changed = true;
    Register NewReg = MRI.createVirtualRegister(MRI.getRegClass(OldReg));
    MO.setReg(NewReg);
    MO.setIsDead();
    MFI.stackifyVReg(MRI, NewReg);
  }
  return Changed;
}

static bool maybeRewriteToFallthrough(MachineInstr &MI, MachineBasicBlock &MBB,
                                      const MachineFunction &MF,
                                      MidenFunctionInfo &MFI,
                                      MachineRegisterInfo &MRI,
                                      const MidenInstrInfo &TII) {
  if (DisableMidenFallthroughReturnOpt)
    return false;
  if (&MBB != &MF.back())
    return false;

  MachineBasicBlock::iterator End = MBB.end();
  --End;
  assert(End->getOpcode() == Miden::END_FUNCTION);
  --End;
  if (&MI != &*End)
    return false;

  for (auto &MO : MI.explicit_operands()) {
    // If the operand isn't stackified, insert a COPY to read the operands and
    // stackify them.
    Register Reg = MO.getReg();
    if (!MFI.isVRegStackified(Reg)) {
      unsigned CopyLocalOpc;
      const TargetRegisterClass *RegClass = MRI.getRegClass(Reg);
      CopyLocalOpc = Miden::getCopyOpcodeForRegClass(RegClass);
      Register NewReg = MRI.createVirtualRegister(RegClass);
      BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(CopyLocalOpc), NewReg)
          .addReg(Reg);
      MO.setReg(NewReg);
      MFI.stackifyVReg(MRI, NewReg);
    }
  }

  MI.setDesc(TII.get(Miden::FALLTHROUGH_RETURN));
  return true;
}

bool MidenPeephole::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG({
    dbgs() << "********** Peephole **********\n"
           << "********** Function: " << MF.getName() << '\n';
  });

  MachineRegisterInfo &MRI = MF.getRegInfo();
  MidenFunctionInfo &MFI = *MF.getInfo<MidenFunctionInfo>();
  const auto &TII = *MF.getSubtarget<MidenSubtarget>().getInstrInfo();
  const MidenTargetLowering &TLI =
      *MF.getSubtarget<MidenSubtarget>().getTargetLowering();
  auto &LibInfo =
      getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(MF.getFunction());
  bool Changed = false;

  for (auto &MBB : MF)
    for (auto &MI : MBB)
      switch (MI.getOpcode()) {
      default:
        break;
      case Miden::CALL: {
        MachineOperand &Op1 = MI.getOperand(1);
        if (Op1.isSymbol()) {
          StringRef Name(Op1.getSymbolName());
          if (Name == TLI.getLibcallName(RTLIB::MEMCPY) ||
              Name == TLI.getLibcallName(RTLIB::MEMMOVE) ||
              Name == TLI.getLibcallName(RTLIB::MEMSET)) {
            LibFunc Func;
            if (LibInfo.getLibFunc(Name, Func)) {
              const auto &Op2 = MI.getOperand(2);
              if (!Op2.isReg())
                report_fatal_error("Peephole: call to builtin function with "
                                   "wrong signature, not consuming reg");
              MachineOperand &MO = MI.getOperand(0);
              Register OldReg = MO.getReg();
              Register NewReg = Op2.getReg();

              if (MRI.getRegClass(NewReg) != MRI.getRegClass(OldReg))
                report_fatal_error("Peephole: call to builtin function with "
                                   "wrong signature, from/to mismatch");
              Changed |= maybeRewriteToDrop(OldReg, NewReg, MO, MFI, MRI);
            }
          }
        }
        break;
      }
      // Optimize away an explicit void return at the end of the function.
      case Miden::RETURN:
        Changed |= maybeRewriteToFallthrough(MI, MBB, MF, MFI, MRI, TII);
        break;
      }

  return Changed;
}
