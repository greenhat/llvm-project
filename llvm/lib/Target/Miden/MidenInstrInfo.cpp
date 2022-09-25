//===-- MidenInstrInfo.cpp - Miden Instruction Information ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the Miden implementation of the
/// TargetInstrInfo class.
///
//===----------------------------------------------------------------------===//

#include "MidenInstrInfo.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "Miden.h"
#include "MidenMachineFunctionInfo.h"
#include "MidenSubtarget.h"
#include "Utils/MidenUtilities.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
using namespace llvm;

#define DEBUG_TYPE "miden-instr-info"

#define GET_INSTRINFO_CTOR_DTOR
#include "MidenGenInstrInfo.inc"

// defines Miden::getNamedOperandIdx
#define GET_INSTRINFO_NAMED_OPS
#include "MidenGenInstrInfo.inc"

MidenInstrInfo::MidenInstrInfo(const MidenSubtarget &STI)
    : MidenGenInstrInfo(Miden::ADJCALLSTACKDOWN, Miden::ADJCALLSTACKUP,
                        Miden::CATCHRET),
      RI(STI.getTargetTriple()) {}

bool MidenInstrInfo::isReallyTriviallyReMaterializable(
    const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case Miden::CONST_I32:
  case Miden::CONST_I64:
  case Miden::CONST_F32:
  case Miden::CONST_F64:
    // isReallyTriviallyReMaterializableGeneric misses these because of the
    // ARGUMENTS implicit def, so we manualy override it here.
    return true;
  default:
    return false;
  }
}

void MidenInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I,
                                 const DebugLoc &DL, MCRegister DestReg,
                                 MCRegister SrcReg, bool KillSrc) const {
  // This method is called by post-RA expansion, which expects only pregs to
  // exist. However we need to handle both here.
  auto &MRI = MBB.getParent()->getRegInfo();
  const TargetRegisterClass *RC =
      Register::isVirtualRegister(DestReg)
          ? MRI.getRegClass(DestReg)
          : MRI.getTargetRegisterInfo()->getMinimalPhysRegClass(DestReg);

  unsigned CopyOpcode = Miden::getCopyOpcodeForRegClass(RC);

  BuildMI(MBB, I, DL, get(CopyOpcode), DestReg)
      .addReg(SrcReg, KillSrc ? RegState::Kill : 0);
}

MachineInstr *MidenInstrInfo::commuteInstructionImpl(MachineInstr &MI,
                                                     bool NewMI,
                                                     unsigned OpIdx1,
                                                     unsigned OpIdx2) const {
  // If the operands are stackified, we can't reorder them.
  MidenFunctionInfo &MFI =
      *MI.getParent()->getParent()->getInfo<MidenFunctionInfo>();
  if (MFI.isVRegStackified(MI.getOperand(OpIdx1).getReg()) ||
      MFI.isVRegStackified(MI.getOperand(OpIdx2).getReg()))
    return nullptr;

  // Otherwise use the default implementation.
  return TargetInstrInfo::commuteInstructionImpl(MI, NewMI, OpIdx1, OpIdx2);
}

// Branch analysis.
bool MidenInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool /*AllowModify*/) const {
  const auto &MFI = *MBB.getParent()->getInfo<MidenFunctionInfo>();
  // Miden has control flow that doesn't have explicit branches or direct
  // fallthrough (e.g. try/catch), which can't be modeled by analyzeBranch. It
  // is created after CFGStackify.
  if (MFI.isCFGStackified())
    return true;

  bool HaveCond = false;
  for (MachineInstr &MI : MBB.terminators()) {
    switch (MI.getOpcode()) {
    default:
      // Unhandled instruction; bail out.
      return true;
    case Miden::BR_IF:
      if (HaveCond)
        return true;
      Cond.push_back(MachineOperand::CreateImm(true));
      Cond.push_back(MI.getOperand(1));
      TBB = MI.getOperand(0).getMBB();
      HaveCond = true;
      break;
    case Miden::BR_UNLESS:
      if (HaveCond)
        return true;
      Cond.push_back(MachineOperand::CreateImm(false));
      Cond.push_back(MI.getOperand(1));
      TBB = MI.getOperand(0).getMBB();
      HaveCond = true;
      break;
    case Miden::BR:
      if (!HaveCond)
        TBB = MI.getOperand(0).getMBB();
      else
        FBB = MI.getOperand(0).getMBB();
      break;
    }
    if (MI.isBarrier())
      break;
  }

  return false;
}

unsigned MidenInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                      int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  MachineBasicBlock::instr_iterator I = MBB.instr_end();
  unsigned Count = 0;

  while (I != MBB.instr_begin()) {
    --I;
    if (I->isDebugInstr())
      continue;
    if (!I->isTerminator())
      break;
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.instr_end();
    ++Count;
  }

  return Count;
}

unsigned MidenInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
    if (!TBB)
      return 0;

    BuildMI(&MBB, DL, get(Miden::BR)).addMBB(TBB);
    return 1;
  }

  assert(Cond.size() == 2 && "Expected a flag and a successor block");

  if (Cond[0].getImm())
    BuildMI(&MBB, DL, get(Miden::BR_IF)).addMBB(TBB).add(Cond[1]);
  else
    BuildMI(&MBB, DL, get(Miden::BR_UNLESS)).addMBB(TBB).add(Cond[1]);
  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(Miden::BR)).addMBB(FBB);
  return 2;
}

bool MidenInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 2 && "Expected a flag and a condition expression");
  Cond.front() = MachineOperand::CreateImm(!Cond.front().getImm());
  return false;
}

ArrayRef<std::pair<int, const char *>>
MidenInstrInfo::getSerializableTargetIndices() const {
  static const std::pair<int, const char *> TargetIndices[] = {
      {Miden::TI_LOCAL, "miden-local"},
      {Miden::TI_GLOBAL_FIXED, "miden-global-fixed"},
      {Miden::TI_OPERAND_STACK, "miden-operand-stack"},
      {Miden::TI_GLOBAL_RELOC, "miden-global-reloc"},
      {Miden::TI_LOCAL_INDIRECT, "miden-local-indirect"}};
  return makeArrayRef(TargetIndices);
}

const MachineOperand &
MidenInstrInfo::getCalleeOperand(const MachineInstr &MI) const {
  return Miden::getCalleeOp(MI);
}
