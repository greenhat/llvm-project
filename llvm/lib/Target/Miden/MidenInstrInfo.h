//=- MidenInstrInfo.h - Miden Instruction Information -*- C++ -*-=//
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

#ifndef LLVM_LIB_TARGET_MIDEN_MIDENINSTRINFO_H
#define LLVM_LIB_TARGET_MIDEN_MIDENINSTRINFO_H

#include "MidenRegisterInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "MidenGenInstrInfo.inc"

#define GET_INSTRINFO_OPERAND_ENUM
#include "MidenGenInstrInfo.inc"

namespace llvm {

namespace Miden {

int16_t getNamedOperandIdx(uint16_t Opcode, uint16_t NamedIndex);

}

class MidenSubtarget;

class MidenInstrInfo final : public MidenGenInstrInfo {
  const MidenRegisterInfo RI;

public:
  explicit MidenInstrInfo(const MidenSubtarget &STI);

  const MidenRegisterInfo &getRegisterInfo() const { return RI; }

  bool isReallyTriviallyReMaterializable(const MachineInstr &MI) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                   const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg,
                   bool KillSrc) const override;
  MachineInstr *commuteInstructionImpl(MachineInstr &MI, bool NewMI,
                                       unsigned OpIdx1,
                                       unsigned OpIdx2) const override;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify = false) const override;
  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;
  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;
  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  ArrayRef<std::pair<int, const char *>>
  getSerializableTargetIndices() const override;

  const MachineOperand &getCalleeOperand(const MachineInstr &MI) const override;
};

} // end namespace llvm

#endif
