// MidenRegisterInfo.h - Miden Register Information Impl -*- C++ -*-
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the Miden implementation of the
/// MidenRegisterInfo class.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MIDENREGISTERINFO_H
#define LLVM_LIB_TARGET_MIDEN_MIDENREGISTERINFO_H

#define GET_REGINFO_HEADER
#include "MidenGenRegisterInfo.inc"

namespace llvm {

class MachineFunction;
class RegScavenger;
class TargetRegisterClass;
class Triple;

class MidenRegisterInfo final : public MidenGenRegisterInfo {
  const Triple &TT;

public:
  explicit MidenRegisterInfo(const Triple &TT);

  // Code Generation virtual methods.
  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;
  BitVector getReservedRegs(const MachineFunction &MF) const override;
  void eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  // Debug information queries.
  Register getFrameRegister(const MachineFunction &MF) const override;

  const TargetRegisterClass *
  getPointerRegClass(const MachineFunction &MF,
                     unsigned Kind = 0) const override;
  // This does not apply to miden.
  const uint32_t *getNoPreservedMask() const override { return nullptr; }
};

} // end namespace llvm

#endif
