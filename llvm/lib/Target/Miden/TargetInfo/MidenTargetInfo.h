//===-- MidenTargetInfo.h - Miden Target Impl -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file registers the Miden target.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_TARGETINFO_MIDENTARGETINFO_H
#define LLVM_LIB_TARGET_MIDEN_TARGETINFO_MIDENTARGETINFO_H

namespace llvm {

class Target;

Target &getTheMidenTarget32();
Target &getTheMidenTarget64();

namespace Miden {

int getStackOpcode(unsigned short Opcode);
int getRegisterOpcode(unsigned short Opcode);
int getMiden64Opcode(unsigned short Opcode);

} // namespace Miden

} // namespace llvm

#endif // LLVM_LIB_TARGET_MIDEN_TARGETINFO_MIDENTARGETINFO_H
