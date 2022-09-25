//===-- MidenMCAsmInfo.h - Miden asm properties -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the MidenMCAsmInfo class.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENMCASMINFO_H
#define LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENMCASMINFO_H

#include "llvm/MC/MCAsmInfoMiden.h"

namespace llvm {

class Triple;

class MidenMCAsmInfo final : public MCAsmInfoMiden {
public:
  explicit MidenMCAsmInfo(const Triple &T, const MCTargetOptions &Options);
  ~MidenMCAsmInfo() override;
};

} // end namespace llvm

#endif
