//===-- MidenTargetInfo.cpp - Miden Target Implementation -----===//
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

#include "TargetInfo/MidenTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

#define DEBUG_TYPE "miden-target-info"

Target &llvm::getTheMidenTarget32() {
  static Target TheMidenTarget32;
  return TheMidenTarget32;
}
Target &llvm::getTheMidenTarget64() {
  static Target TheMidenTarget64;
  return TheMidenTarget64;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeMidenTargetInfo() {
  RegisterTarget<Triple::miden> Y(getTheMidenTarget64(), "miden",
                                    "Miden 64-bit", "Miden");
}

// Defines llvm::Miden::getMiden64Opcode llvm::Miden::getStackOpcode
// which have to be in a shared location between CodeGen and MC.
#define GET_INSTRMAP_INFO 1
#define GET_INSTRINFO_ENUM 1
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "MidenGenInstrInfo.inc"
