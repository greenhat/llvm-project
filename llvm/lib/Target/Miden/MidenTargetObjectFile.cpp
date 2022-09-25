//===-- MidenTargetObjectFile.cpp - Miden Object Info ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines the functions of the Miden-specific subclass
/// of TargetLoweringObjectFile.
///
//===----------------------------------------------------------------------===//

#include "MidenTargetObjectFile.h"
#include "MidenTargetMachine.h"

using namespace llvm;

void MidenTargetObjectFile::Initialize(MCContext &Ctx,
                                       const TargetMachine &TM) {
  TargetLoweringObjectFileMiden::Initialize(Ctx, TM);
  InitializeMiden();
}
