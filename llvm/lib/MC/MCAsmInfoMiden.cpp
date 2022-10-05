//===-- MCAsmInfoMiden.cpp - Miden asm properties -----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines target asm properties related what form asm statements
// should take in general on Miden-based targets
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCAsmInfoMiden.h"
using namespace llvm;

void MCAsmInfoMiden::anchor() {}

MCAsmInfoMiden::MCAsmInfoMiden() {
  HasIdentDirective = true;
  HasNoDeadStrip = true;
  WeakRefDirective = "\t.weak\t";
  PrivateGlobalPrefix = ".L";
  PrivateLabelPrefix = ".L";
}
