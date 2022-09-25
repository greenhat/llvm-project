//===-- MCMidenObjectTargetWriter.cpp - Miden Target Writer Subclass --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCMidenObjectWriter.h"

using namespace llvm;

MCMidenObjectTargetWriter::MCMidenObjectTargetWriter(bool Is64Bit,
                                                   bool IsEmscripten)
    : Is64Bit(Is64Bit), IsEmscripten(IsEmscripten) {}

// Pin the vtable to this object file
MCMidenObjectTargetWriter::~MCMidenObjectTargetWriter() = default;
