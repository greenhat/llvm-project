// CodeGen/RuntimeLibcallSignatures.h - R.T. Lib. Call Signatures -*- C++ -*--//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file provides signature information for runtime libcalls.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_RUNTIME_LIBCALL_SIGNATURES_H
#define LLVM_LIB_TARGET_MIDEN_RUNTIME_LIBCALL_SIGNATURES_H

#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/RuntimeLibcalls.h"

namespace llvm {

class MidenSubtarget;

extern void getLibcallSignature(const MidenSubtarget &Subtarget,
                                RTLIB::Libcall LC,
                                SmallVectorImpl<miden::ValType> &Rets,
                                SmallVectorImpl<miden::ValType> &Params);

extern void getLibcallSignature(const MidenSubtarget &Subtarget, StringRef Name,
                                SmallVectorImpl<miden::ValType> &Rets,
                                SmallVectorImpl<miden::ValType> &Params);

} // end namespace llvm

#endif
