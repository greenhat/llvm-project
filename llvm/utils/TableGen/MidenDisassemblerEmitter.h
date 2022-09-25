//===- MidenDisassemblerEmitter.h - Disassembler tables ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file is part of the Miden Disassembler Emitter.
// It contains the interface of the disassembler tables.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_MIDENDISASSEMBLEREMITTER_H
#define LLVM_UTILS_TABLEGEN_MIDENDISASSEMBLEREMITTER_H

#include "llvm/ADT/ArrayRef.h"

namespace llvm {

class CodeGenInstruction;
class raw_ostream;

void emitMidenDisassemblerTables(
    raw_ostream &OS,
    const ArrayRef<const CodeGenInstruction *> &NumberedInstructions);

} // namespace llvm

#endif
