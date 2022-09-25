//===--- llvm/CodeGen/MidenEHFuncInfo.h --------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Data structures for Miden exception handling schemes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_MIDENEHFUNCINFO_H
#define LLVM_CODEGEN_MIDENEHFUNCINFO_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace llvm {

class BasicBlock;
class Function;
class MachineBasicBlock;

namespace Miden {
enum Tag { CPP_EXCEPTION = 0, C_LONGJMP = 1 };
}

using BBOrMBB = PointerUnion<const BasicBlock *, MachineBasicBlock *>;

struct MidenEHFuncInfo {
  // When there is an entry <A, B>, if an exception is not caught by A, it
  // should next unwind to the EH pad B.
  DenseMap<BBOrMBB, BBOrMBB> SrcToUnwindDest;
  DenseMap<BBOrMBB, SmallPtrSet<BBOrMBB, 4>> UnwindDestToSrcs; // reverse map

  // Helper functions
  const BasicBlock *getUnwindDest(const BasicBlock *BB) const {
    assert(hasUnwindDest(BB));
    return SrcToUnwindDest.lookup(BB).get<const BasicBlock *>();
  }
  SmallPtrSet<const BasicBlock *, 4> getUnwindSrcs(const BasicBlock *BB) const {
    assert(hasUnwindSrcs(BB));
    const auto &Set = UnwindDestToSrcs.lookup(BB);
    SmallPtrSet<const BasicBlock *, 4> Ret;
    for (const auto P : Set)
      Ret.insert(P.get<const BasicBlock *>());
    return Ret;
  }
  void setUnwindDest(const BasicBlock *BB, const BasicBlock *Dest) {
    SrcToUnwindDest[BB] = Dest;
    if (!UnwindDestToSrcs.count(Dest))
      UnwindDestToSrcs[Dest] = SmallPtrSet<BBOrMBB, 4>();
    UnwindDestToSrcs[Dest].insert(BB);
  }
  bool hasUnwindDest(const BasicBlock *BB) const {
    return SrcToUnwindDest.count(BB);
  }
  bool hasUnwindSrcs(const BasicBlock *BB) const {
    return UnwindDestToSrcs.count(BB);
  }

  MachineBasicBlock *getUnwindDest(MachineBasicBlock *MBB) const {
    assert(hasUnwindDest(MBB));
    return SrcToUnwindDest.lookup(MBB).get<MachineBasicBlock *>();
  }
  SmallPtrSet<MachineBasicBlock *, 4>
  getUnwindSrcs(MachineBasicBlock *MBB) const {
    assert(hasUnwindSrcs(MBB));
    const auto &Set = UnwindDestToSrcs.lookup(MBB);
    SmallPtrSet<MachineBasicBlock *, 4> Ret;
    for (const auto P : Set)
      Ret.insert(P.get<MachineBasicBlock *>());
    return Ret;
  }
  void setUnwindDest(MachineBasicBlock *MBB, MachineBasicBlock *Dest) {
    SrcToUnwindDest[MBB] = Dest;
    if (!UnwindDestToSrcs.count(Dest))
      UnwindDestToSrcs[Dest] = SmallPtrSet<BBOrMBB, 4>();
    UnwindDestToSrcs[Dest].insert(MBB);
  }
  bool hasUnwindDest(MachineBasicBlock *MBB) const {
    return SrcToUnwindDest.count(MBB);
  }
  bool hasUnwindSrcs(MachineBasicBlock *MBB) const {
    return UnwindDestToSrcs.count(MBB);
  }
};

// Analyze the IR in the given function to build MidenEHFuncInfo.
void calculateMidenEHInfo(const Function *F, MidenEHFuncInfo &EHInfo);

} // namespace llvm

#endif // LLVM_CODEGEN_MIDENEHFUNCINFO_H
