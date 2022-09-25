//===- MidenTraits.h - DenseMap traits for the Miden structures ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides llvm::DenseMapInfo traits for the Miden structures.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_BINARYFORMAT_MIDENTRAITS_H
#define LLVM_BINARYFORMAT_MIDENTRAITS_H

#include "llvm/ADT/Hashing.h"
#include "llvm/BinaryFormat/Miden.h"

namespace llvm {

// Traits for using MidenSignature in a DenseMap.
template <> struct DenseMapInfo<miden::MidenSignature, void> {
  static miden::MidenSignature getEmptyKey() {
    miden::MidenSignature Sig;
    Sig.State = miden::MidenSignature::Empty;
    return Sig;
  }
  static miden::MidenSignature getTombstoneKey() {
    miden::MidenSignature Sig;
    Sig.State = miden::MidenSignature::Tombstone;
    return Sig;
  }
  static unsigned getHashValue(const miden::MidenSignature &Sig) {
    uintptr_t H = hash_value(Sig.State);
    for (auto Ret : Sig.Returns)
      H = hash_combine(H, Ret);
    for (auto Param : Sig.Params)
      H = hash_combine(H, Param);
    return H;
  }
  static bool isEqual(const miden::MidenSignature &LHS,
                      const miden::MidenSignature &RHS) {
    return LHS == RHS;
  }
};

// Traits for using MidenGlobalType in a DenseMap
template <> struct DenseMapInfo<miden::MidenGlobalType, void> {
  static miden::MidenGlobalType getEmptyKey() {
    return miden::MidenGlobalType{1, true};
  }
  static miden::MidenGlobalType getTombstoneKey() {
    return miden::MidenGlobalType{2, true};
  }
  static unsigned getHashValue(const miden::MidenGlobalType &GlobalType) {
    return hash_combine(GlobalType.Type, GlobalType.Mutable);
  }
  static bool isEqual(const miden::MidenGlobalType &LHS,
                      const miden::MidenGlobalType &RHS) {
    return LHS == RHS;
  }
};

// Traits for using MidenLimits in a DenseMap
template <> struct DenseMapInfo<miden::MidenLimits, void> {
  static miden::MidenLimits getEmptyKey() {
    return miden::MidenLimits{0xff, 0xff, 0xff};
  }
  static miden::MidenLimits getTombstoneKey() {
    return miden::MidenLimits{0xee, 0xee, 0xee};
  }
  static unsigned getHashValue(const miden::MidenLimits &Limits) {
    unsigned Hash = hash_value(Limits.Flags);
    Hash = hash_combine(Hash, Limits.Minimum);
    if (Limits.Flags & llvm::miden::MIDEN_LIMITS_FLAG_HAS_MAX) {
      Hash = hash_combine(Hash, Limits.Maximum);
    }
    return Hash;
  }
  static bool isEqual(const miden::MidenLimits &LHS,
                      const miden::MidenLimits &RHS) {
    return LHS == RHS;
  }
};

// Traits for using MidenTableType in a DenseMap
template <> struct DenseMapInfo<miden::MidenTableType, void> {
  static miden::MidenTableType getEmptyKey() {
    return miden::MidenTableType{
        0, DenseMapInfo<miden::MidenLimits, void>::getEmptyKey()};
  }
  static miden::MidenTableType getTombstoneKey() {
    return miden::MidenTableType{
        1, DenseMapInfo<miden::MidenLimits, void>::getTombstoneKey()};
  }
  static unsigned getHashValue(const miden::MidenTableType &TableType) {
    return hash_combine(
        TableType.ElemType,
        DenseMapInfo<miden::MidenLimits, void>::getHashValue(TableType.Limits));
  }
  static bool isEqual(const miden::MidenTableType &LHS,
                      const miden::MidenTableType &RHS) {
    return LHS == RHS;
  }
};

} // end namespace llvm

#endif // LLVM_BINARYFORMAT_MIDENTRAITS_H
