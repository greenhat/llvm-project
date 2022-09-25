//===-- MidenTypeUtilities - Miden Type Utilities---*- C++ -*-====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the Miden-specific type parsing
/// utility functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_UTILS_MIDENTYPEUTILITIES_H
#define LLVM_LIB_TARGET_MIDEN_UTILS_MIDENTYPEUTILITIES_H

#include "llvm/ADT/Optional.h"
#include "llvm/BinaryFormat/Miden.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/MC/MCSymbolMiden.h"
#include "llvm/Support/MachineValueType.h"

namespace llvm {

class TargetRegisterClass;

namespace Miden {

/// Used as immediate MachineOperands for block signatures
enum class BlockType : unsigned {
  Invalid = 0x00,
  Void = 0x40,
  I32 = unsigned(miden::ValType::I32),
  I64 = unsigned(miden::ValType::I64),
  F32 = unsigned(miden::ValType::F32),
  F64 = unsigned(miden::ValType::F64),
  V128 = unsigned(miden::ValType::V128),
  Externref = unsigned(miden::ValType::EXTERNREF),
  Funcref = unsigned(miden::ValType::FUNCREF),
  // Multivalue blocks (and other non-void blocks) are only emitted when the
  // blocks will never be exited and are at the ends of functions (see
  // MidenCFGStackify::fixEndsAtEndOfFunction). They also are never made
  // to pop values off the stack, so the exact multivalue signature can always
  // be inferred from the return type of the parent function in MCInstLower.
  Multivalue = 0xffff,
};

enum MidenAddressSpace : unsigned {
  // Default address space, for pointers to linear memory (stack, heap, data).
  MIDEN_ADDRESS_SPACE_DEFAULT = 0,
  // A non-integral address space for pointers to named objects outside of
  // linear memory: Miden globals or Miden locals.  Loads and stores
  // to these pointers are lowered to global.get / global.set or local.get /
  // local.set, as appropriate.
  MIDEN_ADDRESS_SPACE_VAR = 1,
  // A non-integral address space for externref values
  MIDEN_ADDRESS_SPACE_EXTERNREF = 10,
  // A non-integral address space for funcref values
  MIDEN_ADDRESS_SPACE_FUNCREF = 20,
};

inline bool isDefaultAddressSpace(unsigned AS) {
  return AS == MIDEN_ADDRESS_SPACE_DEFAULT;
}
inline bool isMidenVarAddressSpace(unsigned AS) {
  return AS == MIDEN_ADDRESS_SPACE_VAR;
}
inline bool isValidAddressSpace(unsigned AS) {
  return isDefaultAddressSpace(AS) || isMidenVarAddressSpace(AS);
}
inline bool isFuncrefType(const Type *Ty) {
  return isa<PointerType>(Ty) &&
         Ty->getPointerAddressSpace() ==
             MidenAddressSpace::MIDEN_ADDRESS_SPACE_FUNCREF;
}
inline bool isExternrefType(const Type *Ty) {
  return isa<PointerType>(Ty) &&
         Ty->getPointerAddressSpace() ==
             MidenAddressSpace::MIDEN_ADDRESS_SPACE_EXTERNREF;
}
inline bool isRefType(const Type *Ty) {
  return isFuncrefType(Ty) || isExternrefType(Ty);
}

inline bool isRefType(miden::ValType Type) {
  return Type == miden::ValType::EXTERNREF || Type == miden::ValType::FUNCREF;
}

// Convert StringRef to ValType / HealType / BlockType

Optional<miden::ValType> parseType(StringRef Type);
BlockType parseBlockType(StringRef Type);
MVT parseMVT(StringRef Type);

// Convert ValType or a list/signature of ValTypes to a string.

// Convert an unsinged integer, which can be among miden::ValType enum, to its
// type name string. If the input is not within miden::ValType, returns
// "invalid_type".
const char *anyTypeToString(unsigned Type);
const char *typeToString(miden::ValType Type);
// Convert a list of ValTypes into a string in the format of
// "type0, type1, ... typeN"
std::string typeListToString(ArrayRef<miden::ValType> List);
// Convert a miden signature into a string in the format of
// "(params) -> (results)", where params and results are a string of ValType
// lists.
std::string signatureToString(const miden::MidenSignature *Sig);

// Convert a MVT into its corresponding miden ValType.
miden::ValType toValType(MVT Type);

// Convert a register class ID to a miden ValType.
miden::ValType regClassToValType(unsigned RC);

// Convert a register class to a miden ValType.
miden::ValType regClassToValType(const TargetRegisterClass *RC);

/// Sets a Miden Symbol Type.
void midenSymbolSetType(MCSymbolMiden *Sym, const Type *GlobalVT,
                        const SmallVector<MVT, 1> &VTs);

} // end namespace Miden
} // end namespace llvm

#endif
