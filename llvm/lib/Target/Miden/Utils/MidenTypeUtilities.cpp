//===-- MidenTypeUtilities.cpp - Miden Type Utility Functions -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements several utility functions for Miden type parsing.
///
//===----------------------------------------------------------------------===//

#include "MidenTypeUtilities.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

// Get register classes enum.
#define GET_REGINFO_ENUM
#include "MidenGenRegisterInfo.inc"

using namespace llvm;

Optional<miden::ValType> Miden::parseType(StringRef Type) {
  // FIXME: can't use StringSwitch because miden::ValType doesn't have a
  // "invalid" value.
  if (Type == "i32")
    return miden::ValType::I32;
  if (Type == "i64")
    return miden::ValType::I64;
  if (Type == "f32")
    return miden::ValType::F32;
  if (Type == "f64")
    return miden::ValType::F64;
  if (Type == "v128" || Type == "i8x16" || Type == "i16x8" || Type == "i32x4" ||
      Type == "i64x2" || Type == "f32x4" || Type == "f64x2")
    return miden::ValType::V128;
  if (Type == "funcref")
    return miden::ValType::FUNCREF;
  if (Type == "externref")
    return miden::ValType::EXTERNREF;
  return Optional<miden::ValType>();
}

Miden::BlockType Miden::parseBlockType(StringRef Type) {
  // Multivalue block types are handled separately in parseSignature
  return StringSwitch<Miden::BlockType>(Type)
      .Case("i32", Miden::BlockType::I32)
      .Case("i64", Miden::BlockType::I64)
      .Case("f32", Miden::BlockType::F32)
      .Case("f64", Miden::BlockType::F64)
      .Case("v128", Miden::BlockType::V128)
      .Case("funcref", Miden::BlockType::Funcref)
      .Case("externref", Miden::BlockType::Externref)
      .Case("void", Miden::BlockType::Void)
      .Default(Miden::BlockType::Invalid);
}

MVT Miden::parseMVT(StringRef Type) {
  return StringSwitch<MVT>(Type)
      .Case("i32", MVT::i32)
      .Case("i64", MVT::i64)
      .Case("f32", MVT::f32)
      .Case("f64", MVT::f64)
      .Case("i64", MVT::i64)
      .Case("v16i8", MVT::v16i8)
      .Case("v8i16", MVT::v8i16)
      .Case("v4i32", MVT::v4i32)
      .Case("v2i64", MVT::v2i64)
      .Case("funcref", MVT::funcref)
      .Case("externref", MVT::externref)
      .Default(MVT::INVALID_SIMPLE_VALUE_TYPE);
}

// We have various enums representing a subset of these types, use this
// function to convert any of them to text.
const char *Miden::anyTypeToString(unsigned Type) {
  switch (Type) {
  case miden::MIDEN_TYPE_I32:
    return "i32";
  case miden::MIDEN_TYPE_I64:
    return "i64";
  case miden::MIDEN_TYPE_F32:
    return "f32";
  case miden::MIDEN_TYPE_F64:
    return "f64";
  case miden::MIDEN_TYPE_V128:
    return "v128";
  case miden::MIDEN_TYPE_FUNCREF:
    return "funcref";
  case miden::MIDEN_TYPE_EXTERNREF:
    return "externref";
  case miden::MIDEN_TYPE_FUNC:
    return "func";
  case miden::MIDEN_TYPE_NORESULT:
    return "void";
  default:
    return "invalid_type";
  }
}

const char *Miden::typeToString(miden::ValType Type) {
  return anyTypeToString(static_cast<unsigned>(Type));
}

std::string Miden::typeListToString(ArrayRef<miden::ValType> List) {
  std::string S;
  for (const auto &Type : List) {
    if (&Type != &List[0])
      S += ", ";
    S += Miden::typeToString(Type);
  }
  return S;
}

std::string Miden::signatureToString(const miden::MidenSignature *Sig) {
  std::string S("(");
  S += typeListToString(Sig->Params);
  S += ") -> (";
  S += typeListToString(Sig->Returns);
  S += ")";
  return S;
}

miden::ValType Miden::toValType(MVT Type) {
  switch (Type.SimpleTy) {
  case MVT::i32:
    return miden::ValType::I32;
  case MVT::i64:
    return miden::ValType::I64;
  case MVT::f32:
    return miden::ValType::F32;
  case MVT::f64:
    return miden::ValType::F64;
  case MVT::v16i8:
  case MVT::v8i16:
  case MVT::v4i32:
  case MVT::v2i64:
  case MVT::v4f32:
  case MVT::v2f64:
    return miden::ValType::V128;
  case MVT::funcref:
    return miden::ValType::FUNCREF;
  case MVT::externref:
    return miden::ValType::EXTERNREF;
  default:
    llvm_unreachable("unexpected type");
  }
}

miden::ValType Miden::regClassToValType(unsigned RC) {
  switch (RC) {
  case Miden::I32RegClassID:
    return miden::ValType::I32;
  case Miden::I64RegClassID:
    return miden::ValType::I64;
  case Miden::F32RegClassID:
    return miden::ValType::F32;
  case Miden::F64RegClassID:
    return miden::ValType::F64;
  case Miden::V128RegClassID:
    return miden::ValType::V128;
  case Miden::FUNCREFRegClassID:
    return miden::ValType::FUNCREF;
  case Miden::EXTERNREFRegClassID:
    return miden::ValType::EXTERNREF;
  default:
    llvm_unreachable("unexpected type");
  }
}

miden::ValType Miden::regClassToValType(const TargetRegisterClass *RC) {
  assert(RC != nullptr);
  return regClassToValType(RC->getID());
}

void Miden::midenSymbolSetType(MCSymbolMiden *Sym, const Type *GlobalVT,
                               const SmallVector<MVT, 1> &VTs) {
  assert(!Sym->getType());

  // Tables are represented as Arrays in LLVM IR therefore
  // they reach this point as aggregate Array types with an element type
  // that is a reference type.
  miden::ValType ValTy;
  bool IsTable = false;
  if (GlobalVT->isArrayTy() &&
      Miden::isRefType(GlobalVT->getArrayElementType())) {
    IsTable = true;
    const Type *ElTy = GlobalVT->getArrayElementType();
    if (Miden::isExternrefType(ElTy))
      ValTy = miden::ValType::EXTERNREF;
    else if (Miden::isFuncrefType(ElTy))
      ValTy = miden::ValType::FUNCREF;
    else
      report_fatal_error("unhandled reference type");
  } else if (VTs.size() == 1) {
    ValTy = Miden::toValType(VTs[0]);
  } else
    report_fatal_error("Aggregate globals not yet implemented");

  if (IsTable) {
    Sym->setType(miden::MIDEN_SYMBOL_TYPE_TABLE);
    Sym->setTableType(ValTy);
  } else {
    Sym->setType(miden::MIDEN_SYMBOL_TYPE_GLOBAL);
    Sym->setGlobalType(
        miden::MidenGlobalType{uint8_t(ValTy), /*Mutable=*/true});
  }
}
