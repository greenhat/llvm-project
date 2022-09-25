//==- MidenMCTargetDesc.h - Miden Target Descriptions -*- C++ -*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file provides Miden-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENMCTARGETDESC_H
#define LLVM_LIB_TARGET_MIDEN_MCTARGETDESC_MIDENMCTARGETDESC_H

#include "../MidenSubtarget.h"
#include "llvm/BinaryFormat/Miden.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/DataTypes.h"
#include <memory>

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCInstrInfo;
class MCObjectTargetWriter;
class Triple;

MCCodeEmitter *createMidenMCCodeEmitter(const MCInstrInfo &MCII);

MCAsmBackend *createMidenAsmBackend(const Triple &TT);

std::unique_ptr<MCObjectTargetWriter>
createMidenMidenObjectWriter(bool Is64Bit, bool IsEmscripten);

namespace Miden {
enum OperandType {
  /// Basic block label in a branch construct.
  OPERAND_BASIC_BLOCK = MCOI::OPERAND_FIRST_TARGET,
  /// Local index.
  OPERAND_LOCAL,
  /// Global index.
  OPERAND_GLOBAL,
  /// 32-bit integer immediates.
  OPERAND_I32IMM,
  /// 64-bit integer immediates.
  OPERAND_I64IMM,
  /// 32-bit floating-point immediates.
  OPERAND_F32IMM,
  /// 64-bit floating-point immediates.
  OPERAND_F64IMM,
  /// 8-bit vector lane immediate
  OPERAND_VEC_I8IMM,
  /// 16-bit vector lane immediate
  OPERAND_VEC_I16IMM,
  /// 32-bit vector lane immediate
  OPERAND_VEC_I32IMM,
  /// 64-bit vector lane immediate
  OPERAND_VEC_I64IMM,
  /// 32-bit unsigned function indices.
  OPERAND_FUNCTION32,
  /// 32-bit unsigned memory offsets.
  OPERAND_OFFSET32,
  /// 64-bit unsigned memory offsets.
  OPERAND_OFFSET64,
  /// p2align immediate for load and store address alignment.
  OPERAND_P2ALIGN,
  /// signature immediate for block/loop.
  OPERAND_SIGNATURE,
  /// type signature immediate for call_indirect.
  OPERAND_TYPEINDEX,
  /// Tag index.
  OPERAND_TAG,
  /// A list of branch targets for br_list.
  OPERAND_BRLIST,
  /// 32-bit unsigned table number.
  OPERAND_TABLE,
};
} // end namespace Miden

namespace MidenII {

/// Target Operand Flag enum.
enum TOF {
  MO_NO_FLAG = 0,

  // On a symbol operand this indicates that the immediate is a miden global
  // index.  The value of the miden global will be set to the symbol address at
  // runtime.  This adds a level of indirection similar to the GOT on native
  // platforms.
  MO_GOT,

  // Same as MO_GOT but the address stored in the global is a TLS address.
  MO_GOT_TLS,

  // On a symbol operand this indicates that the immediate is the symbol
  // address relative the __memory_base miden global.
  // Only applicable to data symbols.
  MO_MEMORY_BASE_REL,

  // On a symbol operand this indicates that the immediate is the symbol
  // address relative the __tls_base miden global.
  // Only applicable to data symbols.
  MO_TLS_BASE_REL,

  // On a symbol operand this indicates that the immediate is the symbol
  // address relative the __table_base miden global.
  // Only applicable to function symbols.
  MO_TABLE_BASE_REL,
};

} // end namespace MidenII

} // end namespace llvm

// Defines symbolic names for Miden registers. This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "MidenGenRegisterInfo.inc"

// Defines symbolic names for the Miden instructions.
//
#define GET_INSTRINFO_ENUM
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "MidenGenInstrInfo.inc"

namespace llvm {
namespace Miden {

/// Instruction opcodes emitted via means other than CodeGen.
static const unsigned Nop = 0x01;
static const unsigned End = 0x0b;

/// Return the default p2align value for a load or store with the given opcode.
inline unsigned GetDefaultP2AlignAny(unsigned Opc) {
  switch (Opc) {
#define MIDEN_LOAD_STORE(NAME)                                                 \
  case Miden::NAME##_A32:                                                      \
  case Miden::NAME##_A64:                                                      \
  case Miden::NAME##_A32_S:                                                    \
  case Miden::NAME##_A64_S:
    MIDEN_LOAD_STORE(LOAD8_S_I32)
    MIDEN_LOAD_STORE(LOAD8_U_I32)
    MIDEN_LOAD_STORE(LOAD8_S_I64)
    MIDEN_LOAD_STORE(LOAD8_U_I64)
    MIDEN_LOAD_STORE(ATOMIC_LOAD8_U_I32)
    MIDEN_LOAD_STORE(ATOMIC_LOAD8_U_I64)
    MIDEN_LOAD_STORE(STORE8_I32)
    MIDEN_LOAD_STORE(STORE8_I64)
    MIDEN_LOAD_STORE(ATOMIC_STORE8_I32)
    MIDEN_LOAD_STORE(ATOMIC_STORE8_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_ADD_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_ADD_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_SUB_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_SUB_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_AND_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_AND_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_OR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_OR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_XOR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_XOR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_XCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_XCHG_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_CMPXCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW8_U_CMPXCHG_I64)
    MIDEN_LOAD_STORE(LOAD8_SPLAT)
    MIDEN_LOAD_STORE(LOAD_LANE_I8x16)
    MIDEN_LOAD_STORE(STORE_LANE_I8x16)
    return 0;
    MIDEN_LOAD_STORE(LOAD16_S_I32)
    MIDEN_LOAD_STORE(LOAD16_U_I32)
    MIDEN_LOAD_STORE(LOAD16_S_I64)
    MIDEN_LOAD_STORE(LOAD16_U_I64)
    MIDEN_LOAD_STORE(ATOMIC_LOAD16_U_I32)
    MIDEN_LOAD_STORE(ATOMIC_LOAD16_U_I64)
    MIDEN_LOAD_STORE(STORE16_I32)
    MIDEN_LOAD_STORE(STORE16_I64)
    MIDEN_LOAD_STORE(ATOMIC_STORE16_I32)
    MIDEN_LOAD_STORE(ATOMIC_STORE16_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_ADD_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_ADD_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_SUB_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_SUB_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_AND_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_AND_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_OR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_OR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_XOR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_XOR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_XCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_XCHG_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_CMPXCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW16_U_CMPXCHG_I64)
    MIDEN_LOAD_STORE(LOAD16_SPLAT)
    MIDEN_LOAD_STORE(LOAD_LANE_I16x8)
    MIDEN_LOAD_STORE(STORE_LANE_I16x8)
    return 1;
    MIDEN_LOAD_STORE(LOAD_I32)
    MIDEN_LOAD_STORE(LOAD_F32)
    MIDEN_LOAD_STORE(STORE_I32)
    MIDEN_LOAD_STORE(STORE_F32)
    MIDEN_LOAD_STORE(LOAD32_S_I64)
    MIDEN_LOAD_STORE(LOAD32_U_I64)
    MIDEN_LOAD_STORE(STORE32_I64)
    MIDEN_LOAD_STORE(ATOMIC_LOAD_I32)
    MIDEN_LOAD_STORE(ATOMIC_LOAD32_U_I64)
    MIDEN_LOAD_STORE(ATOMIC_STORE_I32)
    MIDEN_LOAD_STORE(ATOMIC_STORE32_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_ADD_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_ADD_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_SUB_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_SUB_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_AND_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_AND_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_OR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_OR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_XOR_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_XOR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_XCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_XCHG_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_CMPXCHG_I32)
    MIDEN_LOAD_STORE(ATOMIC_RMW32_U_CMPXCHG_I64)
    MIDEN_LOAD_STORE(MEMORY_ATOMIC_NOTIFY)
    MIDEN_LOAD_STORE(MEMORY_ATOMIC_WAIT32)
    MIDEN_LOAD_STORE(LOAD32_SPLAT)
    MIDEN_LOAD_STORE(LOAD_ZERO_I32x4)
    MIDEN_LOAD_STORE(LOAD_LANE_I32x4)
    MIDEN_LOAD_STORE(STORE_LANE_I32x4)
    return 2;
    MIDEN_LOAD_STORE(LOAD_I64)
    MIDEN_LOAD_STORE(LOAD_F64)
    MIDEN_LOAD_STORE(STORE_I64)
    MIDEN_LOAD_STORE(STORE_F64)
    MIDEN_LOAD_STORE(ATOMIC_LOAD_I64)
    MIDEN_LOAD_STORE(ATOMIC_STORE_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_ADD_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_SUB_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_AND_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_OR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_XOR_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_XCHG_I64)
    MIDEN_LOAD_STORE(ATOMIC_RMW_CMPXCHG_I64)
    MIDEN_LOAD_STORE(MEMORY_ATOMIC_WAIT64)
    MIDEN_LOAD_STORE(LOAD64_SPLAT)
    MIDEN_LOAD_STORE(LOAD_EXTEND_S_I16x8)
    MIDEN_LOAD_STORE(LOAD_EXTEND_U_I16x8)
    MIDEN_LOAD_STORE(LOAD_EXTEND_S_I32x4)
    MIDEN_LOAD_STORE(LOAD_EXTEND_U_I32x4)
    MIDEN_LOAD_STORE(LOAD_EXTEND_S_I64x2)
    MIDEN_LOAD_STORE(LOAD_EXTEND_U_I64x2)
    MIDEN_LOAD_STORE(LOAD_ZERO_I64x2)
    MIDEN_LOAD_STORE(LOAD_LANE_I64x2)
    MIDEN_LOAD_STORE(STORE_LANE_I64x2)
    return 3;
    MIDEN_LOAD_STORE(LOAD_V128)
    MIDEN_LOAD_STORE(STORE_V128)
    return 4;
  default:
    return -1;
  }
#undef MIDEN_LOAD_STORE
}

inline unsigned GetDefaultP2Align(unsigned Opc) {
  auto Align = GetDefaultP2AlignAny(Opc);
  if (Align == -1U) {
    llvm_unreachable("Only loads and stores have p2align values");
  }
  return Align;
}

inline bool isArgument(unsigned Opc) {
  switch (Opc) {
  case Miden::ARGUMENT_i32:
  case Miden::ARGUMENT_i32_S:
  case Miden::ARGUMENT_i64:
  case Miden::ARGUMENT_i64_S:
  case Miden::ARGUMENT_f32:
  case Miden::ARGUMENT_f32_S:
  case Miden::ARGUMENT_f64:
  case Miden::ARGUMENT_f64_S:
  case Miden::ARGUMENT_v16i8:
  case Miden::ARGUMENT_v16i8_S:
  case Miden::ARGUMENT_v8i16:
  case Miden::ARGUMENT_v8i16_S:
  case Miden::ARGUMENT_v4i32:
  case Miden::ARGUMENT_v4i32_S:
  case Miden::ARGUMENT_v2i64:
  case Miden::ARGUMENT_v2i64_S:
  case Miden::ARGUMENT_v4f32:
  case Miden::ARGUMENT_v4f32_S:
  case Miden::ARGUMENT_v2f64:
  case Miden::ARGUMENT_v2f64_S:
  case Miden::ARGUMENT_funcref:
  case Miden::ARGUMENT_funcref_S:
  case Miden::ARGUMENT_externref:
  case Miden::ARGUMENT_externref_S:
    return true;
  default:
    return false;
  }
}

inline bool isCopy(unsigned Opc) {
  switch (Opc) {
  case Miden::COPY_I32:
  case Miden::COPY_I32_S:
  case Miden::COPY_I64:
  case Miden::COPY_I64_S:
  case Miden::COPY_F32:
  case Miden::COPY_F32_S:
  case Miden::COPY_F64:
  case Miden::COPY_F64_S:
  case Miden::COPY_V128:
  case Miden::COPY_V128_S:
  case Miden::COPY_FUNCREF:
  case Miden::COPY_FUNCREF_S:
  case Miden::COPY_EXTERNREF:
  case Miden::COPY_EXTERNREF_S:
    return true;
  default:
    return false;
  }
}

inline bool isTee(unsigned Opc) {
  switch (Opc) {
  case Miden::TEE_I32:
  case Miden::TEE_I32_S:
  case Miden::TEE_I64:
  case Miden::TEE_I64_S:
  case Miden::TEE_F32:
  case Miden::TEE_F32_S:
  case Miden::TEE_F64:
  case Miden::TEE_F64_S:
  case Miden::TEE_V128:
  case Miden::TEE_V128_S:
  case Miden::TEE_FUNCREF:
  case Miden::TEE_FUNCREF_S:
  case Miden::TEE_EXTERNREF:
  case Miden::TEE_EXTERNREF_S:
    return true;
  default:
    return false;
  }
}

inline bool isCallDirect(unsigned Opc) {
  switch (Opc) {
  case Miden::CALL:
  case Miden::CALL_S:
  case Miden::RET_CALL:
  case Miden::RET_CALL_S:
    return true;
  default:
    return false;
  }
}

inline bool isCallIndirect(unsigned Opc) {
  switch (Opc) {
  case Miden::CALL_INDIRECT:
  case Miden::CALL_INDIRECT_S:
  case Miden::RET_CALL_INDIRECT:
  case Miden::RET_CALL_INDIRECT_S:
    return true;
  default:
    return false;
  }
}

inline bool isBrTable(const MachineInstr &MI) {
  switch (MI.getOpcode()) {
  case Miden::BR_TABLE_I32:
  case Miden::BR_TABLE_I32_S:
  case Miden::BR_TABLE_I64:
  case Miden::BR_TABLE_I64_S:
    return true;
  default:
    return false;
  }
}

inline bool isMarker(unsigned Opc) {
  switch (Opc) {
  case Miden::BLOCK:
  case Miden::BLOCK_S:
  case Miden::END_BLOCK:
  case Miden::END_BLOCK_S:
  case Miden::LOOP:
  case Miden::LOOP_S:
  case Miden::END_LOOP:
  case Miden::END_LOOP_S:
  case Miden::TRY:
  case Miden::TRY_S:
  case Miden::END_TRY:
  case Miden::END_TRY_S:
    return true;
  default:
    return false;
  }
}

inline bool isCatch(unsigned Opc) {
  switch (Opc) {
  case Miden::CATCH:
  case Miden::CATCH_S:
  case Miden::CATCH_ALL:
  case Miden::CATCH_ALL_S:
    return true;
  default:
    return false;
  }
}

} // end namespace Miden
} // end namespace llvm

#endif
