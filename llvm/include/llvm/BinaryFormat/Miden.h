//===- Miden.h - Miden object file format -------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines manifest constants for the miden object file format.
// See: https://github.com/WebAssembly/design/blob/main/BinaryEncoding.md
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_BINARYFORMAT_MIDEN_H
#define LLVM_BINARYFORMAT_MIDEN_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
namespace miden {

// Object file magic string.
const char MidenMagic[] = {'\0', 'a', 's', 'm'};
// Miden binary format version
const uint32_t MidenVersion = 0x1;
// Miden linking metadata version
const uint32_t MidenMetadataVersion = 0x2;
// Miden uses a 64k page size
const uint32_t MidenPageSize = 65536;

struct MidenObjectHeader {
  StringRef Magic;
  uint32_t Version;
};

struct MidenDylinkImportInfo {
  StringRef Module;
  StringRef Field;
  uint32_t Flags;
};

struct MidenDylinkExportInfo {
  StringRef Name;
  uint32_t Flags;
};

struct MidenDylinkInfo {
  uint32_t MemorySize;           // Memory size in bytes
  uint32_t MemoryAlignment;      // P2 alignment of memory
  uint32_t TableSize;            // Table size in elements
  uint32_t TableAlignment;       // P2 alignment of table
  std::vector<StringRef> Needed; // Shared library dependencies
  std::vector<MidenDylinkImportInfo> ImportInfo;
  std::vector<MidenDylinkExportInfo> ExportInfo;
};

struct MidenProducerInfo {
  std::vector<std::pair<std::string, std::string>> Languages;
  std::vector<std::pair<std::string, std::string>> Tools;
  std::vector<std::pair<std::string, std::string>> SDKs;
};

struct MidenFeatureEntry {
  uint8_t Prefix;
  std::string Name;
};

struct MidenExport {
  StringRef Name;
  uint8_t Kind;
  uint32_t Index;
};

struct MidenLimits {
  uint8_t Flags;
  uint64_t Minimum;
  uint64_t Maximum;
};

struct MidenTableType {
  uint8_t ElemType;
  MidenLimits Limits;
};

struct MidenTable {
  uint32_t Index;
  MidenTableType Type;
  StringRef SymbolName; // from the "linking" section
};

struct MidenInitExprMVP {
  uint8_t Opcode;
  union {
    int32_t Int32;
    int64_t Int64;
    uint32_t Float32;
    uint64_t Float64;
    uint32_t Global;
  } Value;
};

struct MidenInitExpr {
  uint8_t Extended; // Set to non-zero if extended const is used (i.e. more than
                    // one instruction)
  MidenInitExprMVP Inst;
  ArrayRef<uint8_t> Body;
};

struct MidenGlobalType {
  uint8_t Type;
  bool Mutable;
};

struct MidenGlobal {
  uint32_t Index;
  MidenGlobalType Type;
  MidenInitExpr InitExpr;
  StringRef SymbolName; // from the "linking" section
};

struct MidenTag {
  uint32_t Index;
  uint32_t SigIndex;
  StringRef SymbolName; // from the "linking" section
};

struct MidenImport {
  StringRef Module;
  StringRef Field;
  uint8_t Kind;
  union {
    uint32_t SigIndex;
    MidenGlobalType Global;
    MidenTableType Table;
    MidenLimits Memory;
  };
};

struct MidenLocalDecl {
  uint8_t Type;
  uint32_t Count;
};

struct MidenFunction {
  uint32_t Index;
  uint32_t SigIndex;
  std::vector<MidenLocalDecl> Locals;
  ArrayRef<uint8_t> Body;
  uint32_t CodeSectionOffset;
  uint32_t Size;
  uint32_t CodeOffset;            // start of Locals and Body
  Optional<StringRef> ExportName; // from the "export" section
  StringRef SymbolName;           // from the "linking" section
  StringRef DebugName;            // from the "name" section
  uint32_t Comdat;                // from the "comdat info" section
};

struct MidenDataSegment {
  uint32_t InitFlags;
  // Present if InitFlags & MIDEN_DATA_SEGMENT_HAS_MEMINDEX.
  uint32_t MemoryIndex;
  // Present if InitFlags & MIDEN_DATA_SEGMENT_IS_PASSIVE == 0.
  MidenInitExpr Offset;

  ArrayRef<uint8_t> Content;
  StringRef Name; // from the "segment info" section
  uint32_t Alignment;
  uint32_t LinkingFlags;
  uint32_t Comdat; // from the "comdat info" section
};

struct MidenElemSegment {
  uint32_t Flags;
  uint32_t TableNumber;
  uint8_t ElemKind;
  MidenInitExpr Offset;
  std::vector<uint32_t> Functions;
};

// Represents the location of a Miden data symbol within a MidenDataSegment, as
// the index of the segment, and the offset and size within the segment.
struct MidenDataReference {
  uint32_t Segment;
  uint64_t Offset;
  uint64_t Size;
};

struct MidenRelocation {
  uint8_t Type;    // The type of the relocation.
  uint32_t Index;  // Index into either symbol or type index space.
  uint64_t Offset; // Offset from the start of the section.
  int64_t Addend;  // A value to add to the symbol.
};

struct MidenInitFunc {
  uint32_t Priority;
  uint32_t Symbol;
};

struct MidenSymbolInfo {
  StringRef Name;
  uint8_t Kind;
  uint32_t Flags;
  // For undefined symbols the module of the import
  Optional<StringRef> ImportModule;
  // For undefined symbols the name of the import
  Optional<StringRef> ImportName;
  // For symbols to be exported from the final module
  Optional<StringRef> ExportName;
  union {
    // For function, table, or global symbols, the index in function, table, or
    // global index space.
    uint32_t ElementIndex;
    // For a data symbols, the address of the data relative to segment.
    MidenDataReference DataRef;
  };
};

enum class NameType {
  FUNCTION,
  GLOBAL,
  DATA_SEGMENT,
};

struct MidenDebugName {
  NameType Type;
  uint32_t Index;
  StringRef Name;
};

struct MidenLinkingData {
  uint32_t Version;
  std::vector<MidenInitFunc> InitFunctions;
  std::vector<StringRef> Comdats;
  std::vector<MidenSymbolInfo> SymbolTable;
};

enum : unsigned {
  MIDEN_SEC_CUSTOM = 0,     // Custom / User-defined section
  MIDEN_SEC_TYPE = 1,       // Function signature declarations
  MIDEN_SEC_IMPORT = 2,     // Import declarations
  MIDEN_SEC_FUNCTION = 3,   // Function declarations
  MIDEN_SEC_TABLE = 4,      // Indirect function table and other tables
  MIDEN_SEC_MEMORY = 5,     // Memory attributes
  MIDEN_SEC_GLOBAL = 6,     // Global declarations
  MIDEN_SEC_EXPORT = 7,     // Exports
  MIDEN_SEC_START = 8,      // Start function declaration
  MIDEN_SEC_ELEM = 9,       // Elements section
  MIDEN_SEC_CODE = 10,      // Function bodies (code)
  MIDEN_SEC_DATA = 11,      // Data segments
  MIDEN_SEC_DATACOUNT = 12, // Data segment count
  MIDEN_SEC_TAG = 13,       // Tag declarations
  MIDEN_SEC_LAST_KNOWN = MIDEN_SEC_TAG,
};

// Type immediate encodings used in various contexts.
enum : unsigned {
  MIDEN_TYPE_I32 = 0x7F,
  MIDEN_TYPE_I64 = 0x7E,
  MIDEN_TYPE_F32 = 0x7D,
  MIDEN_TYPE_F64 = 0x7C,
  MIDEN_TYPE_V128 = 0x7B,
  MIDEN_TYPE_FUNCREF = 0x70,
  MIDEN_TYPE_EXTERNREF = 0x6F,
  MIDEN_TYPE_FUNC = 0x60,
  MIDEN_TYPE_NORESULT = 0x40, // for blocks with no result values
};

// Kinds of externals (for imports and exports).
enum : unsigned {
  MIDEN_EXTERNAL_FUNCTION = 0x0,
  MIDEN_EXTERNAL_TABLE = 0x1,
  MIDEN_EXTERNAL_MEMORY = 0x2,
  MIDEN_EXTERNAL_GLOBAL = 0x3,
  MIDEN_EXTERNAL_TAG = 0x4,
};

// Opcodes used in initializer expressions.
enum : unsigned {
  MIDEN_OPCODE_END = 0x0b,
  MIDEN_OPCODE_CALL = 0x10,
  MIDEN_OPCODE_LOCAL_GET = 0x20,
  MIDEN_OPCODE_LOCAL_SET = 0x21,
  MIDEN_OPCODE_LOCAL_TEE = 0x22,
  MIDEN_OPCODE_GLOBAL_GET = 0x23,
  MIDEN_OPCODE_GLOBAL_SET = 0x24,
  MIDEN_OPCODE_I32_STORE = 0x36,
  MIDEN_OPCODE_I64_STORE = 0x37,
  MIDEN_OPCODE_I32_CONST = 0x41,
  MIDEN_OPCODE_I64_CONST = 0x42,
  MIDEN_OPCODE_F32_CONST = 0x43,
  MIDEN_OPCODE_F64_CONST = 0x44,
  MIDEN_OPCODE_I32_ADD = 0x6a,
  MIDEN_OPCODE_I32_SUB = 0x6b,
  MIDEN_OPCODE_I32_MUL = 0x6c,
  MIDEN_OPCODE_I64_ADD = 0x7c,
  MIDEN_OPCODE_I64_SUB = 0x7d,
  MIDEN_OPCODE_I64_MUL = 0x7e,
  MIDEN_OPCODE_REF_NULL = 0xd0,
};

// Opcodes used in synthetic functions.
enum : unsigned {
  MIDEN_OPCODE_BLOCK = 0x02,
  MIDEN_OPCODE_BR = 0x0c,
  MIDEN_OPCODE_BR_TABLE = 0x0e,
  MIDEN_OPCODE_RETURN = 0x0f,
  MIDEN_OPCODE_DROP = 0x1a,
  MIDEN_OPCODE_MISC_PREFIX = 0xfc,
  MIDEN_OPCODE_MEMORY_INIT = 0x08,
  MIDEN_OPCODE_MEMORY_FILL = 0x0b,
  MIDEN_OPCODE_DATA_DROP = 0x09,
  MIDEN_OPCODE_ATOMICS_PREFIX = 0xfe,
  MIDEN_OPCODE_ATOMIC_NOTIFY = 0x00,
  MIDEN_OPCODE_I32_ATOMIC_WAIT = 0x01,
  MIDEN_OPCODE_I32_ATOMIC_STORE = 0x17,
  MIDEN_OPCODE_I32_RMW_CMPXCHG = 0x48,
};

enum : unsigned {
  MIDEN_LIMITS_FLAG_NONE = 0x0,
  MIDEN_LIMITS_FLAG_HAS_MAX = 0x1,
  MIDEN_LIMITS_FLAG_IS_SHARED = 0x2,
  MIDEN_LIMITS_FLAG_IS_64 = 0x4,
};

enum : unsigned {
  MIDEN_DATA_SEGMENT_IS_PASSIVE = 0x01,
  MIDEN_DATA_SEGMENT_HAS_MEMINDEX = 0x02,
};

enum : unsigned {
  MIDEN_ELEM_SEGMENT_IS_PASSIVE = 0x01,
  MIDEN_ELEM_SEGMENT_HAS_TABLE_NUMBER = 0x02,
  MIDEN_ELEM_SEGMENT_HAS_INIT_EXPRS = 0x04,
};
const unsigned MIDEN_ELEM_SEGMENT_MASK_HAS_ELEM_KIND = 0x3;

// Feature policy prefixes used in the custom "target_features" section
enum : uint8_t {
  MIDEN_FEATURE_PREFIX_USED = '+',
  MIDEN_FEATURE_PREFIX_REQUIRED = '=',
  MIDEN_FEATURE_PREFIX_DISALLOWED = '-',
};

// Kind codes used in the custom "name" section
enum : unsigned {
  MIDEN_NAMES_FUNCTION = 1,
  MIDEN_NAMES_LOCAL = 2,
  MIDEN_NAMES_GLOBAL = 7,
  MIDEN_NAMES_DATA_SEGMENT = 9,
};

// Kind codes used in the custom "linking" section
enum : unsigned {
  MIDEN_SEGMENT_INFO = 0x5,
  MIDEN_INIT_FUNCS = 0x6,
  MIDEN_COMDAT_INFO = 0x7,
  MIDEN_SYMBOL_TABLE = 0x8,
};

// Kind codes used in the custom "dylink" section
enum : unsigned {
  MIDEN_DYLINK_MEM_INFO = 0x1,
  MIDEN_DYLINK_NEEDED = 0x2,
  MIDEN_DYLINK_EXPORT_INFO = 0x3,
  MIDEN_DYLINK_IMPORT_INFO = 0x4,
};

// Kind codes used in the custom "linking" section in the MIDEN_COMDAT_INFO
enum : unsigned {
  MIDEN_COMDAT_DATA = 0x0,
  MIDEN_COMDAT_FUNCTION = 0x1,
  // GLOBAL, TAG, and TABLE are in here but LLVM doesn't use them yet.
  MIDEN_COMDAT_SECTION = 0x5,
};

// Kind codes used in the custom "linking" section in the MIDEN_SYMBOL_TABLE
enum MidenSymbolType : unsigned {
  MIDEN_SYMBOL_TYPE_FUNCTION = 0x0,
  MIDEN_SYMBOL_TYPE_DATA = 0x1,
  MIDEN_SYMBOL_TYPE_GLOBAL = 0x2,
  MIDEN_SYMBOL_TYPE_SECTION = 0x3,
  MIDEN_SYMBOL_TYPE_TAG = 0x4,
  MIDEN_SYMBOL_TYPE_TABLE = 0x5,
};

enum MidenSegmentFlag : unsigned {
  MIDEN_SEG_FLAG_STRINGS = 0x1,
  MIDEN_SEG_FLAG_TLS = 0x2,
};

// Kinds of tag attributes.
enum MidenTagAttribute : uint8_t {
  MIDEN_TAG_ATTRIBUTE_EXCEPTION = 0x0,
};

const unsigned MIDEN_SYMBOL_BINDING_MASK = 0x3;
const unsigned MIDEN_SYMBOL_VISIBILITY_MASK = 0xc;

const unsigned MIDEN_SYMBOL_BINDING_GLOBAL = 0x0;
const unsigned MIDEN_SYMBOL_BINDING_WEAK = 0x1;
const unsigned MIDEN_SYMBOL_BINDING_LOCAL = 0x2;
const unsigned MIDEN_SYMBOL_VISIBILITY_DEFAULT = 0x0;
const unsigned MIDEN_SYMBOL_VISIBILITY_HIDDEN = 0x4;
const unsigned MIDEN_SYMBOL_UNDEFINED = 0x10;
const unsigned MIDEN_SYMBOL_EXPORTED = 0x20;
const unsigned MIDEN_SYMBOL_EXPLICIT_NAME = 0x40;
const unsigned MIDEN_SYMBOL_NO_STRIP = 0x80;
const unsigned MIDEN_SYMBOL_TLS = 0x100;

#define MIDEN_RELOC(name, value) name = value,

enum : unsigned {
#include "MidenRelocs.def"
};

#undef MIDEN_RELOC

// Subset of types that a value can have
enum class ValType {
  I32 = MIDEN_TYPE_I32,
  I64 = MIDEN_TYPE_I64,
  F32 = MIDEN_TYPE_F32,
  F64 = MIDEN_TYPE_F64,
  V128 = MIDEN_TYPE_V128,
  FUNCREF = MIDEN_TYPE_FUNCREF,
  EXTERNREF = MIDEN_TYPE_EXTERNREF,
};

struct MidenSignature {
  SmallVector<ValType, 1> Returns;
  SmallVector<ValType, 4> Params;
  // Support empty and tombstone instances, needed by DenseMap.
  enum { Plain, Empty, Tombstone } State = Plain;

  MidenSignature(SmallVector<ValType, 1> &&InReturns,
                 SmallVector<ValType, 4> &&InParams)
      : Returns(InReturns), Params(InParams) {}
  MidenSignature() = default;
};

// Useful comparison operators
inline bool operator==(const MidenSignature &LHS, const MidenSignature &RHS) {
  return LHS.State == RHS.State && LHS.Returns == RHS.Returns &&
         LHS.Params == RHS.Params;
}

inline bool operator!=(const MidenSignature &LHS, const MidenSignature &RHS) {
  return !(LHS == RHS);
}

inline bool operator==(const MidenGlobalType &LHS, const MidenGlobalType &RHS) {
  return LHS.Type == RHS.Type && LHS.Mutable == RHS.Mutable;
}

inline bool operator!=(const MidenGlobalType &LHS, const MidenGlobalType &RHS) {
  return !(LHS == RHS);
}

inline bool operator==(const MidenLimits &LHS, const MidenLimits &RHS) {
  return LHS.Flags == RHS.Flags && LHS.Minimum == RHS.Minimum &&
         (LHS.Flags & MIDEN_LIMITS_FLAG_HAS_MAX ? LHS.Maximum == RHS.Maximum
                                                : true);
}

inline bool operator==(const MidenTableType &LHS, const MidenTableType &RHS) {
  return LHS.ElemType == RHS.ElemType && LHS.Limits == RHS.Limits;
}

llvm::StringRef toString(MidenSymbolType type);
llvm::StringRef relocTypetoString(uint32_t type);
llvm::StringRef sectionTypeToString(uint32_t type);
bool relocTypeHasAddend(uint32_t type);

} // end namespace miden
} // end namespace llvm

#endif
