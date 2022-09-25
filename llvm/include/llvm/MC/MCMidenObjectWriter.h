//===-- llvm/MC/MCMidenObjectWriter.h - Miden Object Writer -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCMIDENOBJECTWRITER_H
#define LLVM_MC_MCMIDENOBJECTWRITER_H

#include "llvm/MC/MCObjectWriter.h"
#include <memory>

namespace llvm {

class MCFixup;
class MCSectionMiden;
class MCValue;
class raw_pwrite_stream;

class MCMidenObjectTargetWriter : public MCObjectTargetWriter {
  const unsigned Is64Bit : 1;
  const unsigned IsEmscripten : 1;

protected:
  explicit MCMidenObjectTargetWriter(bool Is64Bit_, bool IsEmscripten);

public:
  virtual ~MCMidenObjectTargetWriter();

  Triple::ObjectFormatType getFormat() const override { return Triple::Miden; }
  static bool classof(const MCObjectTargetWriter *W) {
    return W->getFormat() == Triple::Miden;
  }

  virtual unsigned getRelocType(const MCValue &Target, const MCFixup &Fixup,
                                const MCSectionMiden &FixupSection,
                                bool IsLocRel) const = 0;

  /// \name Accessors
  /// @{
  bool is64Bit() const { return Is64Bit; }
  bool isEmscripten() const { return IsEmscripten; }
  /// @}
};

/// Construct a new Miden writer instance.
///
/// \param MOTW - The target specific Miden writer subclass.
/// \param OS - The stream to write to.
/// \returns The constructed object writer.
std::unique_ptr<MCObjectWriter>
createMidenObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                       raw_pwrite_stream &OS);

std::unique_ptr<MCObjectWriter>
createMidenDwoObjectWriter(std::unique_ptr<MCMidenObjectTargetWriter> MOTW,
                          raw_pwrite_stream &OS, raw_pwrite_stream &DwoOS);

} // namespace llvm

#endif
