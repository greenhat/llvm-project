// MidenAsmPrinter.h - Miden implementation of AsmPrinter-*- C++ -*-
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIDEN_MIDENASMPRINTER_H
#define LLVM_LIB_TARGET_MIDEN_MIDENASMPRINTER_H

#include "MidenMachineFunctionInfo.h"
#include "MidenSubtarget.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class MidenTargetStreamer;

class LLVM_LIBRARY_VISIBILITY MidenAsmPrinter final : public AsmPrinter {
  const MidenSubtarget *Subtarget;
  const MachineRegisterInfo *MRI;
  MidenFunctionInfo *MFI;
  // TODO: Do the uniquing of Signatures here instead of ObjectFileWriter?
  std::vector<std::unique_ptr<miden::MidenSignature>> Signatures;
  std::vector<std::unique_ptr<std::string>> Names;
  bool signaturesEmitted = false;

  StringRef storeName(StringRef Name) {
    std::unique_ptr<std::string> N = std::make_unique<std::string>(Name);
    Names.push_back(std::move(N));
    return *Names.back();
  }

public:
  explicit MidenAsmPrinter(TargetMachine &TM,
                           std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), Subtarget(nullptr), MRI(nullptr),
        MFI(nullptr) {}

  StringRef getPassName() const override { return "Miden Assembly Printer"; }

  const MidenSubtarget &getSubtarget() const { return *Subtarget; }
  void addSignature(std::unique_ptr<miden::MidenSignature> &&Sig) {
    Signatures.push_back(std::move(Sig));
  }

  //===------------------------------------------------------------------===//
  // MachineFunctionPass Implementation.
  //===------------------------------------------------------------------===//

  bool runOnMachineFunction(MachineFunction &MF) override {
    Subtarget = &MF.getSubtarget<MidenSubtarget>();
    MRI = &MF.getRegInfo();
    MFI = MF.getInfo<MidenFunctionInfo>();
    return AsmPrinter::runOnMachineFunction(MF);
  }

  //===------------------------------------------------------------------===//
  // AsmPrinter Implementation.
  //===------------------------------------------------------------------===//

  void emitEndOfAsmFile(Module &M) override;
  void EmitProducerInfo(Module &M);
  void EmitTargetFeatures(Module &M);
  void emitSymbolType(const MCSymbolMiden *Sym);
  void emitGlobalVariable(const GlobalVariable *GV) override;
  void emitJumpTableInfo() override;
  void emitConstantPool() override;
  void emitFunctionBodyStart() override;
  void emitInstruction(const MachineInstr *MI) override;
  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &OS) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                             const char *ExtraCode, raw_ostream &OS) override;

  MVT getRegType(unsigned RegNo) const;
  std::string regToString(const MachineOperand &MO);
  MidenTargetStreamer *getTargetStreamer();
  MCSymbolMiden *getMCSymbolForFunction(const Function *F, bool EnableEmEH,
                                        miden::MidenSignature *Sig,
                                        bool &InvokeDetected);
  MCSymbol *getOrCreateMidenSymbol(StringRef Name);
  void emitDecls(const Module &M);
};

} // end namespace llvm

#endif
