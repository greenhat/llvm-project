//=- MidenMachineFunctionInfo.cpp - Miden Machine Function Info -=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements Miden-specific per-machine-function
/// information.
///
//===----------------------------------------------------------------------===//

#include "MidenMachineFunctionInfo.h"
#include "MCTargetDesc/MidenInstPrinter.h"
#include "MidenISelLowering.h"
#include "MidenSubtarget.h"
#include "Utils/MidenTypeUtilities.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/MidenEHFuncInfo.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

MidenFunctionInfo::~MidenFunctionInfo() = default; // anchor.

MachineFunctionInfo *MidenFunctionInfo::clone(
    BumpPtrAllocator &Allocator, MachineFunction &DestMF,
    const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
    const {
  MidenFunctionInfo *Clone = DestMF.cloneInfo<MidenFunctionInfo>(*this);
  Clone->MF = &DestMF;
  return Clone;
}

void MidenFunctionInfo::initWARegs(MachineRegisterInfo &MRI) {
  assert(WARegs.empty());
  unsigned Reg = UnusedReg;
  WARegs.resize(MRI.getNumVirtRegs(), Reg);
}

void llvm::computeLegalValueVTs(const MidenTargetLowering &TLI,
                                LLVMContext &Ctx, const DataLayout &DL,
                                Type *Ty, SmallVectorImpl<MVT> &ValueVTs) {
  SmallVector<EVT, 4> VTs;
  ComputeValueVTs(TLI, DL, Ty, VTs);

  for (EVT VT : VTs) {
    unsigned NumRegs = TLI.getNumRegisters(Ctx, VT);
    MVT RegisterVT = TLI.getRegisterType(Ctx, VT);
    for (unsigned I = 0; I != NumRegs; ++I)
      ValueVTs.push_back(RegisterVT);
  }
}

void llvm::computeLegalValueVTs(const Function &F, const TargetMachine &TM,
                                Type *Ty, SmallVectorImpl<MVT> &ValueVTs) {
  const DataLayout &DL(F.getParent()->getDataLayout());
  const MidenTargetLowering &TLI =
      *TM.getSubtarget<MidenSubtarget>(F).getTargetLowering();
  computeLegalValueVTs(TLI, F.getContext(), DL, Ty, ValueVTs);
}

void llvm::computeSignatureVTs(const FunctionType *Ty,
                               const Function *TargetFunc,
                               const Function &ContextFunc,
                               const TargetMachine &TM,
                               SmallVectorImpl<MVT> &Params,
                               SmallVectorImpl<MVT> &Results) {
  computeLegalValueVTs(ContextFunc, TM, Ty->getReturnType(), Results);

  MVT PtrVT = MVT::getIntegerVT(TM.createDataLayout().getPointerSizeInBits());
  if (Results.size() > 1 &&
      !TM.getSubtarget<MidenSubtarget>(ContextFunc).hasMultivalue()) {
    // Miden can't lower returns of multiple values without demoting to
    // sret unless multivalue is enabled (see
    // MidenTargetLowering::CanLowerReturn). So replace multiple return
    // values with a poitner parameter.
    Results.clear();
    Params.push_back(PtrVT);
  }

  for (auto *Param : Ty->params())
    computeLegalValueVTs(ContextFunc, TM, Param, Params);
  if (Ty->isVarArg())
    Params.push_back(PtrVT);

  // For swiftcc, emit additional swiftself and swifterror parameters
  // if there aren't. These additional parameters are also passed for caller.
  // They are necessary to match callee and caller signature for indirect
  // call.

  if (TargetFunc && TargetFunc->getCallingConv() == CallingConv::Swift) {
    MVT PtrVT = MVT::getIntegerVT(TM.createDataLayout().getPointerSizeInBits());
    bool HasSwiftErrorArg = false;
    bool HasSwiftSelfArg = false;
    for (const auto &Arg : TargetFunc->args()) {
      HasSwiftErrorArg |= Arg.hasAttribute(Attribute::SwiftError);
      HasSwiftSelfArg |= Arg.hasAttribute(Attribute::SwiftSelf);
    }
    if (!HasSwiftErrorArg)
      Params.push_back(PtrVT);
    if (!HasSwiftSelfArg)
      Params.push_back(PtrVT);
  }
}

void llvm::valTypesFromMVTs(const ArrayRef<MVT> &In,
                            SmallVectorImpl<miden::ValType> &Out) {
  for (MVT Ty : In)
    Out.push_back(Miden::toValType(Ty));
}

std::unique_ptr<miden::MidenSignature>
llvm::signatureFromMVTs(const SmallVectorImpl<MVT> &Results,
                        const SmallVectorImpl<MVT> &Params) {
  auto Sig = std::make_unique<miden::MidenSignature>();
  valTypesFromMVTs(Results, Sig->Returns);
  valTypesFromMVTs(Params, Sig->Params);
  return Sig;
}

yaml::MidenFunctionInfo::MidenFunctionInfo(const llvm::MidenFunctionInfo &MFI)
    : CFGStackified(MFI.isCFGStackified()) {
  auto *EHInfo = MFI.getMidenEHFuncInfo();
  const llvm::MachineFunction &MF = MFI.getMachineFunction();

  for (auto VT : MFI.getParams())
    Params.push_back(EVT(VT).getEVTString());
  for (auto VT : MFI.getResults())
    Results.push_back(EVT(VT).getEVTString());

  //  MFI.getMidenEHFuncInfo() is non-null only for functions with the
  //  personality function.
  if (EHInfo) {
    // SrcToUnwindDest can contain stale mappings in case BBs are removed in
    // optimizations, in case, for example, they are unreachable. We should not
    // include their info.
    SmallPtrSet<const MachineBasicBlock *, 16> MBBs;
    for (const auto &MBB : MF)
      MBBs.insert(&MBB);
    for (auto KV : EHInfo->SrcToUnwindDest) {
      auto *SrcBB = KV.first.get<MachineBasicBlock *>();
      auto *DestBB = KV.second.get<MachineBasicBlock *>();
      if (MBBs.count(SrcBB) && MBBs.count(DestBB))
        SrcToUnwindDest[SrcBB->getNumber()] = DestBB->getNumber();
    }
  }
}

void yaml::MidenFunctionInfo::mappingImpl(yaml::IO &YamlIO) {
  MappingTraits<MidenFunctionInfo>::mapping(YamlIO, *this);
}

void MidenFunctionInfo::initializeBaseYamlFields(
    const yaml::MidenFunctionInfo &YamlMFI) {
  CFGStackified = YamlMFI.CFGStackified;
  for (auto VT : YamlMFI.Params)
    addParam(Miden::parseMVT(VT.Value));
  for (auto VT : YamlMFI.Results)
    addResult(Miden::parseMVT(VT.Value));
  if (MidenEHInfo) {
    for (auto KV : YamlMFI.SrcToUnwindDest)
      MidenEHInfo->setUnwindDest(MF->getBlockNumbered(KV.first),
                                 MF->getBlockNumbered(KV.second));
  }
}
