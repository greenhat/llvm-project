//===-- MidenFrameLowering.cpp - Miden Frame Lowering ----------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the Miden implementation of
/// TargetFrameLowering class.
///
/// On Miden, there aren't a lot of things to do here. There are no
/// callee-saved registers to save, and no spill slots.
///
/// The stack grows downward.
///
//===----------------------------------------------------------------------===//

#include "MidenFrameLowering.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "Miden.h"
#include "MidenInstrInfo.h"
#include "MidenMachineFunctionInfo.h"
#include "MidenSubtarget.h"
#include "MidenTargetMachine.h"
#include "Utils/MidenTypeUtilities.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/Debug.h"
using namespace llvm;

#define DEBUG_TYPE "miden-frame-info"

// TODO: miden64
// TODO: Emit TargetOpcode::CFI_INSTRUCTION instructions

// In an ideal world, when objects are added to the MachineFrameInfo by
// FunctionLoweringInfo::set, we could somehow hook into target-specific code to
// ensure they are assigned the right stack ID.  However there isn't a hook that
// runs between then and DAG building time, though, so instead we hoist stack
// objects lazily when they are first used, and comprehensively after the DAG is
// built via the PreprocessISelDAG hook, called by the
// SelectionDAGISel::runOnMachineFunction.  We have to do it in two places
// because we want to do it while building the selection DAG for uses of alloca,
// but not all alloca instructions are used so we have to follow up afterwards.
Optional<unsigned>
MidenFrameLowering::getLocalForStackObject(MachineFunction &MF,
                                           int FrameIndex) {
  MachineFrameInfo &MFI = MF.getFrameInfo();

  // If already hoisted to a local, done.
  if (MFI.getStackID(FrameIndex) == TargetStackID::MidenLocal)
    return static_cast<unsigned>(MFI.getObjectOffset(FrameIndex));

  // If not allocated in the object address space, this object will be in
  // linear memory.
  const AllocaInst *AI = MFI.getObjectAllocation(FrameIndex);
  if (!AI || !Miden::isMidenVarAddressSpace(AI->getType()->getAddressSpace()))
    return None;

  // Otherwise, allocate this object in the named value stack, outside of linear
  // memory.
  SmallVector<EVT, 4> ValueVTs;
  const MidenTargetLowering &TLI =
      *MF.getSubtarget<MidenSubtarget>().getTargetLowering();
  MidenFunctionInfo *FuncInfo = MF.getInfo<MidenFunctionInfo>();
  ComputeValueVTs(TLI, MF.getDataLayout(), AI->getAllocatedType(), ValueVTs);
  MFI.setStackID(FrameIndex, TargetStackID::MidenLocal);
  // Abuse SP offset to record the index of the first local in the object.
  unsigned Local = FuncInfo->getParams().size() + FuncInfo->getLocals().size();
  MFI.setObjectOffset(FrameIndex, Local);
  // Allocate Miden locals for each non-aggregate component of the
  // allocation.
  for (EVT ValueVT : ValueVTs)
    FuncInfo->addLocal(ValueVT.getSimpleVT());
  // Abuse object size to record number of Miden locals allocated to
  // this object.
  MFI.setObjectSize(FrameIndex, ValueVTs.size());
  return static_cast<unsigned>(Local);
}

/// We need a base pointer in the case of having items on the stack that
/// require stricter alignment than the stack pointer itself.  Because we need
/// to shift the stack pointer by some unknown amount to force the alignment,
/// we need to record the value of the stack pointer on entry to the function.
bool MidenFrameLowering::hasBP(const MachineFunction &MF) const {
  const auto *RegInfo = MF.getSubtarget<MidenSubtarget>().getRegisterInfo();
  return RegInfo->hasStackRealignment(MF);
}

/// Return true if the specified function should have a dedicated frame pointer
/// register.
bool MidenFrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();

  // When we have var-sized objects, we move the stack pointer by an unknown
  // amount, and need to emit a frame pointer to restore the stack to where we
  // were on function entry.
  // If we already need a base pointer, we use that to fix up the stack pointer.
  // If there are no fixed-size objects, we would have no use of a frame
  // pointer, and thus should not emit one.
  bool HasFixedSizedObjects = MFI.getStackSize() > 0;
  bool NeedsFixedReference = !hasBP(MF) || HasFixedSizedObjects;

  return MFI.isFrameAddressTaken() ||
         (MFI.hasVarSizedObjects() && NeedsFixedReference) ||
         MFI.hasStackMap() || MFI.hasPatchPoint();
}

/// Under normal circumstances, when a frame pointer is not required, we reserve
/// argument space for call sites in the function immediately on entry to the
/// current function. This eliminates the need for add/sub sp brackets around
/// call sites. Returns true if the call frame is included as part of the stack
/// frame.
bool MidenFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  return !MF.getFrameInfo().hasVarSizedObjects();
}

// Returns true if this function needs a local user-space stack pointer for its
// local frame (not for exception handling).
bool MidenFrameLowering::needsSPForLocalFrame(const MachineFunction &MF) const {
  auto &MFI = MF.getFrameInfo();
  return MFI.getStackSize() || MFI.adjustsStack() || hasFP(MF);
}

// In function with EH pads, we need to make a copy of the value of
// __stack_pointer global in SP32/64 register, in order to use it when
// restoring __stack_pointer after an exception is caught.
bool MidenFrameLowering::needsPrologForEH(const MachineFunction &MF) const {
  auto EHType = MF.getTarget().getMCAsmInfo()->getExceptionHandlingType();
  return EHType == ExceptionHandling::Miden &&
         MF.getFunction().hasPersonalityFn() && MF.getFrameInfo().hasCalls();
}

/// Returns true if this function needs a local user-space stack pointer.
/// Unlike a machine stack pointer, the miden user stack pointer is a global
/// variable, so it is loaded into a register in the prolog.
bool MidenFrameLowering::needsSP(const MachineFunction &MF) const {
  return needsSPForLocalFrame(MF) || needsPrologForEH(MF);
}

/// Returns true if the local user-space stack pointer needs to be written back
/// to __stack_pointer global by this function (this is not meaningful if
/// needsSP is false). If false, the stack red zone can be used and only a local
/// SP is needed.
bool MidenFrameLowering::needsSPWriteback(const MachineFunction &MF) const {
  auto &MFI = MF.getFrameInfo();
  assert(needsSP(MF));
  // When we don't need a local stack pointer for its local frame but only to
  // support EH, we don't need to write SP back in the epilog, because we don't
  // bump down the stack pointer in the prolog. We need to write SP back in the
  // epilog only if
  // 1. We need SP not only for EH support but also because we actually use
  // stack or we have a frame address taken.
  // 2. We cannot use the red zone.
  bool CanUseRedZone = MFI.getStackSize() <= RedZoneSize && !MFI.hasCalls() &&
                       !MF.getFunction().hasFnAttribute(Attribute::NoRedZone);
  return needsSPForLocalFrame(MF) && !CanUseRedZone;
}

unsigned MidenFrameLowering::getSPReg(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::SP64
                                                       : Miden::SP32;
}

unsigned MidenFrameLowering::getFPReg(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::FP64
                                                       : Miden::FP32;
}

unsigned MidenFrameLowering::getOpcConst(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::CONST_I64
                                                       : Miden::CONST_I32;
}

unsigned MidenFrameLowering::getOpcAdd(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::ADD_I64
                                                       : Miden::ADD_I32;
}

unsigned MidenFrameLowering::getOpcSub(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::SUB_I64
                                                       : Miden::SUB_I32;
}

unsigned MidenFrameLowering::getOpcAnd(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::AND_I64
                                                       : Miden::AND_I32;
}

unsigned MidenFrameLowering::getOpcGlobGet(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::GLOBAL_GET_I64
                                                       : Miden::GLOBAL_GET_I32;
}

unsigned MidenFrameLowering::getOpcGlobSet(const MachineFunction &MF) {
  return MF.getSubtarget<MidenSubtarget>().hasAddr64() ? Miden::GLOBAL_SET_I64
                                                       : Miden::GLOBAL_SET_I32;
}

void MidenFrameLowering::writeSPToGlobal(
    unsigned SrcReg, MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator &InsertStore, const DebugLoc &DL) const {
  const auto *TII = MF.getSubtarget<MidenSubtarget>().getInstrInfo();

  const char *ES = "__stack_pointer";
  auto *SPSymbol = MF.createExternalSymbolName(ES);

  BuildMI(MBB, InsertStore, DL, TII->get(getOpcGlobSet(MF)))
      .addExternalSymbol(SPSymbol)
      .addReg(SrcReg);
}

MachineBasicBlock::iterator MidenFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  assert(!I->getOperand(0).getImm() && (hasFP(MF) || hasBP(MF)) &&
         "Call frame pseudos should only be used for dynamic stack adjustment");
  auto &ST = MF.getSubtarget<MidenSubtarget>();
  const auto *TII = ST.getInstrInfo();
  if (I->getOpcode() == TII->getCallFrameDestroyOpcode() &&
      needsSPWriteback(MF)) {
    DebugLoc DL = I->getDebugLoc();
    writeSPToGlobal(getSPReg(MF), MF, MBB, I, DL);
  }
  return MBB.erase(I);
}

void MidenFrameLowering::emitPrologue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
  // TODO: Do ".setMIFlag(MachineInstr::FrameSetup)" on emitted instructions
  auto &MFI = MF.getFrameInfo();
  assert(MFI.getCalleeSavedInfo().empty() &&
         "Miden should not have callee-saved registers");

  if (!needsSP(MF))
    return;
  uint64_t StackSize = MFI.getStackSize();

  auto &ST = MF.getSubtarget<MidenSubtarget>();
  const auto *TII = ST.getInstrInfo();
  auto &MRI = MF.getRegInfo();

  auto InsertPt = MBB.begin();
  while (InsertPt != MBB.end() && Miden::isArgument(InsertPt->getOpcode()))
    ++InsertPt;
  DebugLoc DL;

  const TargetRegisterClass *PtrRC =
      MRI.getTargetRegisterInfo()->getPointerRegClass(MF);
  unsigned SPReg = getSPReg(MF);
  if (StackSize)
    SPReg = MRI.createVirtualRegister(PtrRC);

  const char *ES = "__stack_pointer";
  auto *SPSymbol = MF.createExternalSymbolName(ES);
  BuildMI(MBB, InsertPt, DL, TII->get(getOpcGlobGet(MF)), SPReg)
      .addExternalSymbol(SPSymbol);

  bool HasBP = hasBP(MF);
  if (HasBP) {
    auto FI = MF.getInfo<MidenFunctionInfo>();
    Register BasePtr = MRI.createVirtualRegister(PtrRC);
    FI->setBasePointerVreg(BasePtr);
    BuildMI(MBB, InsertPt, DL, TII->get(Miden::COPY), BasePtr).addReg(SPReg);
  }
  if (StackSize) {
    // Subtract the frame size
    Register OffsetReg = MRI.createVirtualRegister(PtrRC);
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcConst(MF)), OffsetReg)
        .addImm(StackSize);
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcSub(MF)), getSPReg(MF))
        .addReg(SPReg)
        .addReg(OffsetReg);
  }
  if (HasBP) {
    Register BitmaskReg = MRI.createVirtualRegister(PtrRC);
    Align Alignment = MFI.getMaxAlign();
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcConst(MF)), BitmaskReg)
        .addImm((int64_t) ~(Alignment.value() - 1));
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcAnd(MF)), getSPReg(MF))
        .addReg(getSPReg(MF))
        .addReg(BitmaskReg);
  }
  if (hasFP(MF)) {
    // Unlike most conventional targets (where FP points to the saved FP),
    // FP points to the bottom of the fixed-size locals, so we can use positive
    // offsets in load/store instructions.
    BuildMI(MBB, InsertPt, DL, TII->get(Miden::COPY), getFPReg(MF))
        .addReg(getSPReg(MF));
  }
  if (StackSize && needsSPWriteback(MF)) {
    writeSPToGlobal(getSPReg(MF), MF, MBB, InsertPt, DL);
  }
}

void MidenFrameLowering::emitEpilogue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
  uint64_t StackSize = MF.getFrameInfo().getStackSize();
  if (!needsSP(MF) || !needsSPWriteback(MF))
    return;
  auto &ST = MF.getSubtarget<MidenSubtarget>();
  const auto *TII = ST.getInstrInfo();
  auto &MRI = MF.getRegInfo();
  auto InsertPt = MBB.getFirstTerminator();
  DebugLoc DL;

  if (InsertPt != MBB.end())
    DL = InsertPt->getDebugLoc();

  // Restore the stack pointer. If we had fixed-size locals, add the offset
  // subtracted in the prolog.
  unsigned SPReg = 0;
  unsigned SPFPReg = hasFP(MF) ? getFPReg(MF) : getSPReg(MF);
  if (hasBP(MF)) {
    auto FI = MF.getInfo<MidenFunctionInfo>();
    SPReg = FI->getBasePointerVreg();
  } else if (StackSize) {
    const TargetRegisterClass *PtrRC =
        MRI.getTargetRegisterInfo()->getPointerRegClass(MF);
    Register OffsetReg = MRI.createVirtualRegister(PtrRC);
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcConst(MF)), OffsetReg)
        .addImm(StackSize);
    // In the epilog we don't need to write the result back to the SP32/64
    // physreg because it won't be used again. We can use a stackified register
    // instead.
    SPReg = MRI.createVirtualRegister(PtrRC);
    BuildMI(MBB, InsertPt, DL, TII->get(getOpcAdd(MF)), SPReg)
        .addReg(SPFPReg)
        .addReg(OffsetReg);
  } else {
    SPReg = SPFPReg;
  }

  writeSPToGlobal(SPReg, MF, MBB, InsertPt, DL);
}

bool MidenFrameLowering::isSupportedStackID(TargetStackID::Value ID) const {
  // Use the Object stack for Miden locals which can only be accessed
  // by name, not via an address in linear memory.
  if (ID == TargetStackID::MidenLocal)
    return true;

  return TargetFrameLowering::isSupportedStackID(ID);
}

TargetFrameLowering::DwarfFrameBase
MidenFrameLowering::getDwarfFrameBase(const MachineFunction &MF) const {
  DwarfFrameBase Loc;
  Loc.Kind = DwarfFrameBase::MidenFrameBase;
  const MidenFunctionInfo &MFI = *MF.getInfo<MidenFunctionInfo>();
  if (needsSP(MF) && MFI.isFrameBaseVirtual()) {
    unsigned LocalNum = MFI.getFrameBaseLocal();
    Loc.Location.MidenLoc = {Miden::TI_LOCAL, LocalNum};
  } else {
    // TODO: This should work on a breakpoint at a function with no frame,
    // but probably won't work for traversing up the stack.
    Loc.Location.MidenLoc = {Miden::TI_GLOBAL_RELOC, 0};
  }
  return Loc;
}
