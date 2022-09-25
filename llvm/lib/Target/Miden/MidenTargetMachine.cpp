//===- MidenTargetMachine.cpp - Define TargetMachine for Miden -==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines the Miden-specific subclass of TargetMachine.
///
//===----------------------------------------------------------------------===//

#include "MidenTargetMachine.h"
#include "MCTargetDesc/MidenMCTargetDesc.h"
#include "Miden.h"
#include "MidenMachineFunctionInfo.h"
#include "MidenTargetObjectFile.h"
#include "MidenTargetTransformInfo.h"
#include "TargetInfo/MidenTargetInfo.h"
#include "Utils/MidenUtilities.h"
#include "llvm/CodeGen/MIRParser/MIParser.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Function.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/LowerAtomicPass.h"
#include "llvm/Transforms/Utils.h"
using namespace llvm;

#define DEBUG_TYPE "miden"

// A command-line option to keep implicit locals
// for the purpose of testing with lit/llc ONLY.
// This produces output which is not valid Miden, and is not supported
// by assemblers/disassemblers and other MC based tools.
static cl::opt<bool> MidenDisableExplicitLocals(
    "miden-disable-explicit-locals", cl::Hidden,
    cl::desc("Miden: output implicit locals in"
             " instruction output for test purposes only."),
    cl::init(false));

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeMidenTarget() {
  // Register the target.
  RegisterTargetMachine<MidenTargetMachine> X(getTheMidenTarget32());
  RegisterTargetMachine<MidenTargetMachine> Y(getTheMidenTarget64());

  // Register backend passes
  auto &PR = *PassRegistry::getPassRegistry();
  initializeMidenAddMissingPrototypesPass(PR);
  initializeMidenLowerEmscriptenEHSjLjPass(PR);
  initializeLowerGlobalDtorsLegacyPassPass(PR);
  initializeFixFunctionBitcastsPass(PR);
  initializeOptimizeReturnedPass(PR);
  initializeMidenArgumentMovePass(PR);
  initializeMidenSetP2AlignOperandsPass(PR);
  initializeMidenReplacePhysRegsPass(PR);
  initializeMidenOptimizeLiveIntervalsPass(PR);
  initializeMidenMemIntrinsicResultsPass(PR);
  initializeMidenRegStackifyPass(PR);
  initializeMidenRegColoringPass(PR);
  initializeMidenNullifyDebugValueListsPass(PR);
  initializeMidenFixIrreducibleControlFlowPass(PR);
  initializeMidenLateEHPreparePass(PR);
  initializeMidenExceptionInfoPass(PR);
  initializeMidenCFGSortPass(PR);
  initializeMidenCFGStackifyPass(PR);
  initializeMidenExplicitLocalsPass(PR);
  initializeMidenLowerBrUnlessPass(PR);
  initializeMidenRegNumberingPass(PR);
  initializeMidenDebugFixupPass(PR);
  initializeMidenPeepholePass(PR);
  initializeMidenMCLowerPrePassPass(PR);
}

//===----------------------------------------------------------------------===//
// Miden Lowering public interface.
//===----------------------------------------------------------------------===//

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM,
                                           const Triple &TT) {
  if (!RM) {
    // Default to static relocation model.  This should always be more optimial
    // than PIC since the static linker can determine all global addresses and
    // assume direct function calls.
    return Reloc::Static;
  }

  if (!TT.isOSEmscripten()) {
    // Relocation modes other than static are currently implemented in a way
    // that only works for Emscripten, so disable them if we aren't targeting
    // Emscripten.
    return Reloc::Static;
  }

  return *RM;
}

/// Create an Miden architecture model.
///
MidenTargetMachine::MidenTargetMachine(const Target &T, const Triple &TT,
                                       StringRef CPU, StringRef FS,
                                       const TargetOptions &Options,
                                       Optional<Reloc::Model> RM,
                                       Optional<CodeModel::Model> CM,
                                       CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(
          T,
          TT.isArch64Bit()
              ? (TT.isOSEmscripten() ? "e-m:e-p:64:64-p10:8:8-p20:8:8-i64:64-"
                                       "f128:64-n32:64-S128-ni:1:10:20"
                                     : "e-m:e-p:64:64-p10:8:8-p20:8:8-i64:64-"
                                       "n32:64-S128-ni:1:10:20")
              : (TT.isOSEmscripten() ? "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-"
                                       "f128:64-n32:64-S128-ni:1:10:20"
                                     : "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-"
                                       "n32:64-S128-ni:1:10:20"),
          TT, CPU, FS, Options, getEffectiveRelocModel(RM, TT),
          getEffectiveCodeModel(CM, CodeModel::Large), OL),
      TLOF(new MidenTargetObjectFile()) {
  // Miden type-checks instructions, but a noreturn function with a return
  // type that doesn't match the context will cause a check failure. So we lower
  // LLVM 'unreachable' to ISD::TRAP and then lower that to Miden's
  // 'unreachable' instructions which is meant for that case.
  this->Options.TrapUnreachable = true;

  // Miden treats each function as an independent unit. Force
  // -ffunction-sections, effectively, so that we can emit them independently.
  this->Options.FunctionSections = true;
  this->Options.DataSections = true;
  this->Options.UniqueSectionNames = true;

  initAsmInfo();

  // Note that we don't use setRequiresStructuredCFG(true). It disables
  // optimizations than we're ok with, and want, such as critical edge
  // splitting and tail merging.
}

MidenTargetMachine::~MidenTargetMachine() = default; // anchor.

const MidenSubtarget *MidenTargetMachine::getSubtargetImpl() const {
  return getSubtargetImpl(std::string(getTargetCPU()),
                          std::string(getTargetFeatureString()));
}

const MidenSubtarget *
MidenTargetMachine::getSubtargetImpl(std::string CPU, std::string FS) const {
  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    I = std::make_unique<MidenSubtarget>(TargetTriple, CPU, FS, *this);
  }
  return I.get();
}

const MidenSubtarget *
MidenTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  // This needs to be done before we create a new subtarget since any
  // creation will depend on the TM and the code generation flags on the
  // function that reside in TargetOptions.
  resetTargetOptions(F);

  return getSubtargetImpl(CPU, FS);
}

namespace {

class CoalesceFeaturesAndStripAtomics final : public ModulePass {
  // Take the union of all features used in the module and use it for each
  // function individually, since having multiple feature sets in one module
  // currently does not make sense for Miden. If atomics are not enabled,
  // also strip atomic operations and thread local storage.
  static char ID;
  MidenTargetMachine *MidenTM;

public:
  CoalesceFeaturesAndStripAtomics(MidenTargetMachine *MidenTM)
      : ModulePass(ID), MidenTM(MidenTM) {}

  bool runOnModule(Module &M) override {
    FeatureBitset Features = coalesceFeatures(M);

    std::string FeatureStr = getFeatureString(Features);
    MidenTM->setTargetFeatureString(FeatureStr);
    for (auto &F : M)
      replaceFeatures(F, FeatureStr);

    bool StrippedAtomics = false;
    bool StrippedTLS = false;

    if (!Features[Miden::FeatureAtomics]) {
      StrippedAtomics = stripAtomics(M);
      StrippedTLS = stripThreadLocals(M);
    } else if (!Features[Miden::FeatureBulkMemory]) {
      StrippedTLS |= stripThreadLocals(M);
    }

    if (StrippedAtomics && !StrippedTLS)
      stripThreadLocals(M);
    else if (StrippedTLS && !StrippedAtomics)
      stripAtomics(M);

    recordFeatures(M, Features, StrippedAtomics || StrippedTLS);

    // Conservatively assume we have made some change
    return true;
  }

private:
  FeatureBitset coalesceFeatures(const Module &M) {
    FeatureBitset Features =
        MidenTM
            ->getSubtargetImpl(std::string(MidenTM->getTargetCPU()),
                               std::string(MidenTM->getTargetFeatureString()))
            ->getFeatureBits();
    for (auto &F : M)
      Features |= MidenTM->getSubtargetImpl(F)->getFeatureBits();
    return Features;
  }

  std::string getFeatureString(const FeatureBitset &Features) {
    std::string Ret;
    for (const SubtargetFeatureKV &KV : MidenFeatureKV) {
      if (Features[KV.Value])
        Ret += (StringRef("+") + KV.Key + ",").str();
    }
    return Ret;
  }

  void replaceFeatures(Function &F, const std::string &Features) {
    F.removeFnAttr("target-features");
    F.removeFnAttr("target-cpu");
    F.addFnAttr("target-features", Features);
  }

  bool stripAtomics(Module &M) {
    // Detect whether any atomics will be lowered, since there is no way to tell
    // whether the LowerAtomic pass lowers e.g. stores.
    bool Stripped = false;
    for (auto &F : M) {
      for (auto &B : F) {
        for (auto &I : B) {
          if (I.isAtomic()) {
            Stripped = true;
            goto done;
          }
        }
      }
    }

  done:
    if (!Stripped)
      return false;

    LowerAtomicPass Lowerer;
    FunctionAnalysisManager FAM;
    for (auto &F : M)
      Lowerer.run(F, FAM);

    return true;
  }

  bool stripThreadLocals(Module &M) {
    bool Stripped = false;
    for (auto &GV : M.globals()) {
      if (GV.isThreadLocal()) {
        Stripped = true;
        GV.setThreadLocal(false);
      }
    }
    return Stripped;
  }

  void recordFeatures(Module &M, const FeatureBitset &Features, bool Stripped) {
    for (const SubtargetFeatureKV &KV : MidenFeatureKV) {
      if (Features[KV.Value]) {
        // Mark features as used
        std::string MDKey = (StringRef("miden-feature-") + KV.Key).str();
        M.addModuleFlag(Module::ModFlagBehavior::Error, MDKey,
                        miden::MIDEN_FEATURE_PREFIX_USED);
      }
    }
    // Code compiled without atomics or bulk-memory may have had its atomics or
    // thread-local data lowered to nonatomic operations or non-thread-local
    // data. In that case, we mark the pseudo-feature "shared-mem" as disallowed
    // to tell the linker that it would be unsafe to allow this code ot be used
    // in a module with shared memory.
    if (Stripped) {
      M.addModuleFlag(Module::ModFlagBehavior::Error,
                      "miden-feature-shared-mem",
                      miden::MIDEN_FEATURE_PREFIX_DISALLOWED);
    }
  }
};
char CoalesceFeaturesAndStripAtomics::ID = 0;

/// Miden Code Generator Pass Configuration Options.
class MidenPassConfig final : public TargetPassConfig {
public:
  MidenPassConfig(MidenTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  MidenTargetMachine &getMidenTargetMachine() const {
    return getTM<MidenTargetMachine>();
  }

  FunctionPass *createTargetRegisterAllocator(bool) override;

  void addIRPasses() override;
  void addISelPrepare() override;
  bool addInstSelector() override;
  void addPostRegAlloc() override;
  bool addGCPasses() override { return false; }
  void addPreEmitPass() override;
  bool addPreISel() override;

  // No reg alloc
  bool addRegAssignAndRewriteFast() override { return false; }

  // No reg alloc
  bool addRegAssignAndRewriteOptimized() override { return false; }
};
} // end anonymous namespace

TargetTransformInfo
MidenTargetMachine::getTargetTransformInfo(const Function &F) const {
  return TargetTransformInfo(MidenTTIImpl(this, F));
}

TargetPassConfig *MidenTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new MidenPassConfig(*this, PM);
}

FunctionPass *MidenPassConfig::createTargetRegisterAllocator(bool) {
  return nullptr; // No reg alloc
}

using Miden::MidenEnableEH;
using Miden::MidenEnableEmEH;
using Miden::MidenEnableEmSjLj;
using Miden::MidenEnableSjLj;

static void basicCheckForEHAndSjLj(TargetMachine *TM) {
  // Before checking, we make sure TargetOptions.ExceptionModel is the same as
  // MCAsmInfo.ExceptionsType. Normally these have to be the same, because clang
  // stores the exception model info in LangOptions, which is later transferred
  // to TargetOptions and MCAsmInfo. But when clang compiles bitcode directly,
  // clang's LangOptions is not used and thus the exception model info is not
  // correctly transferred to TargetOptions and MCAsmInfo, so we make sure we
  // have the correct exception model in in MidenMCAsmInfo constructor.
  // But in this case TargetOptions is still not updated, so we make sure they
  // are the same.
  TM->Options.ExceptionModel = TM->getMCAsmInfo()->getExceptionHandlingType();

  // Basic Correctness checking related to -exception-model
  if (TM->Options.ExceptionModel != ExceptionHandling::None &&
      TM->Options.ExceptionModel != ExceptionHandling::Miden)
    report_fatal_error("-exception-model should be either 'none' or 'miden'");
  if (MidenEnableEmEH && TM->Options.ExceptionModel == ExceptionHandling::Miden)
    report_fatal_error("-exception-model=miden not allowed with "
                       "-enable-emscripten-cxx-exceptions");
  if (MidenEnableEH && TM->Options.ExceptionModel != ExceptionHandling::Miden)
    report_fatal_error(
        "-miden-enable-eh only allowed with -exception-model=miden");
  if (MidenEnableSjLj && TM->Options.ExceptionModel != ExceptionHandling::Miden)
    report_fatal_error(
        "-miden-enable-sjlj only allowed with -exception-model=miden");
  if ((!MidenEnableEH && !MidenEnableSjLj) &&
      TM->Options.ExceptionModel == ExceptionHandling::Miden)
    report_fatal_error(
        "-exception-model=miden only allowed with at least one of "
        "-miden-enable-eh or -miden-enable-sjj");

  // You can't enable two modes of EH at the same time
  if (MidenEnableEmEH && MidenEnableEH)
    report_fatal_error(
        "-enable-emscripten-cxx-exceptions not allowed with -miden-enable-eh");
  // You can't enable two modes of SjLj at the same time
  if (MidenEnableEmSjLj && MidenEnableSjLj)
    report_fatal_error(
        "-enable-emscripten-sjlj not allowed with -miden-enable-sjlj");
  // You can't mix Emscripten EH with Miden SjLj.
  if (MidenEnableEmEH && MidenEnableSjLj)
    report_fatal_error("-enable-emscripten-cxx-exceptions not allowed with "
                       "-miden-enable-sjlj");
  // Currently it is allowed to mix Miden EH with Emscripten SjLj as an interim
  // measure, but some code will error out at compile time in this combination.
  // See MidenLowerEmscriptenEHSjLj pass for details.
}

//===----------------------------------------------------------------------===//
// The following functions are called from lib/CodeGen/Passes.cpp to modify
// the CodeGen pass sequence.
//===----------------------------------------------------------------------===//

void MidenPassConfig::addIRPasses() {
  // Add signatures to prototype-less function declarations
  addPass(createMidenAddMissingPrototypes());

  // Lower .llvm.global_dtors into .llvm_global_ctors with __cxa_atexit calls.
  addPass(createLowerGlobalDtorsLegacyPass());

  // Fix function bitcasts, as Miden requires caller and callee signatures
  // to match.
  addPass(createMidenFixFunctionBitcasts());

  // Optimize "returned" function attributes.
  if (getOptLevel() != CodeGenOpt::None)
    addPass(createMidenOptimizeReturned());

  basicCheckForEHAndSjLj(TM);

  // If exception handling is not enabled and setjmp/longjmp handling is
  // enabled, we lower invokes into calls and delete unreachable landingpad
  // blocks. Lowering invokes when there is no EH support is done in
  // TargetPassConfig::addPassesToHandleExceptions, but that runs after these IR
  // passes and Emscripten SjLj handling expects all invokes to be lowered
  // before.
  if (!MidenEnableEmEH && !MidenEnableEH) {
    addPass(createLowerInvokePass());
    // The lower invoke pass may create unreachable code. Remove it in order not
    // to process dead blocks in setjmp/longjmp handling.
    addPass(createUnreachableBlockEliminationPass());
  }

  // Handle exceptions and setjmp/longjmp if enabled. Unlike Miden EH
  // preparation done in MidenEHPrepare pass, Miden SjLj preparation shares
  // libraries and transformation algorithms with Emscripten SjLj, so we run
  // LowerEmscriptenEHSjLj pass also when Miden SjLj is enabled.
  if (MidenEnableEmEH || MidenEnableEmSjLj || MidenEnableSjLj)
    addPass(createMidenLowerEmscriptenEHSjLj());

  // Expand indirectbr instructions to switches.
  addPass(createIndirectBrExpandPass());

  TargetPassConfig::addIRPasses();
}

void MidenPassConfig::addISelPrepare() {
  // Lower atomics and TLS if necessary
  addPass(new CoalesceFeaturesAndStripAtomics(&getMidenTargetMachine()));

  // This is a no-op if atomics are not used in the module
  addPass(createAtomicExpandPass());

  TargetPassConfig::addISelPrepare();
}

bool MidenPassConfig::addInstSelector() {
  (void)TargetPassConfig::addInstSelector();
  addPass(createMidenISelDag(getMidenTargetMachine(), getOptLevel()));
  // Run the argument-move pass immediately after the ScheduleDAG scheduler
  // so that we can fix up the ARGUMENT instructions before anything else
  // sees them in the wrong place.
  addPass(createMidenArgumentMove());
  // Set the p2align operands. This information is present during ISel, however
  // it's inconvenient to collect. Collect it now, and update the immediate
  // operands.
  addPass(createMidenSetP2AlignOperands());

  // Eliminate range checks and add default targets to br_table instructions.
  addPass(createMidenFixBrTableDefaults());

  return false;
}

void MidenPassConfig::addPostRegAlloc() {
  // TODO: The following CodeGen passes don't currently support code containing
  // virtual registers. Consider removing their restrictions and re-enabling
  // them.

  // These functions all require the NoVRegs property.
  disablePass(&MachineCopyPropagationID);
  disablePass(&PostRAMachineSinkingID);
  disablePass(&PostRASchedulerID);
  disablePass(&FuncletLayoutID);
  disablePass(&StackMapLivenessID);
  disablePass(&LiveDebugValuesID);
  disablePass(&PatchableFunctionID);
  disablePass(&ShrinkWrapID);

  // This pass hurts code size for miden because it can generate irreducible
  // control flow.
  disablePass(&MachineBlockPlacementID);

  TargetPassConfig::addPostRegAlloc();
}

void MidenPassConfig::addPreEmitPass() {
  TargetPassConfig::addPreEmitPass();

  // Nullify DBG_VALUE_LISTs that we cannot handle.
  addPass(createMidenNullifyDebugValueLists());

  // Eliminate multiple-entry loops.
  addPass(createMidenFixIrreducibleControlFlow());

  // Do various transformations for exception handling.
  // Every CFG-changing optimizations should come before this.
  if (TM->Options.ExceptionModel == ExceptionHandling::Miden)
    addPass(createMidenLateEHPrepare());

  // Now that we have a prologue and epilogue and all frame indices are
  // rewritten, eliminate SP and FP. This allows them to be stackified,
  // colored, and numbered with the rest of the registers.
  addPass(createMidenReplacePhysRegs());

  // Preparations and optimizations related to register stackification.
  if (getOptLevel() != CodeGenOpt::None) {
    // Depend on LiveIntervals and perform some optimizations on it.
    addPass(createMidenOptimizeLiveIntervals());

    // Prepare memory intrinsic calls for register stackifying.
    addPass(createMidenMemIntrinsicResults());

    // Mark registers as representing miden's value stack. This is a key
    // code-compression technique in Miden. We run this pass (and
    // MemIntrinsicResults above) very late, so that it sees as much code as
    // possible, including code emitted by PEI and expanded by late tail
    // duplication.
    addPass(createMidenRegStackify());

    // Run the register coloring pass to reduce the total number of registers.
    // This runs after stackification so that it doesn't consider registers
    // that become stackified.
    addPass(createMidenRegColoring());
  }

  // Sort the blocks of the CFG into topological order, a prerequisite for
  // BLOCK and LOOP markers.
  addPass(createMidenCFGSort());

  // Insert BLOCK and LOOP markers.
  addPass(createMidenCFGStackify());

  // Insert explicit local.get and local.set operators.
  if (!MidenDisableExplicitLocals)
    addPass(createMidenExplicitLocals());

  // Lower br_unless into br_if.
  addPass(createMidenLowerBrUnless());

  // Perform the very last peephole optimizations on the code.
  if (getOptLevel() != CodeGenOpt::None)
    addPass(createMidenPeephole());

  // Create a mapping from LLVM CodeGen virtual registers to miden registers.
  addPass(createMidenRegNumbering());

  // Fix debug_values whose defs have been stackified.
  if (!MidenDisableExplicitLocals)
    addPass(createMidenDebugFixup());

  // Collect information to prepare for MC lowering / asm printing.
  addPass(createMidenMCLowerPrePass());
}

bool MidenPassConfig::addPreISel() {
  TargetPassConfig::addPreISel();
  addPass(createMidenLowerRefTypesIntPtrConv());
  return false;
}

yaml::MachineFunctionInfo *
MidenTargetMachine::createDefaultFuncInfoYAML() const {
  return new yaml::MidenFunctionInfo();
}

yaml::MachineFunctionInfo *
MidenTargetMachine::convertFuncInfoToYAML(const MachineFunction &MF) const {
  const auto *MFI = MF.getInfo<MidenFunctionInfo>();
  return new yaml::MidenFunctionInfo(*MFI);
}

bool MidenTargetMachine::parseMachineFunctionInfo(
    const yaml::MachineFunctionInfo &MFI, PerFunctionMIParsingState &PFS,
    SMDiagnostic &Error, SMRange &SourceRange) const {
  const auto &YamlMFI = static_cast<const yaml::MidenFunctionInfo &>(MFI);
  MachineFunction &MF = PFS.MF;
  MF.getInfo<MidenFunctionInfo>()->initializeBaseYamlFields(YamlMFI);
  return false;
}
