add_llvm_component_group(Miden)

set(LLVM_TARGET_DEFINITIONS Miden.td)

tablegen(LLVM MidenGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM MidenGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM MidenGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM MidenGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM MidenGenFastISel.inc -gen-fast-isel)
tablegen(LLVM MidenGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM MidenGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM MidenGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM MidenGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(MidenCommonTableGen)

add_llvm_target(MidenCodeGen
  MidenAddMissingPrototypes.cpp
  MidenArgumentMove.cpp
  MidenAsmPrinter.cpp
  MidenCFGStackify.cpp
  MidenCFGSort.cpp
  MidenDebugFixup.cpp
  MidenDebugValueManager.cpp
  MidenLateEHPrepare.cpp
  MidenExceptionInfo.cpp
  MidenExplicitLocals.cpp
  MidenFastISel.cpp
  MidenFixBrTableDefaults.cpp
  MidenFixIrreducibleControlFlow.cpp
  MidenFixFunctionBitcasts.cpp
  MidenFrameLowering.cpp
  MidenISelDAGToDAG.cpp
  MidenISelLowering.cpp
  MidenInstrInfo.cpp
  MidenLowerBrUnless.cpp
  MidenLowerEmscriptenEHSjLj.cpp
  MidenLowerRefTypesIntPtrConv.cpp
  MidenMachineFunctionInfo.cpp
  MidenMCInstLower.cpp
  MidenMCLowerPrePass.cpp
  MidenNullifyDebugValueLists.cpp
  MidenOptimizeLiveIntervals.cpp
  MidenOptimizeReturned.cpp
  MidenPeephole.cpp
  MidenRegisterInfo.cpp
  MidenRegColoring.cpp
  MidenRegNumbering.cpp
  MidenRegStackify.cpp
  MidenReplacePhysRegs.cpp
  MidenRuntimeLibcallSignatures.cpp
  MidenSelectionDAGInfo.cpp
  MidenSetP2AlignOperands.cpp
  MidenSortRegion.cpp
  MidenMemIntrinsicResults.cpp
  MidenSubtarget.cpp
  MidenTargetMachine.cpp
  MidenTargetObjectFile.cpp
  MidenTargetTransformInfo.cpp

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  BinaryFormat
  CodeGen
  Core
  MC
  Scalar
  SelectionDAG
  Support
  Target
  TransformUtils
  MidenDesc
  MidenInfo
  MidenUtils

  ADD_TO_COMPONENT
  Miden
)

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
add_subdirectory(Utils)
