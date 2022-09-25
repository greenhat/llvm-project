add_llvm_component_library(LLVMMidenDesc
  MidenAsmBackend.cpp
  MidenInstPrinter.cpp
  MidenMCAsmInfo.cpp
  MidenMCCodeEmitter.cpp
  MidenMCTargetDesc.cpp
  MidenTargetStreamer.cpp
  MidenMidenObjectWriter.cpp

  LINK_COMPONENTS
  MC
  Support
  MidenInfo
  MidenUtils

  ADD_TO_COMPONENT
  Miden
)
