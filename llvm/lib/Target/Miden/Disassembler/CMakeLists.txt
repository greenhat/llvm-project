add_llvm_component_library(LLVMMidenDisassembler
  MidenDisassembler.cpp

  LINK_COMPONENTS
  MidenDesc
  MCDisassembler
  MidenInfo
  MidenUtils
  Support
  MC

  ADD_TO_COMPONENT
  Miden
  )
