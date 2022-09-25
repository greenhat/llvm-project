add_llvm_component_library(LLVMMidenAsmParser
  MidenAsmParser.cpp
  MidenAsmTypeCheck.cpp

  LINK_COMPONENTS
  MC
  MCParser
  MidenInfo
  MidenUtils
  Support

  ADD_TO_COMPONENT
  Miden
  )
