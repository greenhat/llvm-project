add_llvm_component_library(LLVMMidenUtils
  MidenUtilities.cpp
  MidenTypeUtilities.cpp

  LINK_COMPONENTS
  CodeGen
  Core
  MC
  Support

  ADD_TO_COMPONENT
  Miden
  )
