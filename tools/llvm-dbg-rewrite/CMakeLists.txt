set(LLVM_LINK_COMPONENTS
  AsmParser
  BitWriter
  Core
  Support
  )

add_llvm_tool(llvm-dbg-rewrite
  llvm-dbg-rewrite.cpp

  DEPENDS
  intrinsics_gen
  )
