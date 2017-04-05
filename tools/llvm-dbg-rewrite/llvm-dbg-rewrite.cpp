//===--- llvm-as.cpp - The low-level LLVM assembler -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This utility may be invoked in the following manner:
//   llvm-dbg-rewrite --help         - Output information about command line switches
//   llvm-dbg-rewrite [options]      - Read LLVM asm from stdin, write bitcode to stdout
//   llvm-as [options] x.ll - Read LLVM asm from the x.ll file, write bitcode
//                            to the x.bc file.
//
//===----------------------------------------------------------------------===//

#include "llvm/AsmParser/Parser.h"
#include "llvm/AsmParser/LLParser.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include <libgen.h>
#include <memory>
using namespace llvm;

static char* BaseName = NULL;
static char* DirName = NULL;
static DICompileUnit* CurDICompileUnit = NULL;
static DIFile* CurDIFile = NULL;
static DISubprogram* CurDIFunction = NULL;

static llvm::DIBuilder* DebugInfo = NULL;

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<input .llvm file>"),
                                          cl::init("-"));

static cl::opt<std::string> OutputFilename("o",
                                           cl::desc("Override output filename"),
                                           cl::value_desc("filename"));

static cl::opt<bool> Force("f", cl::desc("Enable binary output on terminals"));

static cl::opt<bool> DisableOutput("disable-output", cl::desc("Disable output"),
                                   cl::init(false));

static cl::opt<bool> EmitModuleHash("module-hash", cl::desc("Emit module hash"),
                                    cl::init(false));

static cl::opt<bool> DumpAsm("d", cl::desc("Print assembly as parsed"),
                             cl::Hidden);

static cl::opt<bool>
    DisableVerify("disable-verify", cl::Hidden,
                  cl::desc("Do not run verifier on input LLVM (dangerous!)"));

static cl::opt<bool> PreserveBitcodeUseListOrder(
    "preserve-bc-uselistorder",
    cl::desc("Preserve use-list order when writing LLVM bitcode."),
    cl::init(true), cl::Hidden);

static void WriteOutputFile(const Module *M) {
  // Infer the output filename if needed.
  if (OutputFilename.empty()) {
    if (InputFilename == "-") {
      OutputFilename = "-";
    } else {
      StringRef IFN = InputFilename;
      OutputFilename = (IFN.endswith(".ll") ? IFN.drop_back(3) : IFN).str();
      OutputFilename += ".bc";
    }
  }

  std::error_code EC;
  std::unique_ptr<tool_output_file> Out(
      new tool_output_file(OutputFilename, EC, sys::fs::F_None));
  if (EC) {
    errs() << EC.message() << '\n';
    exit(1);
  }

  if (Force || !CheckBitcodeOutputToConsole(Out->os(), true))
    WriteBitcodeToFile(M, Out->os(), PreserveBitcodeUseListOrder, nullptr,
                       EmitModuleHash);

  // Declare success.
  Out->keep();
}

void parseFunctionBodyCallback(Module *M, Function *Fn, LLLexer &Lex) {
  std::string name(Fn->getName().str());
  //  printf("Fn      |%s|\n", name.c_str());
  DISubroutineType* subtype = DebugInfo->createSubroutineType(DebugInfo->getOrCreateTypeArray(None));
  CurDIFunction = DebugInfo->createFunction(CurDICompileUnit,name,name,CurDIFile,Lex.SM.FindLineNumber(Lex.getLoc()),subtype,true,true,0);
  //  printf("%s:%d   Variables -> %p\n", __FILE__, __LINE__, (void*)CurDIFunction->getRawVariables());
}

void parseInstructionCallback(Module *M, Instruction *Inst, LLLexer &Lex) {
  SMLoc loc = Lex.getLoc();
  unsigned ln = Lex.SM.FindLineNumber(loc)-1;
  llvm::DebugLoc dl = llvm::DebugLoc::get(ln, 0, CurDIFunction);
  Inst->setDebugLoc(dl);
  //  printf("Instr ln:%d %s\n", ln, Inst->getName().str().c_str());
}

bool parseAssemblyIntoLocally(MemoryBufferRef F, Module &M, SMDiagnostic &Err,
				    SlotMapping *Slots) {
  SourceMgr SM;
  std::unique_ptr<MemoryBuffer> Buf = MemoryBuffer::getMemBuffer(F);
  
  SM.AddNewSourceBuffer(std::move(Buf), SMLoc());

  return LLParser(F.getBuffer(), SM, Err, &M, Slots,
		  parseFunctionBodyCallback,
		  parseInstructionCallback ).Run();
}


std::unique_ptr<Module> parseAssemblyLocally(MemoryBufferRef F,
						   SMDiagnostic &Err,
						   LLVMContext &Context,
						   SlotMapping *Slots) {
  std::unique_ptr<Module> M =
      make_unique<Module>(F.getBufferIdentifier(), Context);

  DebugInfo = new DIBuilder(*M,true);
  CurDIFile = DebugInfo->createFile(BaseName,DirName);
  CurDICompileUnit = DebugInfo->createCompileUnit(dwarf::DW_LANG_C,
						  CurDIFile,
						  "???",
						  true,
						  "",
						  0);
  
  if (parseAssemblyIntoLocally(F, *M, Err, Slots))
    return nullptr;

  return M;
}

std::unique_ptr<Module> parseAssemblyFileLocally(StringRef Filename,
						 SMDiagnostic &Err,
						 LLVMContext &Context,
						 SlotMapping *Slots) {
  ErrorOr<std::unique_ptr<MemoryBuffer>> FileOrErr =
      MemoryBuffer::getFileOrSTDIN(Filename);
  if (std::error_code EC = FileOrErr.getError()) {
    Err = SMDiagnostic(Filename, SourceMgr::DK_Error,
                       "Could not open input file: " + EC.message());
    return nullptr;
  }
  return parseAssemblyLocally(FileOrErr.get()->getMemBufferRef(), Err, Context, Slots);
}

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  LLVMContext Context;
  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.
  cl::ParseCommandLineOptions(argc, argv, "llvm .ll -> .bc assembler\n");

  // Parse the file now...
  SMDiagnostic Err;
  std::unique_ptr<Module> preM = parseAssemblyFile(InputFilename, Err, Context);
  if (!preM.get()) {
    Err.print(argv[0], errs());
    return 1;
  }

  NamedMDNode* node = preM->getNamedMetadata("llvm.dbg.cu");
  if (node!=NULL) {
      errs() << argv[0]
	     << ": assembly parsed, but it already contains debug information - strip it and try again!\n";
      return 1;
  }

  // Get the file truepath
  const char* rp = realpath(InputFilename.c_str(),NULL);
  char* brp = strdup(rp);
  char* drp = strdup(rp);
  BaseName = basename(brp);
  DirName = dirname(drp);
  
  // Here we parse the file again with callbacks

  std::unique_ptr<Module> M = parseAssemblyFileLocally(InputFilename, Err, Context, NULL);

  DebugInfo->finalize();
  //  M.get()->dump();
  

  if (!DisableVerify) {
    std::string ErrorStr;
    raw_string_ostream OS(ErrorStr);
    if (verifyModule(*M.get(), &OS)) {
      errs() << argv[0]
             << ": assembly parsed, but does not verify as correct!\n";
      errs() << OS.str();
      return 1;
    }
  }

  if (DumpAsm)
    errs() << "Here's the assembly:\n" << *M.get();

  if (!DisableOutput)
    WriteOutputFile(M.get());
  return 0;
}
