#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>
#include "../include/KaleidoscopeJIT.h"

static llvm::LLVMContext TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static llvm::IRBuilder<> Builder(TheContext);
static std::map<std::string, llvm::Value*> NamedValues;
static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;

llvm::Value *LogErrorV(const char *Str);
static void InitializeModuleAndPassManager();

enum Token {
  tok_eof = -1,
  tok_def = -2,
  tok_extern = -3,
  tok_identifier = -4,
  tok_number = -5,
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
};

static std::string IdentifierStr;
static double NumVal;

static int gettok() {
  static int LastChar = ' ';

  while (isspace(LastChar)) {
    LastChar = getchar();
  }

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar()))) {
      IdentifierStr += LastChar;
    }
    if (IdentifierStr == "def") {
      return tok_def;
    }
    if (IdentifierStr == "extern") {
      return tok_extern;
    }
    if (IdentifierStr == "if") {
      return tok_if;
    }
    if (IdentifierStr == "then") {
      return tok_then;
    }
    if (IdentifierStr == "else") {
      return tok_else;
    }
    return tok_identifier;
  }

  if (isdigit(LastChar)) {
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
  }

  if (LastChar == '#') {
    do {
      LastChar = getchar();
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF) {
      return gettok();
    }
  }

  if (LastChar == EOF) {
    return tok_eof;
  }

  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}
llvm::Function *getFunction(std::string Name);

class ExprAST {
public:
  virtual ~ExprAST() {}
  virtual llvm::Value *codegen() = 0;
};

class NumberExprAST: public ExprAST {
  double Val;
public:
  NumberExprAST(double Val): Val(Val) {}
  virtual llvm::Value *codegen();
};

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
}

class VariableExprAST: public ExprAST {
  std::string Name;
public:
  VariableExprAST(std::string &Name): Name(Name) {}
  virtual llvm::Value *codegen();
};

llvm::Value *VariableExprAST::codegen() {
  llvm::Value *V = NamedValues[Name];
  if (!V) {
    return LogErrorV("Unknown variable name");
  }
  return V;
}

class BinaryExprAST: public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS):
    Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  virtual llvm::Value *codegen();
};

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R) {
    return nullptr;
  }

  switch (Op) {
  case '+':
    return Builder.CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder.CreateFSub(L, R, "subtmp");
  case '*':
    return Builder.CreateFMul(L, R, "multmp");
  case '<':
    L = Builder.CreateFCmpULT(L, R, "cmptmp");
    // convert 1 or 0 to 1.0 or 0.0
    return Builder.CreateUIToFP(L, llvm::Type::getDoubleTy(TheContext), "booltmp");
  default:
    return nullptr;
  }
}

class CallExprAST: public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST> > Args;
public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST> > Args):
    Callee(Callee), Args(std::move(Args)) {}
  virtual llvm::Value *codegen();
};

llvm::Value *CallExprAST::codegen() {
  llvm::Function *CalleeF = getFunction(Callee);
  if (!CalleeF) {
    return LogErrorV("Unknown function referenced");
  }

  if (CalleeF->arg_size() != Args.size()) {
    return LogErrorV("Incorrect number of arguments passed");
  }

  std::vector<llvm::Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back()) {
      return nullptr;
    }
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

class PrototypeAST: public ExprAST {
  std::string Name;
  std::vector<std::string> Args;
public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args):
    Name(Name), Args(std::move(Args)) {};
  virtual llvm::Function *codegen();
  const std::string &getName() const { return Name; }
  std::string getArg(unsigned Idx) { return Args[Idx]; }
  unsigned getArgCount() { return Args.size(); }
  std::vector<std::string> getArgs() { return Args; }
};

llvm::Function *PrototypeAST::codegen() {
  std::vector<llvm::Type *> Doubles(Args.size(), llvm::Type::getDoubleTy(TheContext));
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(TheContext), Doubles, false);
  llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

  unsigned Idx = 0;
  for(auto &Arg : F->args()) {
    Arg.setName(Args[Idx++]);
  }

  return F;
}

static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

class FunctionAST: public ExprAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body):
    Proto(std::move(Proto)), Body(std::move(Body)) {}
  virtual llvm::Function *codegen();
};

llvm::Function *FunctionAST::codegen() {
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  llvm::Function *TheFunction = getFunction(P.getName());
  if (!TheFunction) {
    return nullptr;
  }
  
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  NamedValues.clear();
  for(auto &Arg : TheFunction->args()) {
    NamedValues[Arg.getName()] = &Arg;
  }

  if (llvm::Value *RetVal = Body->codegen()) {
    Builder.CreateRet(RetVal);
    verifyFunction(*TheFunction);
    TheFPM->run(*TheFunction);
    return TheFunction;
  }

  TheFunction->eraseFromParent();
  return nullptr;
}

class IfExprAST: public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;
public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else):
    Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {};
  virtual llvm::Value *codegen();
};

llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV) {
    return nullptr;
  }

  CondV = Builder.CreateFCmpONE(CondV, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "ifcond");
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

  Builder.CreateCondBr(CondV, ThenBB, ElseBB);

  Builder.SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen();
  if (!ThenV) {
    return nullptr;
  }

  Builder.CreateBr(MergeBB);

  ThenBB = Builder.GetInsertBlock();
  TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder.SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen();
  if (!ElseV) {
    return nullptr;
  }

  Builder.CreateBr(MergeBB);

  ElseBB = Builder.GetInsertBlock();

  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder.SetInsertPoint(MergeBB);
  llvm::PHINode *PN = Builder.CreatePHI(llvm::Type::getDoubleTy(TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Function *getFunction(std::string Name) {
  if (auto *F = TheModule->getFunction(Name)) {
    return F;
  }

  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end()) {
    return FI->second->codegen();
  }

  return nullptr;
}

static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "LogError: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = llvm::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(Result);
}

// TODO: move
static std::unique_ptr<ExprAST> ParseExpression();

static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken();
  auto V = ParseExpression();
  if (!V) {
    return nullptr;
  }

  if (CurTok != ')') {
    return LogError("Expected ')'");
  }
  getNextToken();
  return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken();

  if (CurTok != '(') {
    return llvm::make_unique<VariableExprAST>(IdName);
  }

  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (1) {
      if (auto Arg = ParseExpression()) {
        Args.push_back(std::move(Arg));
      } else {
        return nullptr;
      }

      if (CurTok == ')') {
        break;
      }

      if (CurTok != ',') {
        return LogError("Expected ',' or ')' in argument list");
      }

      getNextToken();
    }
  }
  getNextToken();
  return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParseIfExpr();

static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    printf("unknown token: %d\n", CurTok);
    return LogError("unknown token while parsing expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  }
}

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
  if (!isascii(CurTok)) {
    return -1;
  }

  int TokPrec = BinopPrecedence[CurTok];

  if (TokPrec <= 0) {
    return -1;
  }

  return TokPrec;
}

// TODO: move
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);

static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS) {
    return nullptr;
  }

  return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
  while (1) {
    int TokPrec = GetTokPrecedence();

    if (TokPrec < ExprPrec) {
      return LHS;
    }

    int BinOp = CurTok;
    getNextToken();

    auto RHS = ParsePrimary();
    if (!RHS) {
      return nullptr;
    }

    int NextPerc = GetTokPrecedence();
    if (TokPrec < NextPerc) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS) {
        return nullptr;
      }
    }
    LHS = llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (CurTok != tok_identifier) {
    return LogErrorP("Expected function name in prototype");
  }

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(') {
    return LogErrorP("Exprected '(' in prototype");
  }

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier) {
    ArgNames.push_back(IdentifierStr);
  }
  if (CurTok != ')') {
    return LogErrorP("Expected ')' in prototype");
  }

  getNextToken();
  return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat 'def'
  auto Proto = ParsePrototype();
  if (!Proto) {
    return nullptr;
  }

  if (auto E = ParseExpression()) {
    return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }

  return nullptr;
}

static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat 'extern'
  return ParsePrototype();
}

static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken();

  auto Cond = ParseExpression();
  if (!Cond) {
    return nullptr;
  }

  if (CurTok != tok_then) {
    return LogError("expected then");
  }
  getNextToken();

  auto Then = ParseExpression();
  if (!Then) {
    return nullptr;
  }

  if (CurTok != tok_else) {
    return LogError("expected else");
  }
  getNextToken();

  auto Else = ParseExpression();
  if (!Else) {
    return nullptr;
  }

  return llvm::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
    return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

static void InitializeModuleAndPassManager() {
  TheModule = llvm::make_unique<llvm::Module>("my lonely module jit", TheContext);
  TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

  TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
  TheFPM->add(llvm::createInstructionCombiningPass());
  TheFPM->add(llvm::createReassociatePass());
  TheFPM->add(llvm::createGVNPass());
  TheFPM->add(llvm::createCFGSimplificationPass());

  TheFPM->doInitialization();
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:\n");
      FnIR->dump();
      TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();
    }
  } else {
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern:\n");
      FnIR->dump();
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Parsed a top-level expression\n");
      FnIR->dump();

      auto H = TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();

      auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
      assert(ExprSymbol && "Function not found");

      double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
      fprintf(stderr, "Evaluated to %f\n", FP());

      TheJIT->removeModule(H);
    }
  } else {
    getNextToken();
  }
}

static void MainLoop() {
  while(1) {
    fprintf(stderr, "ready> ");
    switch(CurTok) {
    default:
      HandleTopLevelExpression();
      break;
    case tok_eof:
      return;
    case ';':
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    }
  }
}

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 30;
  BinopPrecedence['*'] = 40;

  fprintf(stderr, "ready >");
  getNextToken();

  TheJIT = llvm::make_unique<llvm::orc::KaleidoscopeJIT>();
  InitializeModuleAndPassManager();

  MainLoop();

  TheModule->dump();

  return 0;
}

extern "C" double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}
