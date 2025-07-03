#include "llracket/CodeGen/CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>
#include <map>

using namespace llvm;

namespace {
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int64Ty;
  Type *Int32Ty;
  Type *Int1Ty;
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<Function *> funcmap;
  std::vector<StringMap<Value *>> nameMapStack;
  std::vector<Var> vars;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int1Ty = Type::getInt1Ty(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int64Ty = Type::getInt64Ty(M->getContext());
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);

    nameMapStack.push_back(StringMap<Value *>()); // Global scope
  }

  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);
    Tree->accept(*this);

    if (V) {
      Type *outtype = V->getType();

      // print outtype
      llvm::errs() << "outtype: " << *outtype << "\n";

      FunctionType *WriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      Function *WriteFn = Function::Create(WriteFnTy, GlobalValue::ExternalLinkage, (outtype == Int32Ty ? "write_int" : "write_bool"), M);
      
      Builder.CreateCall(WriteFnTy, WriteFn, {V});
    }
    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Program &Node) override { 
    BasicBlock * main = Builder.GetInsertBlock();
    for (auto &i:Node.getfunctions())
      CreateFuncDef(i);
    for (auto &i:Node.getfunctions())
      i->accept(*this);
    Builder.SetInsertPoint(main);
    Node.getExpr()->accept(*this); 
  };

  virtual void visit(Expr &Node) override {
    if (llvm::isa<Prim>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<If>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Bool>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<WhileLoop>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Begin>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<SetBang>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Void>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Apply>(Node)) {
      Node.accept(*this);
      return;
    }
  };

  virtual void visit(Prim &Node) override {
    if (Node.getOp() == tok::read) {
      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_value")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {Int32Ty}, false);
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage,
                                  "read_value", M);
      }
      // AllocaInst *ReadInput =
      //     Builder.CreateAlloca(PtrTy, nullptr, "read_input");
      if (Node.getType() == TypeInt) {
        V = Builder.CreateCall(ReadFn, {ConstantInt::get(Int32Ty, 0, true)});
      }
      else if (Node.getType() == TypeBool) {
        // llvm::errs() << "Desperation" << "\n";
        Value* readResult = nullptr;
        readResult = Builder.CreateCall(ReadFn, {ConstantInt::get(Builder.getTrue()->getType(), 1, true)});

        // convert the i32 return type of the read fn to i1
        V = Builder.CreateICmpNE(readResult, Int32Zero, "read_val_is_true");
      }
      
      return;
    }
    if (Node.getOp() == tok::minus) {
      if (Node.getE1() and !Node.getE2()) {
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V);
        return;
      }
    }
    if (Node.getOp() == tok::plus || Node.getOp() == tok::minus) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      if (Node.getOp() == tok::plus) {
        V = Builder.CreateNSWAdd(E1, E2);
      } else {
        V = Builder.CreateNSWSub(E1, E2);
      }
      return;
    }
    if (Node.getOp() == tok::cmp_not) {
      Node.getE1()->accept(*this);
      V = Builder.CreateNot(V);
    }
    if (Node.getOp() == tok::cmp_and ) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Function *parent = Builder.GetInsertBlock()->getParent();
      BasicBlock *condblock = Builder.GetInsertBlock();

      BasicBlock *trueblock = BasicBlock::Create(M->getContext(),"true",parent);
      BasicBlock *mergeblock = BasicBlock::Create(M->getContext(),"merge",parent);
      Builder.CreateCondBr(E1,trueblock,mergeblock);

      Builder.SetInsertPoint(trueblock);
      Node.getE2()->accept(*this);
      Value *E2 = V;
      Builder.CreateBr(mergeblock);
      trueblock = Builder.GetInsertBlock();
      
      // parent->insert(parent->end(),mergeblock);
      Builder.SetInsertPoint(mergeblock);
      PHINode *phiV = Builder.CreatePHI(Type::getInt1Ty(M->getContext()),2, "phiV");
      phiV->addIncoming(E2, trueblock);
      phiV->addIncoming(E1, condblock);
      V = phiV;
    }
    if (Node.getOp() == tok::cmp_or ) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Function *parent = Builder.GetInsertBlock()->getParent();
      BasicBlock *condblock = Builder.GetInsertBlock();

      BasicBlock *falseblock = BasicBlock::Create(M->getContext(),"false",parent);
      BasicBlock *mergeblock = BasicBlock::Create(M->getContext(),"merge",parent);
      Builder.CreateCondBr(E1,mergeblock,falseblock);

      Builder.SetInsertPoint(falseblock);
      Node.getE2()->accept(*this);
      Value *E2 = V;
      Builder.CreateBr(mergeblock);
      falseblock = Builder.GetInsertBlock();
      
      // parent->insert(parent->end(),mergeblock);
      Builder.SetInsertPoint(mergeblock);
      PHINode *phiV = Builder.CreatePHI(Type::getInt1Ty(M->getContext()),2, "phiV");
      phiV->addIncoming(E2, falseblock);
      phiV->addIncoming(E1, condblock);
      V = phiV;
    }
    if ( Node.getOp() == tok::equal || Node.getOp() == tok::lt || Node.getOp() == tok::lte || Node.getOp() == tok::gt || Node.getOp() == tok::gte) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      if(Node.getOp() == tok::equal)
        V = Builder.CreateICmpEQ(E1,E2);
      if(Node.getOp() == tok::lt)
        V = Builder.CreateICmpSLT(E1,E2);
      if(Node.getOp() == tok::lte)
        V = Builder.CreateICmpSLE(E1,E2);
      if(Node.getOp() == tok::gt)
        V = Builder.CreateICmpSGT(E1,E2);
      if(Node.getOp() == tok::gte)
        V = Builder.CreateICmpSGE(E1,E2);
    }
    if ( Node.getOp() == tok::vec) {
      // I think this overwrites stuff, im changing it
      ArrayType *vectype = llvm::ArrayType::get(Int64Ty,Node.getE().size());
      Function *TheFunction = Builder.GetInsertBlock()->getParent();
      IRBuilder<> TmpBuilder(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

      AllocaInst *Alloca = TmpBuilder.CreateAlloca(vectype);
      unsigned int index = 0;
      for (auto &i : Node.getE()) {
        i->accept(*this);
        Value* elementV = V;
        Value * castV = nullptr;
        Type *elementType = elementV->getType();
        if (elementType->isPointerTy()) {
          castV = Builder.CreatePtrToInt(elementV, Int64Ty); // pointer cast
        }
        else if (elementType == Int32Ty) {
          castV = Builder.CreateIntCast(elementV,Int64Ty,true);
        }
        else if (elementType == Int1Ty) {
          castV = Builder.CreateIntCast(elementV,Int64Ty,false);
        }

        // address of element at current index
        Value *ElementPtr = Builder.CreateGEP(vectype, Alloca, {Int32Zero, ConstantInt::get(Int32Ty, index)});

        Builder.CreateStore(castV, ElementPtr);

        index++;
      }
      V = Alloca;
      return;
    }
    //TODO: Fix this mess of a function
    if ( Node.getOp() == tok::veclen) {
      Node.getE1()->accept(*this);
      V = ConstantInt::get(Int32Ty,Node.getE1()->getElementTypes().size());
      return;
    }
    if ( Node.getOp() == tok::vecref) {
      Node.getindex()->accept(*this);
      Value *index = V;
      Node.getE1()->accept(*this);

      Value *vecPtr = V; //  pointer to allocated array
      Value *ElementPtr = Builder.CreateGEP(ArrayType::get(Int64Ty,Node.getE1()->getElementTypes().size()),vecPtr,{Int32Zero,index});
      Value *LoadedVal = Builder.CreateLoad(Int64Ty,ElementPtr);
      //TODO: Type change back to bool or int by checking the type it's supposed to be. Otherwise it will always write bool
      //DONE
      if (Node.getType() == TypeInt) {
        V = Builder.CreateIntCast(LoadedVal, Int32Ty, true, "int_cast");
      }
      else if (Node.getType() == TypeBool) {
        // cast loaded i64 to bool. (this evaluates to true if not 0)
        V = Builder.CreateICmpNE(LoadedVal, ConstantInt::get(Int64Ty, 0, true), "bool_cast");
      }
      else if (Node.getType() == TypeVector) {
        // cast loaded i64 back to pointer to vector
        V = Builder.CreateIntToPtr(LoadedVal, PtrTy, "vector_ptr_cast");
      }

      return;
    }
    if ( Node.getOp() == tok::vecset) {
      Node.getindex()->accept(*this);
      Value *index = V;
      Node.getE1()->accept(*this);

      Value *vecPtr = V; //  pointer to allocated array
      Value *ElementPtr = Builder.CreateGEP(ArrayType::get(Int64Ty,Node.getE1()->getElementTypes().size()),vecPtr,{Int32Zero,index});
      Node.getE2()->accept(*this);
      Value* newVal = V;
      Value* valToStore = nullptr;

      // Cast the new value to i64 before storing
      if (!newVal) {
           llvm::errs() << "Warning: vector-set! storing void/null value.\n";
           valToStore = ConstantInt::get(Int64Ty, 0); // Store 0 for void/null
      } else {
          Type* newValType = newVal->getType();
          if (newValType->isPointerTy()) {
              valToStore = Builder.CreatePtrToInt(newVal, Int64Ty, "ptr_to_int_cast");
          } else if (newValType == Int32Ty) {
              valToStore = Builder.CreateIntCast(newVal, Int64Ty, true);
          } else if (newValType == Int1Ty) {
              valToStore = Builder.CreateIntCast(newVal, Int64Ty, false);
          } else if (newValType == Int64Ty) {
              valToStore = newVal; // Already i64
          }
      }

      // Store the i64 value
      Builder.CreateStore(valToStore, ElementPtr);
      V = nullptr;
      // Builder.CreateStore(V,loc);
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
  };

  virtual void visit(Bool &Node) override {
    StringRef b = Node.getValue();
    if (b[1] == 'f')
      V = Builder.getFalse();
    else if (b[1] == 't')
      V = Builder.getTrue();
  }

  virtual void visit(Void &Node) override {
    V = nullptr;
    return;
  }

  virtual void visit(Var &Node) override {
    // Node.getValue()->accept(*this);

    if (funcmap.count(Node.getname())) {
      V = funcmap[Node.getname()];
      return;
    }
    for (auto scopeIter = nameMapStack.rbegin(); scopeIter != nameMapStack.rend(); ++scopeIter) {
      auto &scope = *scopeIter;
      if (scope.count(Node.getname())) {
        Value *VarPtr = scope[Node.getname()];
        Type *AllocatedTy = cast<AllocaInst>(VarPtr)->getAllocatedType();
        V = Builder.CreateLoad(AllocatedTy, VarPtr, Node.getname());
        return;
      }
    }
  
    // var list if not found in scope list
    // for (auto i = vars.rbegin(); i != vars.rend(); ++i) {
    //   if (i->getname() == Node.getname()) {
    //     i->getValue()->accept(*this);
    //     return;
    //   }
    // }
  }

  virtual void visit(Let &Node) override {
    // Do RHS
    Node.getvarval()->accept(*this);
    Value *InitVal = V;
    if(!InitVal) {
      Node.getexp()->accept(*this);
      return;
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    IRBuilder<> TmpBuilder(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    
    AllocaInst *Alloca = TmpBuilder.CreateAlloca(InitVal->getType(), nullptr, Node.getvar()->getname());

    Builder.CreateStore(InitVal, Alloca);

    // enter scope
    nameMapStack.push_back(nameMapStack.back());
    nameMapStack.back()[Node.getvar()->getname()] = Alloca;

    Node.getexp()->accept(*this);

    // pop after scope done
    nameMapStack.pop_back();
  }

  virtual void visit(If &Node) override {
    Node.getcond()->accept(*this);
    // Value *E = Builder.CreateFCmpONE(V,ConstantFP::get(M->getContext(),APFloat(0.0)),"ifcond");
    Value *E = V;
    Function *parent = Builder.GetInsertBlock()->getParent();

    BasicBlock *trueblock = BasicBlock::Create(M->getContext(),"true",parent);
    BasicBlock *falseblock = BasicBlock::Create(M->getContext(),"false",parent);
    BasicBlock *mergeblock = BasicBlock::Create(M->getContext(),"merge",parent);
    Builder.CreateCondBr(E,trueblock,falseblock);

    Builder.SetInsertPoint(trueblock);
    Node.getifexpr()->accept(*this);
    Value *trueV = V;
    Builder.CreateBr(mergeblock);
    trueblock = Builder.GetInsertBlock();
    //original
    Type *outtype = V->getType();
    //original

    // print outtype
    // llvm::errs() << "outtype: " << *outtype << "\n";

    Builder.SetInsertPoint(falseblock);
    Node.getelseexpr()->accept(*this);
    //test
    // Type *outtype = V->getType();
    //test
    Value *falseV = V;
    Builder.CreateBr(mergeblock);
    falseblock = Builder.GetInsertBlock();
    
    // parent->insert(parent->end(),mergeblock);
    Builder.SetInsertPoint(mergeblock);
    PHINode *phiV = Builder.CreatePHI(outtype,2, "phiV");
    phiV->addIncoming(trueV, trueblock);
    phiV->addIncoming(falseV, falseblock);
    V = phiV;
  }

  virtual void visit(WhileLoop &Node) override {
    Function *parent = Builder.GetInsertBlock()->getParent();

    BasicBlock *condblock = BasicBlock::Create(M->getContext(),"cond",parent);
    BasicBlock *loopblock = BasicBlock::Create(M->getContext(),"loop",parent);
    BasicBlock *exitblock = BasicBlock::Create(M->getContext(),"exit",parent);
    Builder.CreateBr(condblock);

    Builder.SetInsertPoint(condblock);
    Node.getcond()->accept(*this);
    Value *E = V;
    Builder.CreateCondBr(E,loopblock,exitblock);
    condblock = Builder.GetInsertBlock();

    Builder.SetInsertPoint(loopblock);
    Node.getloop()->accept(*this);
    Builder.CreateBr(condblock);
    loopblock = Builder.GetInsertBlock();

    Builder.SetInsertPoint(exitblock);
    V = nullptr;
  }
  
  virtual void visit(Begin &Node) override {
    for (auto & i:Node.getsubexpr())
      i->accept(*this);
    Node.getfinalexpr()->accept(*this);
  }

  virtual void visit(SetBang &Node) override {
    Node.getExpr()->accept(*this);
    Value *NewVal = V;

    StringRef VarName = Node.getVar()->getname();

    for (auto scopeIter = nameMapStack.rbegin(); scopeIter != nameMapStack.rend(); ++scopeIter) {
      auto &scope = *scopeIter;
      if (scope.count(VarName)) {
        Value *VarPtr = scope[VarName];
        Builder.CreateStore(NewVal, VarPtr);

        // Set returns void
        // Void with 0?
        // V = ConstantInt::get(Int32Ty, 0, true);
        V = nullptr;
        return;
      }
    }
    // else {
    //   llvm::errs() << "Error: Cannot set! undefined variable" << VarName << "\n";
    // }
  }

  FunctionType * getfuncNodeType(ASTType *Node) {
    std::vector<Type *> intypes;
    Type * outtype = getNodeType(Node->getouttype());
    if (Node->getintypes().size() == 0)
      return llvm::FunctionType::get(outtype,false);
    for (auto &i:Node->getintypes()) {
      intypes.push_back(getNodeType(i));
    }
    return llvm::FunctionType::get(outtype,intypes,false);
  }
  Type * getNodeType(ASTType *Node) {
    Type * outtype = nullptr;
    if (Node->isfuntype())
      outtype = PointerType::getUnqual(getfuncNodeType(Node));
    else if (Node->isvectype()){
      ArrayType* arrayType = llvm::ArrayType::get(Int64Ty, Node->gettypevec().size());
      outtype = PointerType::getUnqual(arrayType);
    }
    else if(Node->getval()=="Integer")
      outtype = Int32Ty;
    else if(Node->getval()=="Boolean")
      outtype = Int1Ty;
    else if(Node->getval()=="Void")
      outtype = VoidTy;
    return outtype;
  }

  void CreateFuncDef(Define *Node) {
    StringRef funcname = Node->getfuncname()->getname();
    ASTType * astfunctype = Node->getfunctype();
    FunctionType * functype = getfuncNodeType(astfunctype);
    Function * func = llvm::Function::Create(functype,GlobalValue::ExternalLinkage,"func_"+funcname,M);
    funcmap[funcname] = func;
  }

  virtual void visit(Define &Node) override {
    StringRef funcname = Node.getfuncname()->getname();
    Function * func = funcmap[funcname];
    BasicBlock * funblock = BasicBlock::Create(M->getContext(),funcname+"_entry",func);
    Builder.SetInsertPoint(funblock);
    int curarg = 0;
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    IRBuilder<> TmpBuilder(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    AllocaInst *Alloca;
    StringMap<Value *> varmap;
    for (auto arg = func->arg_begin(); arg!=func->arg_end(); arg++)
    {
      StringRef paramname = Node.getinvars()[curarg++]->getname();
      Alloca = TmpBuilder.CreateAlloca(arg->getType(), nullptr, paramname);
      Builder.CreateStore(arg,Alloca);
      varmap[paramname] = Alloca; 
    }
    nameMapStack.push_back(varmap);
    Node.getbody()->accept(*this);
    nameMapStack.pop_back();
    Builder.CreateRet(V);
  }
  virtual void visit(Apply &Node) override {
    Node.getfuncname()->accept(*this);
    Value * func = V;
    std::vector<Value *> params;
    for (auto &i:Node.getparams())
    {
      i->accept(*this);
      params.push_back(V);
    }
    FunctionType * functype = getfuncNodeType(Node.getASTType());
    V = Builder.CreateCall(functype,func,params);
  } 
};
}; // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}
