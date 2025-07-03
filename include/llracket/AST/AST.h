#ifndef LLRACKET_AST_AST_H
#define LLRACKET_AST_AST_H

#include "llracket/Lexer/Token.h"
#include <any>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <vector>

class AST;
class Program;
typedef llvm::StringMap<std::any> ProgramInfo;
class Expr;
class Prim;
class Int;
class Let;
class Var;
class Bool;
class If;
class WhileLoop;
class Begin;
class SetBang;
class Void;
class Define;
class Apply;
class ASTType;

class ASTVisitor {
public:
  virtual ~ASTVisitor() {}
  virtual void visit(Program &) {};
  virtual void visit(Expr &) {};
  virtual void visit(Prim &) {};
  virtual void visit(Int &) = 0;
  virtual void visit(Let &) {};
  virtual void visit(Var &) {};
  virtual void visit(If &) {};
  virtual void visit(Bool &) {};
  virtual void visit(WhileLoop &) {};
  virtual void visit(SetBang &) {};
  virtual void visit(Begin &) {};
  virtual void visit(Void &) {};
  virtual void visit (Apply &) {};
  virtual void visit (ASTType &) {};
  virtual void visit (Define &) {};
};

class AST {
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;
};

class Program : public AST {
  Expr *E;
  ProgramInfo Info;
  std::vector<Define *> functions;

public:
  Program(std::vector<Define *> functions, Expr *E) : E(E), functions(functions) {};
  Program(std::vector<Define *> functions, Expr *E, ProgramInfo Info) : E(E), Info(Info), functions(functions) {};

  Expr *getExpr() const { return E; };
  std::vector<Define *> getfunctions() const { return functions; };
  ProgramInfo getInfo() const { return Info; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

enum TypeKind { TypeInt, TypeBool, TypeVoid, TypeVector, TypeFunc, TypeUnknown };

class Expr : public AST {
public:
  enum ExprKind { ExprPrim, ExprInt, ExprVar, ExprLet, ExprBool, ExprIf, ExprVoid, ExprWhile, ExprSet, ExprBegin, ExprApply, ExprFuncVar };

private:
  const ExprKind Kind;
  TypeKind Type;
  std::vector<TypeKind> ElementTypes; // elemtypes for vectors
  ASTType* definingType = nullptr;

public:
  Expr(ExprKind Kind, TypeKind Type = TypeUnknown) : Kind(Kind), Type(Type) {}

  ExprKind getKind() const { return Kind; }
  TypeKind getType() const { return Type; }
  void setType(TypeKind T) { Type = T; }
  const std::vector<TypeKind>& getElementTypes() const { return ElementTypes; }
  void setElementTypes(const std::vector<TypeKind>& ET) { ElementTypes = ET; }
  ASTType* getDefiningType() const { return definingType; }
  void setDefiningType(ASTType* T) { definingType = T; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Int : public Expr {
  StringRef Value;

public:
  Int(StringRef Value) : Expr(ExprInt, TypeInt), Value(Value) {};
  StringRef getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprInt; }
};

class Prim : public Expr {
  TokenKind Op;
  Expr *E1 = NULL;
  Expr *E2 = NULL;
  std::vector<Expr *> E;
  Int *index = NULL;

public:
  Prim(TokenKind Op) : Expr(ExprPrim), Op(Op) {};
  Prim(TokenKind Op, Expr *E1) : Expr(ExprPrim), Op(Op), E1(E1) {};
  Prim(TokenKind Op, Expr *E1, Expr *E2)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2) {};
  Prim(TokenKind Op, std::vector<Expr *> E) : Expr(ExprPrim), Op(Op), E(E) {};
  Prim(TokenKind Op, Expr *E1, Expr *E2, Int *index) : Expr(ExprPrim), Op(Op), E1(E1), E2(E2), index(index) {};

  TokenKind getOp() const { return Op; };
  Expr *getE1() const { return E1; };
  Expr *getE2() const { return E2; };
  std::vector<Expr *> getE() const { return E; };
  Int *getindex() const { return index; };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprPrim; }
};

class Var : public Expr {
  StringRef VarName;
  bool assigned;
  Expr* Value = NULL;

public:
  Var(StringRef Name ) : Expr(ExprVar), VarName(Name), assigned(false) {};
  Var(StringRef Name, Expr* Value) : Expr(ExprVar), VarName(Name), assigned(true), Value(Value) {};
  void assign(Expr *newval) { Value  = newval; assigned = true; }
  bool isassigned() { return assigned; }
  Expr* getValue() const { return Value; }
  StringRef getname() const { return VarName; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVar; }
};

class Let : public Expr {
  Var* var;
  Expr* exp = NULL;
  
public:
  Let(StringRef varname, Expr *varval, Expr *exp) : Expr(ExprLet), var(new Var(varname,varval)), exp(exp) {};
  Expr *getvarval() const { return var->getValue(); }
  Var* getvar() { return var; }
  Expr *getexp() const { return exp; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
  static bool classof(const Expr *E) { return E->getKind() == ExprLet; }
};

class Bool : public Expr {
  StringRef Value;

public:
  Bool(StringRef Value) : Expr(ExprBool, TypeBool), Value(Value) {};
  StringRef getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBool; }
};

class If : public Expr {
  Expr *cond;
  Expr *IfExpr;
  Expr *ElseExpr;

public:
  If(Expr *E1, Expr *E2, Expr *E3) : Expr(ExprIf), cond(E1), IfExpr(E2), ElseExpr(E3) {};
  Expr *getcond() const { return cond; }
  Expr *getifexpr() const { return IfExpr; }
  Expr *getelseexpr() const { return ElseExpr; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprIf; }
};

class Void : public Expr {
public:
  Void() : Expr(ExprVoid) {};

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVoid; }
};

class Begin : public Expr {
  std::vector<Expr *> subexpressions;
  Expr *finalexpr = NULL;

public:
  Begin(std::vector<Expr *> subexpressions, Expr *finalexpr) : Expr(ExprBegin), subexpressions(subexpressions), finalexpr(finalexpr) {};
  std::vector<Expr *> getsubexpr() const { return subexpressions; }
  Expr *getfinalexpr() const { return finalexpr; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBegin; }

};

class WhileLoop : public Expr {
  Expr *cond = NULL;
  Expr *loop = NULL;

public:
  WhileLoop(Expr* E1, Expr *E2) : Expr(ExprWhile), cond(E1), loop(E2) {};
  Expr *getcond() const { return cond; }
  Expr *getloop() const { return loop; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprWhile; }
};

class SetBang : public Expr {
  Var *var;
  Expr *newvar = NULL;

public:
  SetBang(Var *var, Expr *exp) : Expr(ExprSet), var(var), newvar(exp) {};
  Var *getVar() const { return var; }
  Expr *getExpr() const { return newvar; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprSet; }
};

class Apply : public Expr {
  Expr * funcname;
  std::vector<Expr *> funcparams;
  ASTType * functype = NULL;

public:
  Apply(Expr * funcname, std::vector<Expr *> funcparams) : Expr(ExprApply), funcname(funcname), funcparams(funcparams) {};
  std::vector<Expr *> getparams() const { return funcparams; }
  Expr * getfuncname() const { return funcname; }
  ASTType * getASTType() const { return functype; }
  void setASTType(ASTType * type) { functype = type; } // setter?
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprApply; }
};
class Define : public AST {
  Var * funcname; //StringRef or Var. Not sure.
  std::vector<Var *> funcinvars;
  ASTType * functype = NULL;
  Expr * funcbody = NULL;

public:
  Define(Var* funcname, std::vector<Var *> funcinvars, ASTType * functype, Expr * funcbody)
    : funcname(funcname), funcinvars(funcinvars), functype(functype), funcbody(funcbody) {};
  Var * getfuncname() const { return funcname; }
  std::vector<Var *> getinvars() const { return funcinvars; }
  ASTType * getfunctype() const { return functype; }
  Expr * getbody() const { return funcbody; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class ASTType : public AST {
  StringRef Value;
  std::vector<ASTType *> typevec;
  ASTType * outtype;
  TypeKind type = TypeUnknown;

public:
  ASTType(StringRef Value) : Value(Value) {};
  ASTType(std::vector<ASTType *> typevec) : typevec(typevec), type(TypeVector) {};
  ASTType(std::vector<ASTType *> typevec, ASTType *outtype) : typevec(typevec),outtype(outtype),type(TypeFunc) {};
  bool isfuntype() const { return type == TypeFunc; }
  bool isvectype() const { return type == TypeVector; }
  StringRef getval() const { return Value; }
  std::vector<ASTType *> gettypevec() const { return typevec; }
  std::vector<ASTType *> getintypes() const { return typevec; }
  ASTType * getouttype() const { return outtype; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};
#endif
