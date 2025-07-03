#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace llracket;
using tok::TokenKind;

AST *Parser::parse() {
  Program *P = new Program(parseDef(), parseExpr());
  AST *Res = llvm::dyn_cast<AST>(P);
  expect(TokenKind::eof);
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  if (Tok.is(TokenKind::integer_literal)) {
    Int *Ret = new Int(Tok.getText());
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::var)) {
    Var * Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::boolean)) {
    Bool * Ret = new Bool(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (Tok.is(TokenKind::Void)) {
    advance();
    return new Void();
  }
  if(Tok.is(TokenKind::kw_LET))
  {
    advance();
    if (!consume(TokenKind::l_paren))
      return ErrorHandler();
    if(!consume(TokenKind::l_square))
      return ErrorHandler();
    StringRef var = Tok.getText();
    advance();
    Expr *varval = parseExpr();
    if (!consume(TokenKind::r_square))
      return ErrorHandler();
    if(!consume(TokenKind::r_paren))
      return ErrorHandler();
    Expr *exp = parseExpr();
    if(!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Let(var, varval, exp);
  }

  if(Tok.is(TokenKind::kw_IF))
  {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    Expr *E3 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new If(E1,E2,E3);
  }

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::plus)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::plus, E1, E2);
  }
  if (Tok.is(TokenKind::minus)) {
    advance();

    Expr *E1 = parseExpr();

    if (Tok.is(TokenKind::r_paren)) {
      advance();
      return new Prim(TokenKind::minus, E1);
    }

    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::minus, E1, E2);
  }
  if (Tok.is(TokenKind::cmp_and)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::cmp_and, E1, E2);
  }
  if (Tok.is(TokenKind::cmp_or)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::cmp_or, E1, E2);
  }
  if (Tok.is(TokenKind::lt)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::lt, E1, E2);
  }
  if (Tok.is(TokenKind::lte)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::lte, E1, E2);
  }
  if (Tok.is(TokenKind::gt)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::gt, E1, E2);
  }
  if (Tok.is(TokenKind::gte)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::gte, E1, E2);
  }
  if (Tok.is(TokenKind::equal)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::equal, E1, E2);
  }
  if (Tok.is(TokenKind::cmp_not)) {
    advance();
    Expr *E1 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::cmp_not, E1);
  }
  if (Tok.is(TokenKind::vec)) {
    advance();
    std::vector<Expr *> vec;
    Expr *E;
    while (!Tok.is(TokenKind::r_paren))
    {
      E = parseExpr();  
      vec.push_back(E);
    }
    advance();
    return new Prim(TokenKind::vec,vec);
  }
  if (Tok.is(TokenKind::veclen)) {
    advance();
    Expr *E1 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::veclen, E1);
  }
  if (Tok.is(TokenKind::vecref)) {
    advance();
    Expr *vec = parseExpr();
    if (!Tok.is(TokenKind::integer_literal))
      return ErrorHandler();
    Int *index = new Int(Tok.getText());
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::vecref, vec, NULL, index);
  }
  if (Tok.is(TokenKind::vecset)) {
    advance();
    Expr *vec = parseExpr();
    if (!Tok.is(TokenKind::integer_literal))
      return ErrorHandler();
    Int *index = new Int(Tok.getText());
    advance();
    Expr *newExpr = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::vecset, vec, newExpr, index);
  }
  if (Tok.is(TokenKind::kw_WHILE)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new WhileLoop(E1,E2);
  }
  if (Tok.is(TokenKind::kw_BEGIN)) {
    advance();
    std::vector<Expr *> subexpr;
    Expr *finalexp = parseExpr();
    while (!Tok.is(TokenKind::r_paren))
    {
      subexpr.push_back(finalexp);
      finalexp = parseExpr();
    }
    advance();
    return new Begin(subexpr,finalexp);
  }
  if (Tok.is(TokenKind::kw_SET)) {
    advance();
    if (!Tok.is(TokenKind::var))
      return ErrorHandler();
    Var *var = new Var(Tok.getText());
    advance();
    Expr *exp = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new SetBang(var,exp);
  }
  Expr * funcname = parseExpr();
  std::vector<Expr *> funcparams;
  while (!Tok.is(TokenKind::r_paren))
    funcparams.push_back(parseExpr());
  advance();
  return new Apply(funcname,funcparams);
}

ASTType *Parser::parseType() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  if (Tok.is(TokenKind::Type))
  {
    ASTType *Ret = new ASTType(Tok.getText());
    advance();
    return Ret;
  }
  if(!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (Tok.is(TokenKind::vectype)) {
    advance();
    std::vector<ASTType *> typevec;
    ASTType *curtype;
    while (!Tok.is(TokenKind::r_paren))
    {
      curtype = parseType();
      typevec.push_back(curtype);
    }
    advance();
    return new ASTType(typevec);
  }
  
  else {
    ASTType * curtype; 
    ASTType * outtype;
    std::vector<ASTType *> intypes;
    int numintypes = 0;
    while (true) {
      if (Tok.is(TokenKind::typearrow)) {
        advance();
        break;
      }
      curtype = parseType();
      intypes.push_back(curtype);
      if (++numintypes == 10) {
        return ErrorHandler();
      }
    }
    outtype = parseType();
    if(!consume(TokenKind::r_paren)) 
      return ErrorHandler();
    
    return new ASTType(intypes,outtype);
  }
  
}

Define *Parser::parseFun() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  if(!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (!Tok.is(TokenKind::var)) 
    return ErrorHandler();
  
  Var * FunName = new Var(Tok.getText());
  advance();

  std::vector<ASTType *> funcintypes;
  std::vector<Var *> funcinvars;
  while (!Tok.is(TokenKind::r_paren))
  {
    if(!consume(TokenKind::l_square))
      return ErrorHandler();
    if (!Tok.is(TokenKind::var)) 
      return ErrorHandler();
    Var * varname = new Var(Tok.getText());
    funcinvars.push_back(varname);
    advance();
    if (!consume(TokenKind::colon))
      return ErrorHandler();
    ASTType * vartype = parseType();
    funcintypes.push_back(vartype);
    if (!consume(TokenKind::r_square))
      return ErrorHandler();
  }
  advance();
  if (!consume(TokenKind::colon))
    return ErrorHandler();
  ASTType * funouttype = parseType();
  Expr * funcbody = parseExpr();
  if (!consume(TokenKind::r_paren))
    return ErrorHandler();
  
  ASTType * functype = new ASTType(funcintypes,funouttype);
  return new Define(FunName,funcinvars,functype,funcbody);
}

std::vector<Define *>Parser::parseDef() {
  std::vector<Define *> functions;
  while(Tok.is(TokenKind::l_paren) && peek().is(TokenKind::def)) {
    advance();
    advance();
    Define * fun = parseFun();
    functions.push_back(fun);
  }
  return functions;
}
