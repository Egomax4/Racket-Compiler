#include "llracket/Lexer/Lexer.h"

namespace charinfo {
LLVM_READNONE inline static bool isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}
LLVM_READNONE inline static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
LLVM_READNONE inline static bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

LLVM_READNONE inline static bool isAlphanumeric(char c) {
  return isLetter(c) || isDigit(c);
}

LLVM_READNONE inline static bool isAlphanumeric_(char c) {
  return isAlphanumeric(c) || c == '_';
}
} // namespace charinfo

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = TokenKind::eof;
    return;
  }

  if (charinfo::isDigit(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    formToken(token, End, TokenKind::integer_literal);
    return;
  }

  if (*BufferPtr == '<') {
    const char *End = BufferPtr + 1;
    if (*End == '=')
    {
      End++;
      formToken(token, End, TokenKind::lte);
      return;
    }
    formToken(token, End, TokenKind::lt);
    return;
  }
  
  if (*BufferPtr == '>') {
    const char *End = BufferPtr + 1;
    if (*End == '=')
    {
      End++;
      formToken(token, End, TokenKind::gte);
      return;
    }
    formToken(token, End, TokenKind::gt);
    return;
  }
  // if (charinfo::isComparator(*BufferPtr)) {
  //   formToken(token, BufferPtr+1, TokenKind::comparator)
  //   return;
  // }
  if (*BufferPtr =='#') {
    const char *End = BufferPtr + 1;
    if(*End == 'f' || *End == 't')
    {
      End++;
      formToken(token, End, TokenKind::boolean);
      return;
    }
    else if(*End == '<' && *(End+1) == 'v' && *(End+2) == 'o' && *(End+3) == 'i' && *(End+4) == 'd' && *(End+5) == '>')
    {
      formToken(token, End+5, TokenKind::Void);
      return;
    }
  }

  if (*BufferPtr == '-' && *(BufferPtr+1) == '>') {
    const char *End = BufferPtr + 2;
    formToken(token,End, TokenKind::typearrow); // I've done this for now if it's bad for type checking then change
    return;
  }

  // if (*BufferPtr == '(') {
  //   const char *End = BufferPtr + 1;
  //   while (*End && charinfo::isWhitespace(*End))
  //     ++End;
  //   const char *varname = End;
  //   while (charinfo::isAlphanumeric_(*End))
  //     ++End;
  //   llvm::StringRef Text(varname, End-varname);
  //   if (Text == "define") {
  //     formToken(token, End, TokenKind::def);
  //     return;
  //   }
  // }

  if (charinfo::isLetter(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isAlphanumeric_(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    if (Text == "read") {
      formToken(token, End, TokenKind::read);
      return;
    }
    if (Text == "let") {
      formToken(token, End, TokenKind::kw_LET);
      return;
    }
    if (Text == "and") {
      formToken(token, End, TokenKind::cmp_and);
      return;
    }
    if (Text == "or") {
      formToken(token, End, TokenKind::cmp_or);
      return;
    }
    if (Text == "not") {
      formToken(token, End, TokenKind::cmp_not);
      return;
    }
    if (Text == "if") {
      formToken(token, End, TokenKind::kw_IF);
      return;
    }
    if (Text == "eq") {
      if(*End == '?') 
      {
        End++;
        formToken(token,End, TokenKind::equal);
        return;
      }
    }
    if (Text == "begin") {
      formToken(token, End, TokenKind::kw_BEGIN);
      return;
    }
    if (Text == "while") {
      formToken(token, End, TokenKind::kw_WHILE);
      return;
    }
    if (Text == "set") {
      if(*End == '!')
      {
        End++;
        formToken(token, End, TokenKind::kw_SET);
        return;
      }
    }
    if (Text == "vector") {
      if(*End == '-') {
        ++End;
        while (charinfo::isAlphanumeric_(*End))
          ++End;

        llvm::StringRef Text(BufferPtr, End - BufferPtr);
        if(Text == "vector-length") {
          formToken(token, End, TokenKind::veclen);
          return;
        }
        if(Text == "vector-ref") {
          formToken(token, End, TokenKind::vecref);
          return;
        }
        if(Text == "vector-set") {
          if(*End == '!')
          {
            ++End;
            formToken(token, End, TokenKind::vecset);
            return;
          }
        }
      }
      else {
        formToken(token,End, TokenKind::vec);
        return;
      } 
    }
    if (Text == "define") {
      formToken(token, End, TokenKind::def);
      return;
    }
    if (Text == "Integer" || Text == "Boolean" || Text == "Void") {
      formToken(token, End, TokenKind::Type);
      return;
    }
    if (Text == "Vector") {
      formToken(token,End, TokenKind::vectype);
      return;
    }
    formToken(token, End, TokenKind::var);
    return;
  }

  switch (*BufferPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, BufferPtr + 1, TokenKind::tok);                           \
    break;

    CASE('+', plus);
    CASE('-', minus);
    CASE('(', l_paren);
    CASE(')', r_paren);
    CASE('[', l_square);
    CASE(']', r_square);
    CASE('>', gt);
    CASE('<', lt);
    CASE(':', colon);
#undef CASE

  default:
    Diags.report(getLoc(), diag::err_unknown_token, *BufferPtr);
    formToken(token, BufferPtr + 1, TokenKind::unknown);
    break;
  }
  return;
}
Token Lexer::peek(unsigned N)
{
  const char * peekptr = BufferPtr;
  Token peektok;
  next(peektok);
  BufferPtr = peekptr;
  return peektok;
}


void Lexer::formToken(Token &Tok, const char *TokEnd, TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}
