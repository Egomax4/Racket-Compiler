#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

struct VariableInfo {
  TypeKind type = TypeUnknown;
  std::vector<TypeKind> elementTypes;

  // function info
  std::vector<TypeKind> paramTypes;
  TypeKind returnType = TypeUnknown;
  ASTType *functionSignatureType = nullptr; // for function signature

  VariableInfo() = default;

  // Constructor for non-vector types
  VariableInfo(TypeKind k) : type(k) {}

  // Constructor for vector types
  VariableInfo(TypeKind k, std::vector<TypeKind> et)
      : type(k), elementTypes(std::move(et)) {
    // Ensure type is actually TypeVector if elementTypes are provided
    if (!elementTypes.empty() && type != TypeVector) {
      // This case shouldn't happen with correct usage, but as a safeguard:
      type = TypeVector;
    }
  }

  // constructor for function types
  VariableInfo(TypeKind k, std::vector<TypeKind> params, TypeKind ret,
               ASTType *ftype)
      : type(k), paramTypes(std::move(params)), returnType(ret),
        functionSignatureType(ftype) {
    // Ensure type is actually TypeFunc
    if (type != TypeFunc) {
      type = TypeFunc;
    }
  }
};

const char *getTypeName(TypeKind K) {
  switch (K) {
  case TypeInt:
    return "Integer";
  case TypeBool:
    return "Boolean";
  case TypeVoid:
    return "Void";
  case TypeVector:
    return "Vector";
  case TypeUnknown:
    return "Unknown";
  case TypeFunc:
    return "Function";
  }

  return "InvalidType"; // Should not happen
}

struct ReadInfo {
  Expr *readExpr = nullptr;
  TypeKind inferredType = TypeUnknown;
  std::string assignedVar = "";
  bool processed = false;

  ReadInfo(Expr *expr) : readExpr(expr) {}
};

class ProgramCheck : public ASTVisitor {
  bool HasError;

  std::vector<llvm::StringMap<VariableInfo>> scopeStack;

  // stack for tracking reads
  std::vector<ReadInfo> readNodes;
  // map to keep track of vars inited from read
  // llvm::StringMap<ReadInfo *> readVariables;
  // use indexing to to avoid mem issues
  llvm::StringMap<size_t> readVariables;

  void enterScope() { scopeStack.push_back(llvm::StringMap<VariableInfo>()); }

  void exitScope() {
    if (!scopeStack.empty()) {
      scopeStack.pop_back();
    }
  }

  bool declareVariable(llvm::StringRef Name, const VariableInfo &Info) {
    if (scopeStack.empty()) {
      llvm::errs() << "Internal Error: No scope available for declaration.\n";
      HasError = true;
      return false;
    }

    auto it = scopeStack.back().find(Name);
    if (it != scopeStack.back().end()) {
      // AST *errorNode =
      //     nullptr;
      llvm::errs() << "Type Error: Identifier '" << Name.str()
                   << "' redeclared in the same scope.\n";
      HasError = true;
      return false; // fail
    }

    for (int i = scopeStack.size() - 2; i >= 0; --i) {
      const auto &outerScope = scopeStack[i];
      auto outerIt = outerScope.find(Name);
      if (outerIt != outerScope.end()) {
        const VariableInfo &outerInfo = outerIt->second;
        bool isNewFunc = (Info.type == TypeFunc);
        bool isOuterFunc = (outerInfo.type == TypeFunc);

        if (isNewFunc && !isOuterFunc) {
          AST *errorNode = Info.functionSignatureType;
          if (errorNode) {
            reportTypeError(
                *errorNode,
                "Error: Function definition '" + Name.str() +
                    "' shadows a non-function variable from an outer scope.");
          } else {
            llvm::errs()
                << "Type Error: Function definition '" << Name.str()
                << "' shadows a non-function variable from an outer scope.\n";
          }
          HasError = true;
          return false; // Function shadowing variable
        }
        if (!isNewFunc && isOuterFunc) {
          llvm::errs() << "Type Error: Variable declaration '" << Name.str()
                       << "' shadows a function from an outer scope.\n";
          HasError = true;
          return false;
        }
        break;
      }
    }

    scopeStack.back()[Name] = Info;
    return true; // success
  }

  std::optional<VariableInfo> lookupVariable(llvm::StringRef Name) {
    for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
      auto &scope = *it;
      auto varInfo = scope.find(Name);
      if (varInfo != scope.end()) {
        return varInfo->second; // Found type
      }
    }
    return std::nullopt; // Not found
  }

  // --- Type Checking Error Reporter --- (I AI generated this part)
  void reportTypeError(AST &Node, const std::string &message) {
    // In a real compiler, you'd use Node location info here
    llvm::errs() << "Type Error: " << message << "\n";
    HasError = true;
  }

  // Overload for specific expected/got types
  void reportTypeError(AST &Node, const std::string &context, TypeKind expected,
                       TypeKind got) {
    if (got != TypeUnknown) { // Don't report "expected X got Unknown" if it was
                              // already reported as undefined
      reportTypeError(Node, context + ": expected " + getTypeName(expected) +
                                " but got " + getTypeName(got));
    }
  }

  // mismatch between two types
  void reportTypeMismatchError(AST &Node, const std::string &context,
                               TypeKind type1, TypeKind type2) {
    if (type1 != TypeUnknown && type2 != TypeUnknown) {
      reportTypeError(Node, context + ": type mismatch between " +
                                getTypeName(type1) + " and " +
                                getTypeName(type2));
    } else if (type1 != TypeUnknown) {
      reportTypeError(
          Node, context + ": type mismatch, expected compatibility with " +
                    getTypeName(type1) +
                    " but got an undefined or erroneous type");
    } else if (type2 != TypeUnknown) {
      reportTypeError(
          Node,
          context + ": type mismatch, got " + getTypeName(type2) +
              " which is incompatible with an undefined or erroneous type");
    }
  }
  // --- pretty clear I generated it honestly ---

  void trackRead(Expr *readExpr) { readNodes.emplace_back(readExpr); }

  void assignReadToVar(llvm::StringRef varName, Expr *readExpr) {
    // for (auto &info : readNodes) {
    //   if (info.readExpr == readExpr && !info.processed) {
    //     info.assignedVar = varName.str();
    //     readVariables[varName] = &info;
    //     return;
    //   }
    // }

    for (size_t i = 0; i < readNodes.size(); ++i) {
      if (readNodes[i].readExpr == readExpr && !readNodes[i].processed) {
        readNodes[i].assignedVar = varName.str();
        readVariables[varName] = i; // Store the index 'i'
        return;
      }
    }
  }

  bool inferReadType(Expr *expr, TypeKind type) {
    // for read nodes
    if (auto *prim = llvm::dyn_cast<Prim>(expr)) {
      if (prim->getOp() == tok::read) {
        for (auto &info : readNodes) {
          if (info.readExpr == expr && !info.processed) {
            info.inferredType = type;
            info.processed = true;
            expr->setType(type);
            return true;
          }
        }
      }
    }

    if (auto *var = llvm::dyn_cast<Var>(expr)) {
      auto it = readVariables.find(var->getname());
      // Check if variable is in the map
      if (it != readVariables.end()) {
        size_t index = it->second; // Get the stored index

        if (index < readNodes.size()) {
          ReadInfo &info = readNodes[index];

          if (!info.processed) {
            info.inferredType = type;
            info.processed = true;

            // Ensure the readExpr pointer itself is valid before using it
            if (!info.readExpr) {
              llvm::errs() << "Internal Error: Null readExpr pointer in "
                              "ReadInfo for variable '"
                           << var->getname() << "' at index " << index << "\n";
              HasError = true; // Mark semantic error
              return false;
            }
            info.readExpr->setType(type);

            var->setType(type);
            return true;
          }
        } else {
          llvm::errs() << "Internal Warning: Stale index " << index
                       << " found for variable '" << var->getname()
                       << "' in readVariables.\n";
        }
      }
    }

    return false;
  }

  void validateReads() {
    for (auto &info : readNodes) {
      if (!info.processed) {
        reportTypeError(*info.readExpr,
                        "Cannot determine type for 'read' operation. "
                        "It must be used in a context that implies its type.");
        info.readExpr->setType(TypeUnknown);
      }
    }
  }

  bool inferBranchType(Expr *branchExpr, TypeKind inferredType) {
    // Direct case: branch is a read
    if (auto *prim = llvm::dyn_cast<Prim>(branchExpr)) {
      if (prim->getOp() == tok::read) {
        return inferReadType(branchExpr, inferredType);
      }
    }

    // Case where branch is a variable containing a read
    if (auto *var = llvm::dyn_cast<Var>(branchExpr)) {
      return inferReadType(branchExpr, inferredType);
    }

    // Recursively check for read operations in nested expressions
    return inferNestedReads(branchExpr, inferredType);
  }

  bool inferNestedReads(Expr *expr, TypeKind inferredType) {
    bool anyInferred = false;

    if (auto *letExpr = llvm::dyn_cast<Let>(expr)) {
      // Check let body
      anyInferred |= inferNestedReads(letExpr->getexp(), inferredType);
    } else if (auto *beginExpr = llvm::dyn_cast<Begin>(expr)) {
      // Check final expression in begin
      anyInferred |= inferNestedReads(beginExpr->getfinalexpr(), inferredType);
    } else if (auto *ifExpr = llvm::dyn_cast<If>(expr)) {
      // Both branches should have the same type
      anyInferred |= inferNestedReads(ifExpr->getifexpr(), inferredType);
      anyInferred |= inferNestedReads(ifExpr->getelseexpr(), inferredType);
    } else if (auto *primExpr = llvm::dyn_cast<Prim>(expr)) {
      // For primitives that return the same type as their operands
      if (primExpr->getOp() == tok::read) {
        anyInferred |= inferReadType(primExpr, inferredType);
      }
    }

    return anyInferred;
  }

  TypeKind convertAstTypeToTypeKind(ASTType *astType) {
    if (!astType)
      return TypeUnknown;

    if (astType->isfuntype())
      return TypeFunc;
    if (astType->isvectype())
      return TypeVector;

    StringRef typeName = astType->getval();
    if (typeName == "Integer")
      return TypeInt;
    if (typeName == "Boolean")
      return TypeBool;
    if (typeName == "Void")
      return TypeVoid;

    reportTypeError(*astType, "Unknown type '" + typeName.str() + "'");
    return TypeUnknown;
  }

  bool declareFnSigs(Define &Node) {
    ASTType *funcType = Node.getfunctype();

    std::vector<ASTType *> inputTypes = funcType->getintypes();
    ASTType *outputType = funcType->getouttype();
    Var *funcName = Node.getfuncname();
    std::vector<Var *> funcParams = Node.getinvars();

    // Validate that the number of parameters matches the input types
    if (inputTypes.size() != funcParams.size()) {
      reportTypeError(Node, "Function '" + funcName->getname().str() +
                                "' has " + std::to_string(funcParams.size()) +
                                " parameters but " +
                                std::to_string(inputTypes.size()) +
                                " input types declared");
      HasError = true;
      return false;
    }

    // Validate function name is not already declared
    if (scopeStack.back().count(funcName->getname())) {
      reportTypeError(Node, "Function '" + funcName->getname().str() +
                                "' redeclared in the same scope");
      HasError = true;
      return false;
    }

    // Create function type information
    std::vector<TypeKind> paramTypes;
    for (ASTType *paramType : inputTypes) {
      paramTypes.push_back(convertAstTypeToTypeKind(paramType));
    }

    TypeKind returnType = convertAstTypeToTypeKind(outputType);

    // Store function signature in variable scope using the function constructor
    VariableInfo funcInfo(TypeFunc, paramTypes, returnType, funcType);
    if (!this->declareVariable(funcName->getname(), funcInfo)) {
      return false;
    }

    return true;
  }

public:
  ProgramCheck() : HasError(false) {
    scopeStack.push_back(llvm::StringMap<VariableInfo>());
  }

  bool hasError() { return HasError; }

  virtual void visit(Program &Node) override {
    // creates all fn defns
    for (auto &funcdef : Node.getfunctions()) {
      if (!declareFnSigs(*funcdef)) {
      }
    }

    // checks the body exprs of fns
    for (auto &i : Node.getfunctions()) {
      i->accept(*this);
    }

    // checks the main program
    if (Node.getExpr())
      Node.getExpr()->accept(*this);
    else
      HasError = true;

    validateReads();
  };

  virtual void visit(Expr &Node) override {
    if (auto *P = llvm::dyn_cast<Prim>(&Node)) {
      visit(*P);
      return;
    }
    if (auto *I = llvm::dyn_cast<Int>(&Node)) {
      visit(*I);
      return;
    }
    if (auto *L = llvm::dyn_cast<Let>(&Node)) {
      visit(*L);
      return;
    }
    if (auto *V = llvm::dyn_cast<Var>(&Node)) {
      visit(*V);
      return;
    }
    if (auto *B = llvm::dyn_cast<Bool>(&Node)) {
      visit(*B);
      return;
    }
    if (auto *Ifn = llvm::dyn_cast<If>(&Node)) {
      visit(*Ifn);
      return;
    }
    if (auto *W = llvm::dyn_cast<WhileLoop>(&Node)) {
      visit(*W);
      return;
    }
    if (auto *Bgn = llvm::dyn_cast<Begin>(&Node)) {
      visit(*Bgn);
      return;
    }
    if (auto *S = llvm::dyn_cast<SetBang>(&Node)) {
      visit(*S);
      return;
    }
    if (auto *Vd = llvm::dyn_cast<Void>(&Node)) {
      visit(*Vd);
      return;
    }
    // if (auto *Def = llvm::dyn_cast<Define>(&Node)) {
    //   visit(*Def);
    //   return;
    // }
    if (auto *A = llvm::dyn_cast<Apply>(&Node)) {
      visit(*A);
      return;
    }

    reportTypeError(Node, "Unknown expression type");
    Node.setType(TypeUnknown);
  }

  virtual void visit(Prim &Node) override {
    TokenKind Op = Node.getOp();
    Expr *E1 = Node.getE1();
    Expr *E2 = Node.getE2();
    std::vector<Expr *> Args = Node.getE(); // For vector
    Int *IndexNode = Node.getindex();

    switch (Op) {
    case tok::read:
      // Node.setType(TypeInt);
      trackRead(&Node);
      Node.setType(TypeUnknown);
      break;

    case tok::plus:
    case tok::minus: // Binary minus or addition
      if (E1 && E2) {
        E1->accept(*this);
        E2->accept(*this);

        inferReadType(E1, TypeInt);
        inferReadType(E2, TypeInt);

        if (E1->getType() != TypeInt)
          reportTypeError(Node, "Left operand for arithmetic", TypeInt,
                          E1->getType());
        if (E2->getType() != TypeInt)
          reportTypeError(Node, "Right operand for arithmetic", TypeInt,
                          E2->getType());
        if (E1->getType() == TypeInt && E2->getType() == TypeInt) {
          Node.setType(TypeInt);
        } else {
          Node.setType(TypeUnknown);
        }
      } else if (Op == tok::minus && E1 && !E2) { // Unary minus
        E1->accept(*this);

        inferReadType(E1, TypeInt);

        if (E1->getType() != TypeInt)
          reportTypeError(Node, "Operand for unary minus", TypeInt,
                          E1->getType());
        if (E1->getType() == TypeInt) {
          Node.setType(TypeInt);
        } else {
          Node.setType(TypeUnknown);
        }
      } else {
        reportTypeError(Node, "Invalid arguments for arithmetic operation.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::cmp_and:
    case tok::cmp_or:
      if (E1 && E2) {
        E1->accept(*this);
        E2->accept(*this);

        inferReadType(E1, TypeBool);
        inferReadType(E2, TypeBool);

        if (E1->getType() != TypeBool)
          reportTypeError(Node, "Left operand for logical op", TypeBool,
                          E1->getType());
        if (E2->getType() != TypeBool)
          reportTypeError(Node, "Right operand for logical op", TypeBool,
                          E2->getType());
        if (E1->getType() == TypeBool && E2->getType() == TypeBool) {
          Node.setType(TypeBool);
        } else {
          Node.setType(TypeUnknown);
        }
      } else {
        reportTypeError(Node, "Invalid arguments for logical operation.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::cmp_not:
      if (E1 && !E2) {
        E1->accept(*this);

        inferReadType(E1, TypeBool);

        if (E1->getType() != TypeBool)
          reportTypeError(Node, "Operand for 'not'", TypeBool, E1->getType());
        if (E1->getType() == TypeBool) {
          Node.setType(TypeBool);
        } else {
          Node.setType(TypeUnknown);
        }
      } else {
        reportTypeError(Node, "Invalid arguments for 'not' operation.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::lt:
    case tok::lte:
    case tok::gt:
    case tok::gte:
      // Comparisons require integers
      if (E1 && E2) {
        E1->accept(*this);
        E2->accept(*this);

        inferReadType(E1, TypeInt);
        inferReadType(E2, TypeInt);

        if (E1->getType() != TypeInt)
          reportTypeError(Node, "Left operand for comparison", TypeInt,
                          E1->getType());
        if (E2->getType() != TypeInt)
          reportTypeError(Node, "Right operand for comparison", TypeInt,
                          E2->getType());
        if (E1->getType() == TypeInt && E2->getType() == TypeInt) {
          Node.setType(TypeBool); // Result is boolean
        } else {
          Node.setType(TypeUnknown);
        }
      } else {
        reportTypeError(Node, "Invalid arguments for comparison operation.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::equal: // eq? can compare Ints or Bools (or others, but
                     // restricted here)
      if (E1 && E2) {
        E1->accept(*this);
        E2->accept(*this);
        TypeKind T1 = E1->getType();
        TypeKind T2 = E2->getType();

        // if type is unknown but from read, try infer from other
        if (T1 != TypeUnknown && T2 == TypeUnknown) {
          inferReadType(E2, T1);
          T2 = E2->getType();
        } else if (T1 == TypeUnknown && T2 != TypeUnknown) {
          inferReadType(E1, T2);
          T1 = E1->getType();
        }

        if (T1 == TypeUnknown || T2 == TypeUnknown) {
          Node.setType(TypeUnknown); // Error already reported
        } else if (T1 == T2) {
          Node.setType(TypeBool); // Result is boolean
        } else if (T1 == TypeVoid && T2 == TypeVoid) {
          Node.setType(TypeBool); // Comparing void is allowed?
        } else {
          reportTypeMismatchError(
              Node, "'eq?' operands have incompatible types", T1, T2);
          Node.setType(TypeUnknown);
        }
      } else {
        reportTypeError(Node, "Invalid arguments for 'eq?' operation.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::vec: {
      std::vector<TypeKind> elementTypes;
      std::vector<ASTType *> elementASTTypes;
      bool anyElementError = false;

      for (Expr *arg : Args) {
        arg->accept(*this);
        TypeKind elemKind = arg->getType();

        if (elemKind == TypeUnknown) {
          anyElementError = true;
          elementASTTypes.push_back(nullptr);
          elementTypes.push_back(TypeUnknown);
          continue;
        }

        elementTypes.push_back(elemKind); // Store the TypeKind

        // ASTType* for this element
        ASTType *argASTType = nullptr;
        if (elemKind == TypeInt) {
          argASTType = new ASTType("Integer");
        } else if (elemKind == TypeBool) {
          argASTType = new ASTType("Boolean");
        } else if (elemKind == TypeVoid) {
          argASTType = new ASTType("Void");
        } else if (elemKind == TypeVector || elemKind == TypeFunc) {
          argASTType = arg->getDefiningType();
          if (!argASTType) {
            reportTypeError(*arg, "Internal Error: Vector or Function argument "
                                  "missing its defining ASTType.");
            anyElementError = true;
          }
        } else {
          reportTypeError(*arg, "Internal Error: Unhandled TypeKind when "
                                "creating vector ASTType.");
          anyElementError = true;
        }
        if (!argASTType && elemKind != TypeUnknown) {
          anyElementError = true;
        }
        elementASTTypes.push_back(argASTType);
      }

      if (anyElementError ||
          std::any_of(elementASTTypes.begin(), elementASTTypes.end(),
                      [](ASTType *p) { return p == nullptr; })) {
        anyElementError = true;
      }

      if (anyElementError) {
        Node.setType(TypeUnknown); // Propagate error
        Node.setElementTypes({});
        Node.setDefiningType(nullptr);
      } else {
        Node.setType(TypeVector);
        Node.setElementTypes(elementTypes);
        ASTType *vectorDefiningType = new ASTType(elementASTTypes);
        Node.setDefiningType(
            vectorDefiningType); // Store the detailed structure
      }
    } break;

    case tok::veclen:
      if (E1) {
        E1->accept(*this);
        if (E1->getType() != TypeVector) {
          reportTypeError(*E1, "Argument for 'vector-length'", TypeVector,
                          E1->getType());
          Node.setType(TypeUnknown);
        } else {
          Node.setType(TypeInt); // Length is an integer
        }
      } else {
        reportTypeError(Node, "Missing argument for 'vector-length'.");
        Node.setType(TypeUnknown);
      }
      break;

    case tok::vecref:
      if (E1 && IndexNode) {
        E1->accept(*this);
        IndexNode->accept(*this);

        TypeKind e1BaseType =
            E1->getType(); // Get base type (should be TypeVector)
        const std::vector<TypeKind> &e1ElementTypes =
            E1->getElementTypes(); // Get flat list

        if (e1BaseType != TypeVector) {
          reportTypeError(*E1, "First argument for 'vector-ref'", TypeVector,
                          e1BaseType);
          Node.setType(TypeUnknown);
          Node.setElementTypes({});
          Node.setDefiningType(nullptr);
        } else if (IndexNode->getType() != TypeInt) {
          reportTypeError(*IndexNode, "Index for 'vector-ref'", TypeInt,
                          IndexNode->getType());
          Node.setType(TypeUnknown);
          Node.setElementTypes({});
          Node.setDefiningType(nullptr);
        } else {
          long long indexVal;
          bool indexIsConstant =
              !IndexNode->getValue().getAsInteger(10, indexVal);

          if (indexIsConstant) {
            if (indexVal < 0 || (size_t)indexVal >= e1ElementTypes.size()) {
              reportTypeError(
                  *IndexNode,
                  "Error: Constant index " + std::to_string(indexVal) +
                      " for 'vector-ref' is out of bounds for vector of size " +
                      std::to_string(e1ElementTypes.size()) + ".");
              Node.setType(TypeUnknown);
              Node.setElementTypes({});
              Node.setDefiningType(nullptr);
            } else {
              // Get the element's base type
              TypeKind elementType = e1ElementTypes[indexVal];
              Node.setType(elementType);
              Node.setElementTypes({});
              Node.setDefiningType(nullptr);

              if (elementType == TypeVector) {
                ASTType *e1DefiningType = E1->getDefiningType();

                if (e1DefiningType && e1DefiningType->isvectype() &&
                    (size_t)indexVal < e1DefiningType->gettypevec().size()) {
                  ASTType *elementASTType =
                      e1DefiningType->gettypevec()[indexVal];

                  if (elementASTType && elementASTType->isvectype()) {
                    std::vector<TypeKind> nestedElementTypes;
                    bool conversionError = false;
                    for (ASTType *nestedElemASTType :
                         elementASTType->gettypevec()) {
                      TypeKind kind =
                          convertAstTypeToTypeKind(nestedElemASTType);
                      if (kind == TypeUnknown) {
                        conversionError = true;
                        break;
                      }
                      nestedElementTypes.push_back(kind);
                    }

                    if (!conversionError) {
                      Node.setElementTypes(nestedElementTypes);
                      Node.setDefiningType(elementASTType); // Store the details
                    } else {
                      reportTypeError(Node, "Internal Error: Failed to convert "
                                            "nested vector element types.");
                      Node.setType(TypeUnknown); // Propagate error
                      Node.setElementTypes({});
                      Node.setDefiningType(nullptr);
                    }
                  } else if (elementASTType) {
                    if (elementType == TypeVector &&
                        !elementASTType->isvectype()) {
                      reportTypeError(
                          Node,
                          "Internal Error: Inconsistent type definition for "
                          "nested vector element (ASTType is not vector).");
                      Node.setType(TypeUnknown);
                    }
                    Node.setDefiningType(nullptr);
                  } else {
                    reportTypeError(Node, "Internal Error: Could not find "
                                          "ASTType for vector element.");
                    Node.setType(TypeUnknown);
                    Node.setElementTypes({});
                    Node.setDefiningType(nullptr);
                  }
                } else {
                  reportTypeError(Node,
                                  "Internal Error: Could not determine nested "
                                  "vector element types for vector-ref "
                                  "(Missing E1 defining type or bad index).");
                  Node.setType(TypeUnknown);
                  Node.setElementTypes({});
                  Node.setDefiningType(nullptr);
                }
              }
            }
          }
        }
      } else {
        reportTypeError(Node, "Invalid arguments for 'vector-ref'.");
        Node.setType(TypeUnknown);
        Node.setElementTypes({});
        Node.setDefiningType(nullptr);
      }
      break;

    case tok::vecset:
      if (E1 && E2 && IndexNode) {
        E1->accept(*this); // Check vector
        E2->accept(*this); // Check value being set

        bool vectorArgError = false;
        bool valueArgError = false;
        bool boundsError = false;

        if (E1->getType() != TypeVector) {
          reportTypeError(*E1, "First argument for 'vector-set!'", TypeVector,
                          E1->getType());
          vectorArgError = true;
        }
        if (E2->getType() == TypeUnknown) {
          // Error reported during E2 visit
          valueArgError = true;
        }

        if (!vectorArgError) {
          const std::vector<TypeKind> &elementTypes = E1->getElementTypes();
          long long indexVal;
          bool indexIsConstant =
              !IndexNode->getValue().getAsInteger(10, indexVal);

          if (indexIsConstant) {
            if (indexVal < 0 || (size_t)indexVal >= elementTypes.size()) {
              reportTypeError(*IndexNode,
                              "Error: Constant index " +
                                  std::to_string(indexVal) +
                                  " for 'vector-set!' is out of bounds for "
                                  "vector of size " +
                                  std::to_string(elementTypes.size()) + ".");
              boundsError = true; // Mark bounds error
            }
          }
        }

        Node.setType(TypeVoid);

        if (vectorArgError || valueArgError || boundsError) {
          Node.setType(TypeUnknown);
        }

      } else {
        reportTypeError(Node, "Invalid arguments for 'vector-set!'.");
        Node.setType(TypeUnknown);
      }
      break;

    default:
      reportTypeError(Node, "Internal Error: Unhandled primitive operation in "
                            "semantic analysis.");
      Node.setType(TypeUnknown);
      break;
    }
  }

  virtual void visit(Int &Node) override { Node.setType(TypeInt); }

  virtual void visit(Bool &Node) override { Node.setType(TypeBool); }

  virtual void visit(Void &Node) override { Node.setType(TypeVoid); }

  virtual void visit(Var &Node) override {
    std::optional<VariableInfo> varInfoData = lookupVariable(Node.getname());
    if (!varInfoData) {
      reportTypeError(Node, "Use of undefined variable '" +
                                Node.getname().str() + "'.");
      Node.setType(TypeUnknown);
      Node.setElementTypes({}); // clear elem types on error
      Node.setDefiningType(nullptr);
    } else {
      // Found the variable info
      const VariableInfo &varInfo = *varInfoData;
      Node.setType(varInfo.type);
      // Set element types on the Var node if it's a vector
      if (varInfo.type == TypeVector) {
        Node.setElementTypes(varInfo.elementTypes);
      } else {
        Node.setElementTypes({}); // Clear if not a vector
      }

      Node.setDefiningType(varInfo.functionSignatureType);
    }
  }

  virtual void visit(Let &Node) override {
    Expr *varValueExpr = Node.getvarval();
    varValueExpr->accept(*this);

    if (auto *primExpr = llvm::dyn_cast<Prim>(varValueExpr)) {
      if (primExpr->getOp() == tok::read) {
        assignReadToVar(Node.getvar()->getname(), varValueExpr);
        // guaranteed that variable assignments to read in let will be int
        inferReadType(varValueExpr, TypeInt);
      }
    }

    TypeKind varType = varValueExpr->getType();

    // if (varType == TypeUnknown) {
    //   Node.setType(TypeUnknown); // Let expression type is unknown
    //   return;                    // Don't declaration
    // }

    VariableInfo varInfo;
    if (varType == TypeVector) {
      varInfo = VariableInfo(TypeVector, varValueExpr->getElementTypes());
      varInfo.functionSignatureType = varValueExpr->getDefiningType();
    } else if (varType == TypeFunc) {
      varInfo = VariableInfo(varType);
      varInfo.functionSignatureType = varValueExpr->getDefiningType();
    } else {
      varInfo = VariableInfo(varType);
      varInfo.functionSignatureType = nullptr;
    }

    // new scope for let body
    enterScope();

    bool declared = declareVariable(Node.getvar()->getname(), varInfo);

    Expr *bodyExpr = Node.getexp();
    TypeKind finalBodyType = TypeUnknown;
    if (declared) {
      bodyExpr->accept(*this);
      // Node.setType(bodyExpr->getType()); // Type of Let is type of its body
      finalBodyType = bodyExpr->getType();
    } else {
      // failure :(
      // Node.setType(TypeUnknown);
      finalBodyType = TypeUnknown;
    }

    if (!declared || finalBodyType == TypeUnknown) {
      Node.setType(TypeUnknown);
    } else {
      Node.setType(finalBodyType);
    }

    // exit scope
    exitScope();
  }

  virtual void visit(If &Node) override {
    Expr *Cond = Node.getcond();
    Expr *ThenBranch = Node.getifexpr();
    Expr *ElseBranch = Node.getelseexpr();

    Cond->accept(*this);

    inferReadType(Cond, TypeBool);

    if (Cond->getType() != TypeBool && Cond->getType() != TypeUnknown) {
      reportTypeError(*Cond, "Condition for 'if'", TypeBool, Cond->getType());
    }

    ThenBranch->accept(*this);
    ElseBranch->accept(*this);

    TypeKind thenType = ThenBranch->getType();
    TypeKind elseType = ElseBranch->getType();

    if (thenType != TypeUnknown && elseType == TypeUnknown) {
      // Try to infer the else branch type from the then branch
      if (inferBranchType(ElseBranch, thenType)) {
        elseType = thenType;
      }
    } else if (thenType == TypeUnknown && elseType != TypeUnknown) {
      // Try to infer the then branch type from the else branch
      if (inferBranchType(ThenBranch, elseType)) {
        thenType = elseType;
      }
    }

    // llvm::errs() << "Then type: " << getTypeName(thenType) << "\n";
    // llvm::errs() << "Else type: " << getTypeName(elseType) << "\n";

    if (Cond->getType() == TypeUnknown || thenType == TypeUnknown ||
        elseType == TypeUnknown) {
      // Error occurred in one of the subexpressions
      Node.setType(TypeUnknown);
    } else if (thenType == elseType) {
      // Types match, the result is that type
      Node.setType(thenType);
    } else {
      reportTypeMismatchError(Node, "'if' branches must have the same type",
                              thenType, elseType);
      Node.setType(TypeUnknown);
    }
  }

  virtual void visit(WhileLoop &Node) override {
    Expr *Cond = Node.getcond();
    Expr *LoopBody = Node.getloop();

    Cond->accept(*this);

    inferReadType(Cond, TypeBool);

    if (Cond->getType() != TypeBool && Cond->getType() != TypeUnknown) {
      reportTypeError(*Cond, "Condition for 'while'", TypeBool,
                      Cond->getType());
    }

    LoopBody->accept(*this);
    Node.setType(TypeVoid);

    if (Cond->getType() == TypeUnknown || LoopBody->getType() == TypeUnknown) {
      // Node.setType(TypeUnknown);
    }
  }

  virtual void visit(Begin &Node) override {
    bool subExprError = false;

    for (auto &expr : Node.getsubexpr()) {
      expr->accept(*this);
      if (expr->getType() == TypeUnknown) {
        subExprError = true;
      }
    }

    Expr *finalExpr = Node.getfinalexpr();
    if (finalExpr) {
      finalExpr->accept(*this);
      if (finalExpr->getType() == TypeUnknown || subExprError) {
        Node.setType(TypeUnknown); // Propagate error
      } else {
        Node.setType(finalExpr->getType());
      }
    } else {
      reportTypeError(
          Node, "Internal Error: 'begin' construct missing final expression.");
      Node.setType(TypeUnknown);
    }
  }

  virtual void visit(SetBang &Node) override {
    Var *variable = Node.getVar();
    Expr *newValueExpr = Node.getExpr();
    llvm::StringRef varName = variable->getname();

    std::optional<VariableInfo> varInfoData = lookupVariable(varName);
    newValueExpr->accept(*this);

    if (auto *primExpr = llvm::dyn_cast<Prim>(newValueExpr)) {
      if (primExpr->getOp() == tok::read) {
        // If variable exists and has known type, infer read type
        if (varInfoData && varInfoData->type != TypeUnknown) {
          inferReadType(newValueExpr, varInfoData->type);
        } else {
          // Track for future inference
          assignReadToVar(varName, newValueExpr);
        }
      }
    }

    if (!varInfoData) {
      reportTypeError(*variable, "Cannot 'set!' undefined variable '" +
                                     varName.str() + "'.");
      // Don't check value type if var is undefined
    } else {
      const VariableInfo &varInfo = *varInfoData;

      TypeKind newValueType = newValueExpr->getType();

      if (newValueType != TypeUnknown && varInfo.type != newValueType) {
        reportTypeError(*newValueExpr, "Type mismatch in 'set!': variable '" +
                                           varName.str() + "' has type " +
                                           getTypeName(varInfo.type) +
                                           ", but assigned value has type " +
                                           getTypeName(newValueType));
      }
    }

    Node.setType(TypeVoid);

    if (!varInfoData || newValueExpr->getType() == TypeUnknown) {
      Node.setType(TypeUnknown);
    }
  }

  virtual void visit(Define &Node) override {
    Var *funcNameNode = Node.getfuncname();
    llvm::StringRef funcName = funcNameNode->getname();
    Expr *funcBody = Node.getbody();
    std::vector<Var *> funcParams = Node.getinvars();

    std::optional<VariableInfo> funcInfoOpt;
    if (!scopeStack.empty()) {
      auto globalScope = scopeStack.front().find(funcName);
      if (globalScope != scopeStack.front().end()) {
        funcInfoOpt = globalScope->second;
      }
    }

    const VariableInfo &funcInfo = *funcInfoOpt;

    // new scope for fn body
    enterScope();

    const auto &expectedParamASTTypes =
        funcInfo.functionSignatureType->getintypes();

    for (size_t i = 0; i < funcParams.size(); ++i) {
      Var *param = funcParams[i];
      ASTType *paramASTType =
          expectedParamASTTypes[i]; // Get the ASTType for this param
      TypeKind paramKind =
          convertAstTypeToTypeKind(paramASTType); // Determine its base kind

      VariableInfo paramVarInfo; // Create the VariableInfo for the parameter

      if (paramKind == TypeFunc && paramASTType->isfuntype()) {
        std::vector<TypeKind> funcParamKinds;
        for (ASTType *pType : paramASTType->getintypes()) {
          funcParamKinds.push_back(convertAstTypeToTypeKind(pType));
        }
        TypeKind funcReturnKind =
            convertAstTypeToTypeKind(paramASTType->getouttype());

        paramVarInfo = VariableInfo(TypeFunc, funcParamKinds, funcReturnKind,
                                    paramASTType);

      } else if (paramKind == TypeVector && paramASTType->isvectype()) {
        // It's a vector parse its element types
        std::vector<TypeKind> vecElementKinds;
        for (ASTType *eType : paramASTType->gettypevec()) { // Use gettypevec()
          vecElementKinds.push_back(convertAstTypeToTypeKind(eType));
        }
        paramVarInfo = VariableInfo(TypeVector, vecElementKinds);

        paramVarInfo.functionSignatureType = paramASTType;

      } else if (paramKind != TypeUnknown) {
        paramVarInfo = VariableInfo(paramKind);
        paramVarInfo.functionSignatureType = nullptr;
      } else {
        paramVarInfo = VariableInfo(TypeUnknown);
        paramVarInfo.functionSignatureType = nullptr;
      }

      declareVariable(param->getname(), paramVarInfo);
    }

    // Check function body
    if (funcBody) {
      funcBody->accept(*this);

      TypeKind bodyReturnType = funcBody->getType();
      TypeKind expectedReturnType = funcInfo.returnType;

      // Verify return type matches the declared return type
      if (bodyReturnType != expectedReturnType &&
          bodyReturnType != TypeUnknown) {
        reportTypeError(*funcBody,
                        "Function body returns type '" +
                            std::string(getTypeName(funcBody->getType())) +
                            "' but function is declared to return '" +
                            std::string(getTypeName(expectedReturnType)) + "'");
      }
    }

    exitScope();

    // funcTypes->gettypevec(); // this has the input types in string format
    // funcTypes->getouttype(); // this has the output type in a string format

    // Check if the function body uses the input variable types correctly
    // and outputs the correct variable type

    // Check if function name is already declared

    // make a map of function names to ASTType * so it can be used in Apply
    // later
  }

  virtual void visit(Apply &Node) override {
    Expr *funcExpr = Node.getfuncname();
    std::vector<Expr *> args = Node.getparams();

    funcExpr->accept(*this);

    for (Expr *arg : args) {
      arg->accept(*this);
    }

    // Check if the function is defined
    // Check if the function exists and is a function
    if (auto *funcVar = llvm::dyn_cast<Var>(funcExpr)) {
      auto funcInfoOpt = lookupVariable(funcVar->getname());
      if (!funcInfoOpt) {
        reportTypeError(*funcVar, "Call to undefined function '" +
                                      funcVar->getname().str() + "'");
        Node.setType(TypeUnknown);
        Node.setASTType(nullptr);
        Node.setElementTypes({});
        return;
      }

      const VariableInfo &funcInfo = *funcInfoOpt;
      if (funcInfo.type != TypeFunc) {
        reportTypeError(*funcVar,
                        "'" + funcVar->getname().str() + "' is not a function");
        Node.setType(TypeUnknown);
        Node.setASTType(nullptr);
        Node.setElementTypes({});
        return;
      }

      // Check parameter count
      if (args.size() != funcInfo.paramTypes.size()) {
        reportTypeError(
            Node, "Function '" + funcVar->getname().str() + "' expects " +
                      std::to_string(funcInfo.paramTypes.size()) +
                      " arguments but got " + std::to_string(args.size()));
        Node.setType(TypeUnknown);
        Node.setASTType(nullptr);
        Node.setElementTypes({});
        return;
      }

      // Check parameter types
      bool typeError = false;

      for (size_t i = 0; i < args.size(); i++) {
        TypeKind expectedType = funcInfo.paramTypes[i];
        TypeKind actualType = args[i]->getType();

        // Try to infer read types if needed
        if (actualType == TypeUnknown) {
          inferReadType(args[i], expectedType);
          actualType = args[i]->getType();
        }

        if (actualType != expectedType && actualType != TypeUnknown) {
          reportTypeError(*args[i],
                          "Function '" + funcVar->getname().str() +
                              "' parameter " + std::to_string(i + 1),
                          expectedType, actualType);
          typeError = true;
        }
      }

      if (typeError) {
        Node.setType(TypeUnknown);
        Node.setASTType(nullptr);
        Node.setElementTypes({});
      } else {
        TypeKind returnKind = funcInfo.returnType;
        Node.setType(returnKind);
        Node.setASTType(
            funcInfo
                .functionSignatureType); // Set the overall function type AST

        if (returnKind == TypeVector) {
          if (funcInfo.functionSignatureType) {
            ASTType *returnASTType =
                funcInfo.functionSignatureType->getouttype();
            if (returnASTType && returnASTType->isvectype()) {
              std::vector<TypeKind> returnedElementTypes;
              bool elementTypeError = false;
              for (ASTType *elemASTType : returnASTType->gettypevec()) {
                TypeKind elemKind = convertAstTypeToTypeKind(elemASTType);
                if (elemKind == TypeUnknown) {
                  elementTypeError = true;
                  break; // Stop processing elements
                }
                returnedElementTypes.push_back(elemKind);
              }

              if (!elementTypeError) {
                Node.setElementTypes(
                    returnedElementTypes); // Set the element types
              } else {
                reportTypeError(Node, "Internal Error: Could not determine "
                                      "element types for vector returned by '" +
                                          funcVar->getname().str() + "'");
                Node.setElementTypes({});  // Clear on error
                Node.setType(TypeUnknown); // Propagate error
              }
            } else {
              reportTypeError(
                  Node,
                  "Internal Error: Type mismatch for vector return type of '" +
                      funcVar->getname().str() + "'");
              Node.setElementTypes({});
              Node.setType(TypeUnknown);
            }
          } else {
            reportTypeError(
                Node,
                "Internal Error: Missing function signature ASTType for '" +
                    funcVar->getname().str() +
                    "' when setting vector element types");
            Node.setElementTypes({});
            Node.setType(TypeUnknown);
          }
        } else {
          Node.setElementTypes({});
        }
      }
    } else {
      reportTypeError(*funcExpr, "Function call target must be an identifier");
      Node.setType(TypeUnknown);
      Node.setASTType(nullptr);
      Node.setElementTypes({}); // Clear element types on error
    }
  }
};
} // namespace

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false;
  ProgramCheck Check;
  Tree->accept(Check);
  return !Check.hasError();
}
