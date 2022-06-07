//
// Created by BY210033 on 2022/6/6.
//

#include "Sema.h"

using namespace C100;

void Sema::VisitorTranslationUnitNode(TranslationUnit *node) {
    SymTable.EnterScope();
    for (auto &extDec : node->ExtDecl) {
        extDec->Accept(this);
    }
    SymTable.ExitScope();
}

void Sema::VisitorFunctionNode(Function *node) {
    node->Spec->Accept(this);

    node->Dec->BaseTy = node->Spec->Ty;
    node->Dec->Accept(this);
    node->Ty = node->Dec->Ty;

    std::shared_ptr<Symbol> fSym = SymTable.FindVar(node->FuncName);
    if (fSym == nullptr) {
        node->FSym = SymTable.AddFunction(node->FuncName, node->Ty, node->Tok->Location);
    } else if (!fSym->Ty->IsFuncTy()) {
        SemaDiag(node->Tok->Location, "Redeclaration as a function");
    } else {
        if (node->FSym->Defined) {
            SemaDiag(node->Tok->Location, "Function redefinition");
        }
    }
    node->FSym->Defined = true;

    CurrentFunc = node;

    SymTable.EnterScope();

    auto funTy = std::dynamic_pointer_cast<FunctionType>(node->Ty);
    for (auto &param : funTy->Params) {
        auto sym = SymTable.AddFunction(param->Tok->Content, param->Ty, param->Tok->Location);
        node->FSym->Params.push_back(sym);
        node->FSym->Locals.push_back(sym);
    }

    node->BlockStmt->needEnterScope = false;
    node->BlockStmt->Accept(this);

    SymTable.ExitScope();
    CurrentFunc = nullptr;
}

void Sema::VisitorDeclarationNode(Declaration *node) {
    if (node->IsLocalDecl) {
        CheckLocalDeclaration(node);
    } else {
        CheckGlobalDeclaration(node);
    }
}

void Sema::VisitorDeclSpecifierNode(DeclSpecifier *node) {
    CheckStorageSpecifier(node);
    CheckTypeSpecifier(node);
}

void Sema::VisitorPointerDeclaratorNode(PointerDeclarator *node) {
    auto ty = std::make_shared<PointerType>(node->BaseTy);
    if (node->Dec) {
        node->Dec->BaseTy = ty;
        node->Dec->Accept(this);
        node->Ty = node->Dec->Ty;
        node->Id = node->Dec->Id;
    } else {
        node->Ty = ty;
    }
}

void Sema::VisitorNameDeclaratorNode(NameDeclarator *node) {
    auto ty = node->BaseTy;
    if (node->Dec != nullptr) {
        node->Dec->BaseTy = ty;
        node->Dec->Accept(this);
        node->Ty = node->Dec->Ty;
        node->Id = node->Dec->Id;
    } else {
        node->Ty = ty;
    }
}

void Sema::VisitorFuncDeclaratorNode(FunctionDeclarator *node) {
    auto ty = std::make_shared<FunctionType>(node->BaseTy);
    node->ParamTyLists->FunTy = ty;
    node->ParamTyLists->Accept(this);

    if (node->Dec != nullptr) {
        node->Dec->BaseTy = ty;
        node->Dec->Accept(this);
        node->Ty = node->Dec->Ty;
        node->Id = node->Dec->Id;
    } else {
        node->Ty = ty;
    }
}

void Sema::VisitorArrayDeclaratorNode(ArrayDeclarator *node) {
    auto ty = std::make_shared<ArrayType>(node->BaseTy, node->ArrayLen);
    if (node->Dec) {
        node->Dec->BaseTy = ty;
        node->Dec->Accept(this);
        node->Ty = node->Dec->Ty;
        node->Id = node->Dec->Id;
    } else {
        node->Ty = ty;
    }
}

void Sema::VisitorInitDeclaratorNode(InitDeclarator *node) {
    node->Dec->BaseTy = node->BaseTy;
    node->Dec->Accept(this);
    node->Ty = node->Dec->Ty;
    node->Id= node->Dec->Id;

    if (node->Init) {
        node->Init->Ty = node->Ty;
        node->Init->Accept(this);
    }
}

void Sema::VisitorInitializerNode(Initializer *node) {
    CheckInitializerInternal(node);
}

void Sema::VisitorParamTypeListNode(ParamTypeList *node) {
    for (auto &p : node->ParamDecl) {
        p->FunTy = node->FunTy;
        p->Accept(this);
    }
}

void Sema::VisitorParamDeclarationNode(ParamDeclaration *node) {
    node->Spec->Accept(this);
    node->Dec->BaseTy = node->Spec->Ty;
    node->Dec->Accept(this);
    node->Ty = node->Dec->Ty;

    auto param = std::make_shared<Param>();
    param->Ty = node->Ty;
    param->Ty = param->Ty->AdjustParameter(param->Ty);
    param->Tok = node->Dec->Tok;
    node->FunTy->Params.push_back(param);
}

void Sema::VisitorStructSpecifierNode(StructSpecifier *node) {
    std::shared_ptr<Symbol> tag;
    std::shared_ptr<RecordType> ty;
    TypeClass cls = node->Cls == DeclClass::StructSpecifier ? TypeClass::Struct : TypeClass::Union;
    if (!node->Id.empty() && !node->HasLBrace) {
        tag = SymTable.FindTag(node->Id);
        if (tag == nullptr) {
            ty = std::make_shared<RecordType>(cls);
            tag = SymTable.AddTag(node->Id, ty, node->Tok->Location);
        } else {
            ty = std::dynamic_pointer_cast<RecordType>(tag->Ty);
            if (ty->TyCls != cls) {
                SemaDiag(node->Tok->Location, "Inconsistent tag declaration.");
            }
        }
    } else if (node->Id.empty() && node->HasLBrace) {
        ty = std::make_shared<RecordType>(cls);
        ty->Complete = true;
    } else if (!node->Id.empty() && node->HasLBrace) {
        tag = SymTable.FindTag(node->Id);
        if (tag == nullptr) {
            ty = std::make_shared<RecordType>(cls);
            ty->Complete = true;
            tag = SymTable.AddTag(node->Id, ty, node->Tok->Location);
        } else {
            if (tag->Level < SymTable.Level) {
                ty = std::make_shared<RecordType>(cls);
                ty->Complete = true;
                tag = SymTable.AddTag(node->Id, ty, node->Tok->Location);
            } else {
                ty = std::dynamic_pointer_cast<RecordType>(tag->Ty);
                if (ty->TyCls != cls) {
                    SemaDiag(node->Tok->Location, "use of '%s' with tag type that does not match previous declaration",
                             tag->Name.data());
                }
                if (ty->Complete) {
                    SemaDiag(node->Tok->Location, "redefinition");
                }
                ty->Complete = true;
            }
        }
    }
    for (auto &decl : node->DeclList) {
        decl->RecordTy = ty;
        decl->Accept(this);
    }

    ty->FinishLayout(0);

    node->Ty = ty;
}

void Sema::VisitorStructDeclarationNode(StructDeclaration *node) {
    node->Spec->Accept(this);

    if (node->DecList.size() == 0) {
        if (node->Spec->Ty->IsRecordTy()) {
            if (node->Spec->TagName.empty()) {
                node->RecordTy->AddField(std::make_shared<Field>(node->Spec->Ty, 0));
                return;
            }
        }
        return;
    }
    for (auto &dec : node->DecList) {
        dec->RecordTy = node->RecordTy;
        dec->BaseTy = node->Spec->Ty;
        dec->Accept(this);
        dec->Ty = dec->Dec->Ty;
    }
}

void Sema::VisitorStructDeclaratorNode(StructDeclarator *node) {
    if (node->Dec != nullptr) {
        node->Dec->BaseTy = node->BaseTy;
        node->Dec->Accept(this);
        node->Ty = node->Dec->Ty;
        node->Id = node->Dec->Id;
    } else {
        node->Ty = node->BaseTy;
    }

    if (!node->Id.empty() && node->RecordTy->FindField(node->Id) != nullptr) {
        SemaDiag(node->Tok->Location, "member redefinition");
    }
    node->RecordTy->AddField(std::make_shared<Field>(node->Id, node->Ty, 0));
}

void Sema::VisitorEnumSpecifierNode(EnumSpecifier *node) {
    AstVisitor::VisitorEnumSpecifierNode(node);
}

void Sema::VisitorEnumDeclaratorNode(EnumDeclarator *node) {
    AstVisitor::VisitorEnumDeclaratorNode(node);
}

void Sema::VisitorDeclSpecTokenNode(DeclSpecToken *node) {
    AstVisitor::VisitorDeclSpecTokenNode(node);
}

void Sema::VisitorExprStmtNode(ExprStmtNode *node) {
    if (node->Lhs) {
        node->Lhs->Accept(this);
    }
}

void Sema::VisitorIfStmtNode(IfStmtNode *node) {
    node->Cond->Accept(this);
    node->Then->Accept(this);
    if (node->Else)
        node->Else->Accept(this);
}

void Sema::VisitorWhileStmtNode(WhileStmtNode *node) {
    CurrentFunc->LoopStmts.push(node);
    CurrentFunc->Breakables.push(node);

    node->Cond->Accept(this);
    node->Then->Accept(this);

    CurrentFunc->LoopStmts.pop();
    CurrentFunc->Breakables.pop();
}

void Sema::VisitorDoWhileStmtNode(DoWhileStmtNode *node) {
    CurrentFunc->LoopStmts.push(node);
    CurrentFunc->Breakables.push(node);

    node->Stmt->Accept(this);
    node->Cond->Accept(this);

    CurrentFunc->LoopStmts.pop();
    CurrentFunc->Breakables.pop();
}

void Sema::VisitorForStmtNode(ForStmtNode *node) {
    CurrentFunc->LoopStmts.push(node);
    CurrentFunc->Breakables.push(node);

    if (node->InitExpr)
        node->InitExpr->Accept(this);
    else if (node->InitDecl) {
        SymTable.EnterScope();
        node->InitDecl->Accept(this);
    }
    if (node->Cond)
        node->Cond->Accept(this);
    if (node->Inc)
        node->Inc->Accept(this);

    if (node->Stmt->Cls == StmtNodeClass::BlockStmtNode && node->InitDecl) {
        auto blockStmt = std::dynamic_pointer_cast<BlockStmtNode>(node->Stmt);
        blockStmt->needEnterScope = false;
    }
    node->Stmt->Accept(this);

    if (node->InitDecl) {
        SymTable.ExitScope();
    }

    CurrentFunc->LoopStmts.pop();
    CurrentFunc->Breakables.pop();
}

void Sema::VisitorBlockStmtNode(BlockStmtNode *node) {
    if (node->needEnterScope)
        SymTable.EnterScope();

    for (auto &decl : node->Decls) {
        decl->Accept(this);
    }

    for (auto &stmt : node->Stmts) {
        stmt->Accept(this);
    }

    if (node->needEnterScope)
        SymTable.ExitScope();
}

void Sema::VisitorReturnStmtNode(ReturnStmtNode *node) {
    if (node->Lhs) {
        node->Lhs->Accept(this);
    }
}

void Sema::VisitorBreakStmtNode(BreakStmtNode *node) {
    if (CurrentFunc->Breakables.empty()) {
        SemaDiag(node->Tok->Location, "break shall in loop or switch stmt");
    }
}

void Sema::VisitorContinueStmtNode(ContinueStmtNode *node) {
    if (CurrentFunc->LoopStmts.empty()) {
        SemaDiag(node->Tok->Location, "continue shall in loop stmt");
    }
}

void Sema::VisitorGotoStmtNode(GotoStmtNode *node) {
    if (CurrentFunc->LabelsMap.find(node->LabelName) == CurrentFunc->LabelsMap.end()) {
        CurrentFunc->LabelsMap[node->LabelName] = std::make_shared<Label>(node->LabelName);
    }
}

void Sema::VisitorLabelStmtNode(LabelStmtNode *node) {
    if (CurrentFunc->LabelsMap.find(node->LabelName) != CurrentFunc->LabelsMap.end()) {
        auto lab = CurrentFunc->LabelsMap[node->LabelName];
        if (lab->Defined) {
            SemaDiag(node->Tok->Location, "redefinition of label");
        }
        lab->Defined = true;
    } else {
        auto lab = std::make_shared<Label>(node->LabelName);
        lab->Defined = true;
        CurrentFunc->LabelsMap[node->LabelName] = lab;
    }
    node->Stmt->Accept(this);
}

void Sema::VisitorCaseStmtNode(CaseStmtNode *node) {
    if (CurrentFunc->SwitchStmts.empty()) {
        SemaDiag(node->Tok->Location, "'case' statement not in switch statement");
    }
    node->Expr->Accept(this);
    if (!node->Expr->Ty->IsIntegerTy()) {
        SemaDiag(node->Tok->Location, "case expr value should be integer");
    }
    node->Stmt->Accept(this);

    auto &switchNode = CurrentFunc->SwitchStmts.top();
    switchNode->CaseStmtList.push_back(node);
}

void Sema::VisitorDefaultStmtNode(DefaultStmtNode *node) {
    if (CurrentFunc->SwitchStmts.empty()) {
        SemaDiag(node->Tok->Location, "multiple default label in one switch");
    }

    auto &switchNode = CurrentFunc->SwitchStmts.top();
    if (switchNode->DefaultStmt) {
        SemaDiag(node->Tok->Location, "multiple default labels in one switch");
    }
    switchNode->DefaultStmt = node;

    node->Stmt->Accept(this);
}

void Sema::VisitorSwitchStmtNode(SwitchStmtNode *node) {
    CurrentFunc->SwitchStmts.push(node);
    CurrentFunc->Breakables.push(node);

    node->Expr->Accept(this);
    if (!node->Expr->Ty->IsIntegerTy()) {
        SemaDiag(node->Tok->Location, "switch expr value should be integer");
    }
    node->Stmt->Accept(this);

    CurrentFunc->SwitchStmts.pop();
    CurrentFunc->Breakables.pop();
}

void Sema::VisitorAssignExprNode(AssignExpr *node) {
    node->Lhs->Accept(this);
    node->Rhs->Accept(this);
    node->Ty = node->Lhs->Ty;
}

void Sema::VisitorSizeOfExprNode(SizeOfExpr *node) {
    node->Lhs->Accept(this);
    node->Ty = node->Lhs->Ty;
}

void Sema::VisitorBinaryExprNode(BinaryExpr *node) {
    switch (node->BinOp) {
        case BinaryOperator::Add:
            CheckAddOperator(node);
            break;
        case BinaryOperator::Sub:
            CheckSubOperator(node);
            break;
        case BinaryOperator::Mul:
        case BinaryOperator::Div: {
            node->Lhs->Accept(this);
            node->Rhs->Accept(this);
            node->Ty = node->Lhs->Ty;
            if (!node->Lhs->Ty->IsArithTy() && !node->Rhs->Ty->IsArithTy()) {
                SemaDiag(node->Tok->Location, "Invalid operands");
            }
            break;
        }
        case BinaryOperator::Equal:
        case BinaryOperator::PipeEqual:
        case BinaryOperator::Greater:
        case BinaryOperator::GreaterEqual:
        case BinaryOperator::Lesser:
        case BinaryOperator::LesserEqual: {
            node->Lhs->Accept(this);
            node->Rhs->Accept(this);
            node->Ty = Type::LongTy;
            break;
        }
        default:
            break;
    }
}

void Sema::VisitorUnaryExprNode(UnaryExpr *node) {
    node->Lhs->Accept(this);
    switch (node->Uop) {
        case UnaryOperator::Plus:
        case UnaryOperator::Minus: {
            node->Ty  = node->Lhs->Ty;
            break;
        }
        case UnaryOperator::Amp: {
            node->Ty = std::make_shared<PointerType>(node->Lhs->Ty);
            break;
        }
        case UnaryOperator::Star: {
            if (node->Lhs->Ty->IsPointerTy()) {
                node->Ty = std::dynamic_pointer_cast<PointerType>(node->Lhs->Ty)->BaseTy;
            } else if (node->Lhs->Ty->IsArithTy()) {
                node->Ty = std::dynamic_pointer_cast<ArrayType>(node->Lhs->Ty)->ElementType;
            } else {
                SemaDiag(node->Tok->Location, "invalid dereference operation");
                assert(0);
            }
            break;
        }
        default:
            break;
    }
}

void Sema::VisitorNumExprNode(NumExpr *node) {
    node->Ty = Type::LongTy;
}

void Sema::VisitorVarExprNode(VarExpr *node) {
    std::shared_ptr<Symbol> sym = SymTable.FindVar(node->VarName);
    if (!sym) {

    }
}

void Sema::VisitorFuncCallExprNode(FuncCallExpr *node) {

}

void Sema::VisitorStmtExprNode(StmtExpr *node) {

}

void Sema::VisitorMemberExprNode(MemberExpr *node) {

}

void Sema::CheckStorageSpecifier(DeclSpecifier *declSpecifier) {
    /// storage class specifier
    if (declSpecifier->StorageClsList.size() == 0) {
        declSpecifier->SClass = StorageClass::UnSpecified;
    } else if (declSpecifier->StorageClsList.size() == 1) {
        auto tok = std::dynamic_pointer_cast<DeclSpecToken>(declSpecifier->StorageClsList.front())->Tok;
        switch (tok->Kind) {
            case TokenKind::Auto:
                declSpecifier->SClass = StorageClass::Auto;
                break;
            case TokenKind::Register:
                declSpecifier->SClass = StorageClass::Register;
                break;
            case TokenKind::Static:
                declSpecifier->SClass = StorageClass::Static;
                break;
            case TokenKind::Extern:
                declSpecifier->SClass = StorageClass::Extern;
                break;
            case TokenKind::Typedef:
                declSpecifier->SClass = StorageClass::Typedef;
                break;
            default:
                break;
        }
    } else {
        SemaDiag(declSpecifier->Tok->Location, "At most one storage class");
    }
}

void Sema::CheckTypeSpecifier(DeclSpecifier *declSpecifier) {
    std::shared_ptr<Type> ty;

    int tyCnt = 0, signCnt = 0, sizeCnt = 0;
    TokenKind size = TokenKind::Eof, sign = TokenKind::Eof;

    for (auto &p : declSpecifier->TySpecList)
    {
        switch (p->Cls) {
            case DeclClass::StructSpecifier:
            case DeclClass::UnionSpecifier: {
                tyCnt++;
                p->Accept(this);
                ty = p->Ty;
                declSpecifier->TagName = std::dynamic_pointer_cast<StructSpecifier>(p)->Id;
                break;
            }
            case DeclClass::EnumSpecifier: {
                tyCnt++;
                p->Accept(this);
                ty = p->Ty;
                declSpecifier->TagName = std::dynamic_pointer_cast<EnumSpecifier>(p)->Id;
                break;
            }
            default: {
                auto tok = std::dynamic_pointer_cast<DeclSpecifier>(p)->Tok;
                switch (tok->Kind) {
                    case TokenKind::Float:
                    case TokenKind::Double: {
                        assert(0);
                        break;
                    }
                    case TokenKind::Void: {
                        tyCnt++;
                        ty = Type::VoidTy;
                        break;
                    }
                    case TokenKind::Bool: {
                        tyCnt++;
                        ty = Type::BoolTy;
                        break;
                    }
                    case TokenKind::Char: {
                        tyCnt++;
                        ty = Type::CharTy;
                        break;
                    }
                    case TokenKind::Int: {
                        tyCnt++;
                        ty = Type::IntTy;
                        break;
                    }
                    case TokenKind::Long: {
                        if (size == TokenKind::Long && sizeCnt == 1) {
                            size = TokenKind::LongLong;
                        } else {
                            sizeCnt++;
                            size = TokenKind::Long;
                        }
                        break;
                    }
                    default:
                        assert(0);
                }
            }
        }
    }
    ty = tyCnt == 0 ? Type::IntTy : ty;
    if (signCnt > 1 || sizeCnt > 1 || tyCnt > 1) {
        goto err;
    }
    if (ty == Type::CharTy) {
        switch (size) {
            case TokenKind::Short: {
                ty = Type::ShortTy;
                break;
            }
            case TokenKind::Long: {
                ty = Type::LongTy;
                break;
            }
            case TokenKind::LongLong: {
                ty = Type::LongTy;
                break;
            }
            default:
                assert(0);
                break;
        }
    } else if (sign != TokenKind::Eof || size != TokenKind::Eof) {
        goto err;
    }
    declSpecifier->Ty = ty;
    return;
    err:
    SemaDiag(declSpecifier->Tok->Location, "Illegal type specifier.");
    declSpecifier->Ty = Type::IntTy;
    return;
}

void Sema::CheckLocalDeclaration(Declaration *decl) {
    decl->Spec->Accept(this);

    for (auto &initDec : decl->DecList) {
        initDec->BaseTy = decl->Spec->Ty;
        initDec->Accept(this);
        initDec->Ty = initDec->Dec->Ty;

        std::shared_ptr<Symbol> varSym = SymTable.FindVar(initDec->Id);
        if (varSym == nullptr || varSym->Level < SymTable.Level) {
            auto sym = SymTable.AddVariable(initDec->Id, initDec->Ty, initDec->Tok->Location);
            if (initDec->Init) {
                sym->Init = initDec->Init->IData;
            }
            initDec->Sym = sym;
            CurrentFunc->FSym->Locals.push_back(sym);
        } else {
            if (initDec->Ty->TyCls == varSym->Ty->TyCls) {
                SemaDiag(initDec->Tok->Location, "redefinition of \'%s\'", std::string(initDec->Id).data());
            } else {
                SemaDiag(initDec->Tok->Location, "redefinition of \'%s\' as different kind of symbol", std::string(initDec->Id).data());
            }
        }
    }
}

void Sema::CheckGlobalDeclaration(Declaration *decl) {
    decl->Spec->Accept(this);

    for (auto &initDec : decl->DecList) {
        initDec->BaseTy = decl->Spec->Ty;
        initDec->Accept(this);

        if (initDec->Ty->IsFuncTy()) {
            if (initDec->Init)
                SemaDiag(initDec->Tok->Location, "please don't initialize function");
            std::shared_ptr<Symbol> sym = SymTable.FindVar(initDec->Id);
            if (sym == nullptr)
                sym = SymTable.AddFunction(initDec->Id, initDec->Ty, initDec->Tok->Location);
        } else {
            std::shared_ptr<Symbol> sym = SymTable.FindVar(initDec->Id);
            if (sym == nullptr)
                sym = SymTable.AddVariable(initDec->Id, initDec->Ty, initDec->Tok->Location);
            if (initDec->Init) {
                if (sym->Init) {
                    if (sym->Defined) {
                        SemaDiag(initDec->Tok->Location, "redefinition of %s", std::string(initDec->Id).data());
                    } else {
                        sym->Init = initDec->Init->IData;
                        sym->Defined = true;
                    }
                }
            }
        }
    }
}

void Sema::CheckAddOperator(BinaryExpr *expr) {
    expr->Lhs->Accept(this);
    expr->Rhs->Accept(this);
    expr->Ty = expr->Lhs->Ty;

    auto lhsTy = AdjustExprTy(expr->Lhs.get());
    auto rhsTy = AdjustExprTy(expr->Rhs.get());

    if (lhsTy->IsArithTy() && rhsTy->IsArithTy()) {
        return;
    }

    if (rhsTy->IsObjPtr() && lhsTy->IsIntegerTy()) {
        SwapKids(expr);
        lhsTy = AdjustExprTy(expr->Lhs.get());
        rhsTy = AdjustExprTy(expr->Rhs.get());
    }

    if (lhsTy->IsObjPtr() && rhsTy->IsIntegerTy()) {
        auto binNode = std::make_shared<BinaryExpr>(BinaryOperator::Mul, expr->Tok);
        auto num = std::make_shared<NumExpr>(expr->Tok);
        num->Value = std::dynamic_pointer_cast<PointerType>(lhsTy)->BaseTy->Size;
        binNode->Lhs = num;
        binNode->Rhs = expr->Rhs;

        expr->Rhs = binNode;
        expr->Ty = expr->Lhs->Ty;
        return;
    }
    SemaDiag(expr->Tok->Location, "Invalid operands");
}

void Sema::CheckSubOperator(BinaryExpr *expr) {
    expr->Lhs->Accept(this);
    expr->Rhs->Accept(this);
    expr->Ty = expr->Lhs->Ty;

    auto lhsTy = AdjustExprTy(expr->Lhs.get());
    auto rhsTy = AdjustExprTy(expr->Rhs.get());

    if (lhsTy->IsArithTy() && rhsTy->IsArithTy()) {
        return;
    }

    if (lhsTy->IsObjPtr() && rhsTy->IsIntegerTy()) {
        auto binNode = std::make_shared<BinaryExpr>(BinaryOperator::Mul, expr->Tok);
        auto num = std::make_shared<NumExpr>(expr->Tok);
        num->Value = std::dynamic_pointer_cast<PointerType>(lhsTy)->BaseTy->Size;
        binNode->Lhs = num;
        binNode->Rhs = expr->Rhs;

        expr->Rhs = binNode;
        expr->Ty = expr->Lhs->Ty;
        return;
    }

    if (lhsTy->IsObjPtr() && rhsTy->IsObjPtr()) {
        expr->Ty = Type::LongTy;

        auto num = std::make_shared<NumExpr>(expr->Tok);
        num->Value = std::dynamic_pointer_cast<PointerType>(lhsTy)->BaseTy->Size;

        auto divLeft = std::make_shared<BinaryExpr>(BinaryOperator::Div, expr->Tok);
        divLeft->Lhs = expr->Lhs;
        divLeft->Rhs = num;

        auto divRight = std::make_shared<BinaryExpr>(BinaryOperator::Div, expr->Tok);
        divRight->Lhs = expr->Rhs;
        divRight->Rhs = num;

        expr->Lhs = divLeft;
        expr->Rhs = divRight;
        return;
    }
    SemaDiag(expr->Tok->Location, "Invalid operands");
}

void Sema::CheckInitializerInternal(Initializer *node) {
    if (node->Ty->IsScalarType()) {
        if (node->LBrace) {
            SemaDiag(node->Tok->Location, "braces around scalar initializer");
        }
        node->Expr->Accept(this);

        auto iData = std::make_shared<InitData>();
        iData->Expr = node->Expr;
        iData->Next = nullptr;
        node->IData = iData;
        return;
    }
    SemaDiag(node->Tok->Location, "not support yet");
}

std::shared_ptr<Type> Sema::AdjustExprTy(ExprNode *expr) {
    if (expr->Ty->IsFuncTy()) {
        return std::make_shared<PointerType>(expr->Ty);
    } else if (expr->Ty->IsArrayTy()) {
        return std::make_shared<PointerType>(std::dynamic_pointer_cast<ArrayType>(expr->Ty)->ElementType);
    }
    return expr->Ty;
}

void Sema::SwapKids(BinaryExpr *expr) {
    auto t = expr->Lhs;
    expr->Lhs = expr->Rhs;
    expr->Rhs = t;
}
