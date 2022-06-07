//
// Created by BY210033 on 2022/6/6.
//

#ifndef C100_SEMA_H
#define C100_SEMA_H

#include "AstVisitor.h"
#include "SymbolTable.h"
#include "Decl.h"
#include "Expr.h"
#include "Stmt.h"
#include "Type.h"
#include "Diag.h"
#include <set>
#include <stack>
#include <string_view>

namespace C100
{
    class Sema : public AstVisitor
    {
    public:
        void VisitorTranslationUnitNode(TranslationUnit *node) override;

        void VisitorFunctionNode(Function *node) override;

        void VisitorDeclarationNode(Declaration *node) override;

        void VisitorDeclSpecifierNode(DeclSpecifier *node) override;

        void VisitorPointerDeclaratorNode(PointerDeclarator *node) override;

        void VisitorNameDeclaratorNode(NameDeclarator *node) override;

        void VisitorFuncDeclaratorNode(FunctionDeclarator *node) override;

        void VisitorArrayDeclaratorNode(ArrayDeclarator *node) override;

        void VisitorInitDeclaratorNode(InitDeclarator *node) override;

        void VisitorInitializerNode(Initializer *node) override;

        void VisitorParamTypeListNode(ParamTypeList *node) override;

        void VisitorParamDeclarationNode(ParamDeclaration *node) override;

        void VisitorStructSpecifierNode(StructSpecifier *node) override;

        void VisitorStructDeclarationNode(StructDeclaration *node) override;

        void VisitorStructDeclaratorNode(StructDeclarator *node) override;

        void VisitorEnumSpecifierNode(EnumSpecifier *node) override;

        void VisitorEnumDeclaratorNode(EnumDeclarator *node) override;

        void VisitorDeclSpecTokenNode(DeclSpecToken *node) override;

        void VisitorExprStmtNode(ExprStmtNode *node) override;

        void VisitorIfStmtNode(IfStmtNode *node) override;

        void VisitorWhileStmtNode(WhileStmtNode *node) override;

        void VisitorDoWhileStmtNode(DoWhileStmtNode *node) override;

        void VisitorForStmtNode(ForStmtNode *node) override;

        void VisitorBlockStmtNode(BlockStmtNode *node) override;

        void VisitorReturnStmtNode(ReturnStmtNode *node) override;

        void VisitorBreakStmtNode(BreakStmtNode *node) override;

        void VisitorContinueStmtNode(ContinueStmtNode *node) override;

        void VisitorGotoStmtNode(GotoStmtNode *node) override;

        void VisitorLabelStmtNode(LabelStmtNode *node) override;

        void VisitorCaseStmtNode(CaseStmtNode *node) override;

        void VisitorDefaultStmtNode(DefaultStmtNode *node) override;

        void VisitorSwitchStmtNode(SwitchStmtNode *node) override;

        void VisitorAssignExprNode(AssignExpr *node) override;

        void VisitorSizeOfExprNode(SizeOfExpr *node) override;

        void VisitorBinaryExprNode(BinaryExpr *node) override;

        void VisitorUnaryExprNode(UnaryExpr *node) override;

        void VisitorNumExprNode(NumExpr *node) override;

        void VisitorVarExprNode(VarExpr *node) override;

        void VisitorFuncCallExprNode(FuncCallExpr *node) override;

        void VisitorStmtExprNode(StmtExpr *node) override;

        void VisitorMemberExprNode(MemberExpr *node) override;

    private:
        SymbolTable SymTable;
        Function *CurrentFunc;

        void CheckStorageSpecifier(DeclSpecifier *declSpecifier);

        void CheckTypeSpecifier(DeclSpecifier *declSpecifier);

        void CheckLocalDeclaration(Declaration *decl);

        void CheckGlobalDeclaration(Declaration *decl);

        void CheckAddOperator(BinaryExpr *expr);

        void CheckSubOperator(BinaryExpr *expr);

        void CheckInitializerInternal(Initializer *node);

        std::shared_ptr<Type> AdjustExprTy(ExprNode *expr);

        void SwapKids(BinaryExpr *expr);
    };
}

#endif //C100_SEMA_H
