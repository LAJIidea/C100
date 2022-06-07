//
// Created by BY210033 on 2022/5/30.
//

#include "Expr.h"

namespace C100
{
    void AssignExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorAssignExprNode(this);
    }

    void SizeOfExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorSizeOfExprNode(this);
    }

    void BinaryExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorBinaryExprNode(this);
    }

    void UnaryExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorUnaryExprNode(this);
    }

    void NumExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorNumExprNode(this);
    }

    void VarExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorVarExprNode(this);
    }

    void FuncCallExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorFuncCallExprNode(this);
    }

    void StmtExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorStmtExprNode(this);
    }

    void MemberExpr::Accept(AstVisitor *visitor) {
        visitor->VisitorMemberExprNode(this);
    }
}