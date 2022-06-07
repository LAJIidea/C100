//
// Created by BY210033 on 2022/5/30.
//

#include "Stmt.h"

namespace C100
{
    void IfStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorIfStmtNode(this);
    }

    void DoWhileStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorDoWhileStmtNode(this);
    }

    void WhileStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorWhileStmtNode(this);
    }

    void ForStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorForStmtNode(this);
    }

    void ReturnStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorReturnStmtNode(this);
    }

    void BlockStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorBlockStmtNode(this);
    }

    void ExprStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorExprStmtNode(this);
    }

    void BreakStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorBreakStmtNode(this);
    }

    void ContinueStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorContinueStmtNode(this);
    }

    void GotoStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorGotoStmtNode(this);
    }

    void LabelStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorLabelStmtNode(this);
    }

    void CaseStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorCaseStmtNode(this);
    }

    void DefaultStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorDefaultStmtNode(this);
    }

    void SwitchStmtNode::Accept(AstVisitor *visitor) {
        visitor->VisitorSwitchStmtNode(this);
    }
}