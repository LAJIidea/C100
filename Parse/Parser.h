//
// Created by BY210033 on 2022/5/23.
//

#ifndef C100_PARSER_H
#define C100_PARSER_H

#include "AstVisitor.h"
#include "Expr.h"
#include "Decl.h"
#include "Diag.h"
#include "Stmt.h"
#include <set>
#include <unordered_map>

namespace C100 {

    class Parser
    {
    public:
        Parser(Lexer &lex);
        std::shared_ptr<TranslationUnit> ParseTranslationUnit();
    private:
        Lexer &Lex;
        std::set<TokenKind> FirstDeclarationSet;

        std::shared_ptr<DeclBase> ParseExternalDeclaration();
        std::shared_ptr<Declaration> ParseCommonHeader();
        std::shared_ptr<Declaration> ParseDeclaration();
        std::shared_ptr<DeclSpecifier> ParseDeclSpecifier();
        std::shared_ptr<InitDeclarator> ParseInitDeclarator();
        std::shared_ptr<Declarator> ParseDeclarator();
        std::shared_ptr<PointerDeclarator> ParsePointerDeclarator();
        std::shared_ptr<Declarator> ParseDirectDeclarator();
        std::shared_ptr<ParamTypeList> ParseParamTypeList();
        std::shared_ptr<ParamDeclaration> ParseParaDeclaration();
        std::shared_ptr<Initializer> ParseInitializer();
        std::shared_ptr<StructSpecifier> ParseStrucOrUnionSpecifier();
        std::shared_ptr<StructDeclaration> ParseStructDeclaration();
        std::shared_ptr<StructDeclarator> ParseStrucDeclarator();
        std::shared_ptr<EnumSpecifier> ParseEnumSpecifier();
        std::shared_ptr<EnumDeclarator> ParseEnumDeclarator();

        std::shared_ptr<ExprNode> ParseExpr();
        std::shared_ptr<ExprNode> ParseConstantExpression();
        std::shared_ptr<ExprNode> ParseConditionalExpression();
        std::shared_ptr<ExprNode> ParseAssignExpr();
        std::shared_ptr<ExprNode> ParseEqualExpr();
        std::shared_ptr<ExprNode> ParseRelationalExpr();
        std::shared_ptr<ExprNode> ParseAddExpr();
        std::shared_ptr<ExprNode> ParseMultiExpr();
        std::shared_ptr<ExprNode> ParseUnaryExpr();
        std::shared_ptr<ExprNode> ParsePostFixExpr();
        std::shared_ptr<ExprNode> ParseFuncCallNode(std::shared_ptr<ExprNode> expr);
        std::shared_ptr<ExprNode> ParseMemberNode(std::shared_ptr<ExprNode> expr);
        std::shared_ptr<ExprNode> ParsePrimaryExpr();
        std::shared_ptr<ExprNode> ParseStmtExpr();
        std::shared_ptr<ExprNode> ParseNumExpr();
        std::shared_ptr<ExprNode> ParseVarExpr();
        std::shared_ptr<ExprNode> ParseSizeofExpr();

        std::shared_ptr<StmtNode> ParseStmt();
        std::shared_ptr<IfStmtNode> ParseIfStmt();
        std::shared_ptr<DoWhileStmtNode> ParseDoWhileStmt();
        std::shared_ptr<WhileStmtNode> ParseWhileStmt();
        std::shared_ptr<ForStmtNode> ParseForStmt();
        std::shared_ptr<BlockStmtNode> ParseBlockStmt();
        std::shared_ptr<ReturnStmtNode> ParseReturnStmt();
        std::shared_ptr<ExprStmtNode> ParseExprStmt();
        std::shared_ptr<BreakStmtNode> ParseBreakStmt();
        std::shared_ptr<ContinueStmtNode> ParseContinueStmt();
        std::shared_ptr<GotoStmtNode> ParseGotoStmt();
        std::shared_ptr<StmtNode> ParseLabelStmt();
        std::shared_ptr<CaseStmtNode> ParseCaseStmt();
        std::shared_ptr<DefaultStmtNode> ParseDefaultStmt();
        std::shared_ptr<SwitchStmtNode> ParseSwitchStmt();

        bool IsFunctionDeclaration(std::shared_ptr<Declarator> dec, std::string_view &funcName);
    };
}


#endif //C100_PARSER_H
