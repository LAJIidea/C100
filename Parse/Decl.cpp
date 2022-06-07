//
// Created by kingkiller on 2022/5/28.
//

#include "Decl.h"

namespace C100
{
    void DeclSpecToken::Accept(AstVisitor *visitor) {
        visitor->VisitorDeclSpecTokenNode(this);
    }

    void InitDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorInitDeclaratorNode(this);
    }

    void DeclSpecifier::Accept(AstVisitor *visitor) {
        visitor->VisitorDeclSpecifierNode(this);
    }

    void Initializer::Accept(AstVisitor *visitor) {
        visitor->VisitorInitializerNode(this);
    }

    void ParamTypeList::Accept(AstVisitor *visitor) {
        visitor->VisitorParamTypeListNode(this);
    }

    void ParamDeclaration::Accept(AstVisitor *visitor) {
        visitor->VisitorParamDeclarationNode(this);
    }

    void NameDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorNameDeclaratorNode(this);
    }

    void PointerDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorPointerDeclaratorNode(this);
    }

    void ArrayDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorArrayDeclaratorNode(this);
    }

    void FunctionDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorFuncDeclaratorNode(this);
    }

    void StructSpecifier::Accept(AstVisitor *visitor) {
        visitor->VisitorStructSpecifierNode(this);
    }

    void StructDeclaration::Accept(AstVisitor *visitor) {
        visitor->VisitorStructDeclarationNode(this);
    }

    void StructDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorStructDeclaratorNode(this);
    }

    void EnumSpecifier::Accept(AstVisitor *visitor) {
        visitor->VisitorEnumSpecifierNode(this);
    }

    void EnumDeclarator::Accept(AstVisitor *visitor) {
        visitor->VisitorEnumDeclaratorNode(this);
    }

    void Declaration::Accept(AstVisitor *visitor) {
        visitor->VisitorDeclarationNode(this);
    }

    void Function::Accept(AstVisitor *visitor) {
        visitor->VisitorFunctionNode(this);
    }

    void TranslationUnit::Accept(AstVisitor *visitor) {
        visitor->VisitorTranslationUnitNode(this);
    }
}