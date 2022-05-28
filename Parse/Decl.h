//
// Created by BY210033 on 2022/5/25.
//

#ifndef C100_DECL_H
#define C100_DECL_H

#include "AstVisitor.h"
#include "Symbol.h"
#include <stack>
#include <unordered_map>

namespace C100
{
    enum class DeclClass {
        TranslationUnit,
        Function,
        Declaration,
        DeclSpecifier,
        StructSpecifier,
        UnionSpecifier,
        StructDeclaration,
        StructDeclarator,
        EnumSpecifier,
        EnumDeclarator,
        DeclSpecToken,
        PointerDeclarator,
        NameDeclarator,
        FunctionDeclarator,
        ArrayDeclarator,
        InitDeclarator,
        Initializer,
        ParamTypeList,
        ParamDeclaration
    };

    class DeclBase : public AstNode
    {
    public:
        virtual ~DeclBase() {}
        DeclClass Cls;
        std::shared_ptr<Type> Ty;
        DeclBase(std::shared_ptr<Token> tok) : AstNode(tok) {}
        virtual void Accept(AstVisitor *visitor) = 0;
    };
}

#endif //C100_DECL_H
