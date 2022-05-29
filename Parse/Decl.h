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

    class DeclSpecToken : public DeclBase
    {
    public:
        DeclSpecToken(std::shared_ptr<Token> tok) : DeclBase(tok) {
            Cls = DeclClass::DeclSpecToken;
        }
        void Accept(AstVisitor *visitor) override;
    };

    class DeclSpecifier : public DeclBase
    {
    public:
        std::list<std::shared_ptr<DeclSpecToken>> StorageClsList;
        std::list<std::shared_ptr<DeclSpecToken>> TyQualifyList;
        std::list<std::shared_ptr<DeclBase>> TySpecList;
        StorageClass SClass;
        TypeQualifier TyQualify;
        std::string_view TagName;
        DeclSpecifier(std::shared_ptr<Token> tok) : DeclBase(tok) {
            Cls = DeclClass::DeclSpecifier;
        }
        void Accept(AstVisitor *visitor) override;
    };

    class Declarator : public DeclBase
    {
    public:
        std::string_view Id{};
        std::shared_ptr<Type> BaseTy;
        std::shared_ptr<Declarator> Dec{nullptr};
        Declarator(std::shared_ptr<Token> tok): DeclBase(tok) {}
        virtual void Accept(AstVisitor *visitor) = 0;
    };

    class PointerDeclarator : public Declarator
    {
    public:
        PointerDeclarator(std::shared_ptr<Token> tok): Declarator(tok) {
            Cls = DeclClass::PointerDeclarator;
        }
        void Accept(AstVisitor *visitor) override;
    };

    class FunctionDeclarator : public Declarator
    {
    public:
        std::shared_ptr<ParamTypeList> ParamTyLists;
        FunctionDeclarator(std::shared_ptr<Token> tok): Declarator(tok) {
            Cls = DeclClass::FunctionDeclarator;
        }
        void Accept(AstVisitor *visitor) override;
    };

    class ArrayDeclarator : public Declarator
    {
    public:
        int ArrayLen{0};
        ArrayDeclarator(std::shared_ptr<Token> tok): Declarator(tok) {
            Cls = DeclClass::ArrayDeclarator;
        }
        void Accept(AstVisitor *visitor) override
    };

    class Initializer : public DeclBase
    {
    public:
        bool LBrace{false};
        std::shared_ptr<ExprNode> Expr{nullptr};
        std::shared_ptr<InitData> IData;
        std::shared_ptr<Initializer> Initials{nullptr};
        std::shared_ptr<Initializer> Next{nullptr};
        Initializer(std::shared_ptr<Token> tok): DeclBase(tok) {
            Cls = DeclClass::Initializer;
        }
        void Accept(AstVisitor *visitor) override;
    };

    class InitDeclarator : public DeclBase
    {
    public:
        std::string_view Id;
        std::shared_ptr<Type> BaseTy;
        std::shared_ptr<Symbol> Sym;
        std::shared_ptr<Declarator> Dec;
        std::shared_ptr<Initializer> Init{nullptr};
        InitDeclarator(std::shared_ptr<Token> tok): DeclBase(tok) {
            Cls = DeclClass::InitDeclarator;
        }
        void Accept(AstVisitor *visitor) override;
    };

}

#endif //C100_DECL_H
