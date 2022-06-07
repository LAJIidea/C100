//
// Created by BY210033 on 2022/5/25.
//

#ifndef C100_ASTNODE_H
#define C100_ASTNODE_H

#include <memory>
#include <list>
#include <string_view>
#include <utility>
#include <vector>
#include "Lexer.h"

namespace C100 {
    class AstVisitor;

    class AstNode
    {
    public:
        std::shared_ptr<Token> Tok;
        virtual ~AstNode() {}
        AstNode(std::shared_ptr<Token> tok): Tok(tok) {}
        virtual void Accept(AstVisitor *visitor) = 0;
    };

    enum class TagKind {
        Struct,
        Union
    };

    // storage-class-specifier
    enum class StorageClass {
        UnSpecified, Typedef, Extern, Static,
        Auto, Register
    };

    // type-specifier
    enum class TypeSpecifier {
        UnSpecified, Void, Char, Short, Int,
        Long, Float, Double, Signed, UnSigned,
        Bool, Struct, Union, Enum
    };

    // type-qualifier
    enum class TypeQualifier {
        UnSpecified, Const, Restrict, Volatile
    };
}

#endif //C100_ASTNODE_H
