//
// Created by BY210033 on 2022/5/26.
//

#ifndef C100_SYMBOL_H
#define C100_SYMBOL_H

#include <string_view>
#include "Type.h"

namespace C100
{
    enum class SymbolKind {
        Tag,
        Variable,
        Constant,
        Function
    };

    class ExprNode;
    struct InitData
    {
        int Offset;
        std::shared_ptr<ExprNode> Expr{nullptr};
        std::shared_ptr<ExprNode> Next{nullptr};
    };

    class Symbol {
    public:
        SymbolKind Cls;
        std::string_view Name{};
        std::shared_ptr<Type> Ty;
        int Level{0};
        bool Defined{false};
        SourceLocation Loc;
        std::shared_ptr<InitData> Init{nullptr};
        int Offset;
        std::list<std::shared_ptr<Symbol>> Params{};
        std::list<std::shared_ptr<Symbol>> Locals{};

        Symbol(SymbolKind cls): Cls(cls), Ty(nullptr) {}
    };
}

#endif //C100_SYMBOL_H
