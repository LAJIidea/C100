//
// Created by kingkiller on 2022/5/28.
//

#ifndef C100_SYMBOLTABLE_H
#define C100_SYMBOLTABLE_H

#include "AstNode.h"
#include "Symbol.h"
#include <stack>
#include <string_view>
#include <unordered_map>

namespace C100
{
    class SymbolTable {
    private:
        class Env
        {
        public:
            std::unordered_map<std::string_view, std::shared_ptr<Symbol>> VarEnv;
            std::unordered_map<std::string_view, std::shared_ptr<Symbol>> TagEnv;
        };
        std::list<std::shared_ptr<Env>> SymTab;
    public:
        int Level{-1};
        std::list<std::shared_ptr<Symbol>> FuncSymList;
        std::list<std::shared_ptr<Symbol>> GlobalSymList;

        void EnterScope();

        void ExitScope();

        std::shared_ptr<Symbol> FindVar(std::string_view id);

        std::shared_ptr<Symbol> FindTah(std::string_view id);

        std::shared_ptr<Symbol> AddTag(std::string_view id, std::shared_ptr<Type> ty, SourceLocation loc);

        std::shared_ptr<Symbol> AddVariable(std::string_view id, std::shared_ptr<Type> ty, SourceLocation loc);

        std::shared_ptr<Symbol> AddFunction(std::string_view id, std::shared_ptr<Type> ty, SourceLocation loc);
    };
}

#endif //C100_SYMBOLTABLE_H
