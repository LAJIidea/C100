//
// Created by BY210033 on 2022/5/23.
//

#include "Parser.h"
using namespace C100;
Parser::Parser(C100::Lexer &lex): Lex(lex) {
    Lex.GetNextToken();
    FirstDeclarationSet = {
            TokenKind::Void, TokenKind::Bool, TokenKind::Char, TokenKind::Short, TokenKind::Int,
            TokenKind::Long, TokenKind::Float, TokenKind::Double, TokenKind::Signed, TokenKind::UnSigned,
            TokenKind::Struct, TokenKind::Union, TokenKind::Enum,
            TokenKind::Auto, TokenKind::Static, TokenKind::Register, TokenKind::Extern, TokenKind::Typedef,
            TokenKind::Const, TokenKind::Volatile, TokenKind::Restrict
    };
}

/**
 * Parse Translation Unit
 * translation-unit:
 *      external-declaration
 *      translation-unit external-declaration
 * external-declaration:
 *      function-definition
 *      declaration
 * @return TranslationUnit AST
 */
std::shared_ptr<TranslationUnit> Parser::ParseTranslationUnit() {
    auto node = std::make_shared<TranslationUnit>(Lex.CurrentToken);
    while (!Lex.Match(TokenKind::Eof)) {
        node->ExtDecl.push_back(ParseExternalDeclaration());
    }
    return node;
}

/**
 * Parse external-declaration
 * external-declaration:
 *      function-definition
 *      declaration
 * declaration:
 *      declaration-specifiers init-declarator-list? ;
 * function-definition:
 *      declaration-specifiers declarator declaration-list? compound-statement
 * @return DeclBase AST
 */
std::shared_ptr<DeclBase> Parser::ParseExternalDeclaration() {
    auto decl = ParseCommonHeader();

    auto OnlyOneDec = [&decl]()->bool {
        return decl->DecList.size() == 1 && decl->DecList.front()->Init == nullptr;
    };

    if (OnlyOneDec())
    {
        std::string_view funcName;
        bool isFunc = IsFunctionDeclaration(decl->DecList.front()->Dec, funcName);
        if (isFunc) {
            /// function declaration
            if (Lex.Match(TokenKind::Semicolon)) {
                decl->IsLocalDecl = false;
                Lex.SkipToken(TokenKind::Semicolon);
                return decl;
            }
            /// function definition
            auto funcNode = std::make_shared<Function>(Lex.CurrentToken);
            funcNode->FuncName = funcName;
            funcNode->Spec = decl->Spec;
            funcNode->Dec = decl->DecList.front()->Dec;
            funcNode->BlockStmt = ParseBlockStmt();
            return funcNode;
        }
    }

    /// global declaration
    Lex.ExpectToken(TokenKind::Semicolon);
    decl->IsLocalDecl = false;
    return decl;
}

/**
 * Parse header
 * common header:
 * declaration-specifiers init-declarator-list?
 *
 * init-declarator-list:
 *    init-declarator
 *    init-declarator-list, init-declarator
 * @return AST of Declaration without Semicolon
 */
std::shared_ptr<Declaration> Parser::ParseCommonHeader() {
    auto node = std::make_shared<Declaration>(Lex.CurrentToken);
    node->Spec = ParseDeclSpecifier();
    if (!Lex.Match(TokenKind::Semicolon)) {
        node->DecList.push_back(ParseInitDeclarator());
        while (Lex.CurrentToken->Kind == TokenKind::Comma) {
            Lex.SkipToken(TokenKind::Comma);
            node->DecList.push_back(ParseInitDeclarator());
        }
    }
    return node;
}

/**
 * Parse declaration
 * declaration:
 *      declaration-specifiers init-declarator-list? ;
 * @return Declaration AST
 */
std::shared_ptr<Declaration> Parser::ParseDeclaration() {
    auto node = ParseCommonHeader();
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

/**
 * Parse DeclSpecifier
 * declaration-specifiers:
 *      storage-class-specifier declaration-specifiers?
 *      type-specifier declaration-specifiers?
 *      type-specifier declaration-specifiers?
 *
 * storage-class-specifier:
 *      typedef extern static auto register
 *
 * type-specifier:
 *      void char short int long float double
 *      signed unsigned bool complex
 *      struct-or-union-specifier
 *      enum-specifier typedef-name
 *
 * type-qualifier:
 *      const restrict volatile
 * @return DeclSpecifier AST
 */
std::shared_ptr<DeclSpecifier> Parser::ParseDeclSpecifier() {
    auto node = std::make_shared<DeclSpecifier>(Lex.CurrentToken);
    while (true) {
        switch (Lex.CurrentToken->Kind) {
            case TokenKind::Auto:
            case TokenKind::Static:
            case TokenKind::Extern:
            case TokenKind::Typedef:
            case TokenKind::Register: {
                auto tok = std::make_shared<DeclSpecToken>(Lex.CurrentToken);
                node->StorageClsList.push_back(tok);
                Lex.GetNextToken();
                break;
            }
            case TokenKind::Const:
            case TokenKind::Volatile:
            case TokenKind::Restrict: {
                auto tok = std::make_shared<DeclSpecToken>(Lex.CurrentToken);
                node->TyQualifyList.push_back(tok);
                Lex.GetNextToken();
                break;
            }
            case TokenKind::Void:
            case TokenKind::Bool:
            case TokenKind::Char:
            case TokenKind::Short:
            case TokenKind::Int:
            case TokenKind::Long:
            case TokenKind::Float:
            case TokenKind::Double:
            case TokenKind::Signed:
            case TokenKind::UnSigned: {
                auto tok = std::make_shared<DeclSpecToken>(Lex.CurrentToken);
                node->TySpecList.push_back(tok);
                Lex.GetNextToken();
                break;
            }
            case TokenKind::Struct:
            case TokenKind::Union: {
                node->TySpecList.push_back(ParseStrucOrUnionSpecifier());
                break;
            }
            case TokenKind::Enum: {
                node->TySpecList.push_back(ParseEnumSpecifier());
                break;
            }
            default:
                return node;
        }
    }
}

/**
 * parser init declarator
 * init-declarator:
 *      declarator
 *      declarator = initializer
 * @return init declarator
 */
std::shared_ptr<InitDeclarator> Parser::ParseInitDeclarator() {
    auto node = std::make_shared<InitDeclarator>(Lex.CurrentToken);
    node->Dec = ParseDeclarator();
    node->Id = node->Dec->Id;
    if (Lex.Match(TokenKind::Assign)) {
        Lex.SkipToken(TokenKind::Assign);
        node->Init = ParseInitializer();
    }
    return node;
}

/**
 * Parse Declarator
 * declarator:
 *      pointer? direct-declarator
 * @return Declarator AST
 */
std::shared_ptr<Declarator> Parser::ParseDeclarator() {
    std::shared_ptr<Declarator> dec = ParsePointerDeclarator();
    if (!dec) {
        return ParseDirectDeclarator();
    }

    auto p = dec;
    while (p && p->Dec) {
        p = p->Dec;
    }
    p->Dec = ParseDirectDeclarator();
    p->Id = p->Dec->Id;

    return dec;
}

/**
 * Parse Pointer Declarator
 * pointer:
 *      * type-qualifier-list?
 *      * type-qualifier-list? pointer
 * @return PointerDeclarator AST
 */
std::shared_ptr<PointerDeclarator> Parser::ParsePointerDeclarator() {
    if (Lex.CurrentToken->Kind == TokenKind::Star) {
        auto node = std::make_shared<PointerDeclarator>(Lex.CurrentToken);
        Lex.SkipToken(TokenKind::Star);
        node->Dec = ParsePointerDeclarator();
        return node;
    }
    return nullptr;
}

/**
 * Parse Direct Declarator
 * direct-declarator:
 *      identifier
 *      ( declarator )
 *      direct-declarator [ type-qualifier-list? assignment-expression? ]
 *      direct-declarator [ static type-qualifier-list? assignment-expression ]
 *      direct-declarator [ type-qualifier-list static assignment-expression ]
 *      direct-declarator [ type-qualifier-list? * ]
 *      direct-declarator ( parameter-type-list )
 *      direct-declarator ( identifier-list? )
 * @return DirectDeclarator AST
 */
std::shared_ptr<Declarator> Parser::ParseDirectDeclarator() {
    std::shared_ptr<Declarator> dec;

    if (Lex.Match(TokenKind::LParen)) {
        Lex.SkipToken(TokenKind::LParen);
        dec = ParseDeclarator();
        Lex.ExpectToken(TokenKind::RParen);
    } else {
        if (!Lex.Match(TokenKind::Id)) {
            ParseDiag(Lex.CurrentToken->Location, "expected a identifier");
        }
        dec = std::make_shared<NameDeclarator>(Lex.CurrentToken);
        dec->Id = Lex.CurrentToken->Content;
        Lex.SkipToken(TokenKind::Id);
    }

    while (true)
    {
        if (Lex.Match(TokenKind::LBracket))
        {
            auto arrayDec = std::make_shared<ArrayDeclarator>(Lex.CurrentToken);
            arrayDec->Dec = dec;
            arrayDec->Id = dec->Id;
            Lex.SkipToken(TokenKind::LBracket);
            if (Lex.CurrentToken->Kind != TokenKind::Num) {
                ParseDiag(Lex.CurrentToken->Location, "expected a num");
            }
            arrayDec->ArrayLen = Lex.CurrentToken->Value;
            Lex.SkipToken(TokenKind::Num);
            Lex.ExpectToken(TokenKind::RBracket);
            dec = arrayDec;
        } else if (Lex.Match(TokenKind::LParen)) {
            auto funcDec = std::make_shared<FunctionDeclarator>(Lex.CurrentToken);
            funcDec->Dec = dec;
            funcDec->Id = dec->Id;
            Lex.SkipToken(TokenKind::LParen);
            Lex.ExpectToken(TokenKind::RParen);
            dec = funcDec;
        } else {
            break;
        }
    }
    return dec;
}

/**
 * Parse Parameter Type List
 * parameter-type-list:
 *      parameter-list
 *      parameter-list , ...
 *
 * parameter_list:
 *      parameter-declaration
 *      parameter-list, parameter-declaration
 * @return ParamTypeList AST
 */
std::shared_ptr<ParamTypeList> Parser::ParseParamTypeList() {
    auto node = std::make_shared<ParamTypeList>(Lex.CurrentToken);
    if (!Lex.Match(TokenKind::RParen)) {
        node->ParamDecl.push_back(ParseParaDeclaration());
    }
    while (!Lex.Match(TokenKind::RParen)) {
        Lex.ExpectToken(TokenKind::Comma);
        if (Lex.CurrentToken->Kind == TokenKind::Ellipsis) {
            node->HaveEllipsis = true;
            break;
        }
        node->ParamDecl.push_back(ParseParaDeclaration());
    }
    return node;
}

/**
 * Parse Parameter Declaration
 * parameter-declaration:
 *      declaration-specifiers declarator
 *      declaration-specifiers abstract-declarator?
 * @return ParaDeclaration AST
 */
std::shared_ptr<ParamDeclaration> Parser::ParseParaDeclaration() {
    auto node = std::make_shared<ParamDeclaration>(Lex.CurrentToken);
    node->Spec = ParseDeclSpecifier();
    node->Dec = ParseDeclarator();
    return node;
}

/**
 * Parse Initializer
 * initializer:
 *      assignment-expression
 *      { initializer-list }
 *      { initializer-list }
 *
 * initializer-list:
 *      designation? initializer
 *      initializer-list , designation? initializer
 *
 * designation:
 *      designation-list =
 *
 * designator-list:
 *      designator
 *      designator-list designator
 *
 * designator:
 *      [ constant-expression ]
 *      . identifier
 * @return Initializer AST
 */
std::shared_ptr<Initializer> Parser::ParseInitializer() {
    auto node = std::make_shared<Initializer>(Lex.CurrentToken);
    /// Start View
    if (Lex.Match(TokenKind::LBrace)) {
        node->LBrace = true;
        Lex.SkipToken(TokenKind::LBrace);
        node->Initials = ParseInitializer();
        auto tail = node->Initials;
        /// Inner View
        while (Lex.Match(TokenKind::Comma)) {
            Lex.SkipToken(TokenKind::Comma);
            if (Lex.Match(TokenKind::RBrace)) {
                break;
            }
            tail->Next = ParseInitializer();
            tail = tail->Next;
        }
        Lex.ExpectToken(TokenKind::RBrace);
    } else {
        node->LBrace = false;
        node->Expr = ParseAssignExpr();
    }
    return node;
}

/**
 * Parse Struct Or Union Specifier
 * struct-or-union-specifier:
 *      struct-or-union identifier? struct-declaration-list ;
 *      struct-or-union identifier
 *
 * struct-or-union:
 *      struct union
 *
 * struct-declaration-list:
 *      struct-declaration
 *      struct-declaration-list struct-declaration
 * @return StructSpecifier AST
 */
std::shared_ptr<StructSpecifier> Parser::ParseStrucOrUnionSpecifier() {
    DeclClass cls = DeclClass::StructSpecifier;
    if (Lex.Match(TokenKind::Union)) {
        cls = DeclClass::UnionSpecifier;
    }
    auto node = std::make_shared<StructSpecifier>(Lex.CurrentToken);
    node->Cls = cls;
    Lex.GetNextToken();

    if (Lex.Match(TokenKind::Id)) {
        node->Id = Lex.CurrentToken->Content;
        Lex.SkipToken(TokenKind::Id);
    }

    if (!Lex.Match(TokenKind::LBrace)) {
        return node;
    }

    node->HasLBrace = true;
    Lex.ExpectToken(TokenKind::LBrace);
    while (!Lex.Match(TokenKind::RBrace)) {
        node->DeclList.push_back(ParseStructDeclaration());
    }
    Lex.ExpectToken(TokenKind::RBrace);
    return node;
}

/**
 * Parse Struct Declaration
 * struct-declaration:
 *      specifier-qualifier-list struct-declarator-list ;
 *
 * specifier-qualifier-list:
 *      type-specifier specifier-qualifier-list?
 *      type-qualifier specifier-qualifier-list?
 *
 * struct-declarator-list:
 *      struct-declarator
 *      struct-declarator-list, struct-declarator
 * @return StructDeclaration AST
 */
std::shared_ptr<StructDeclaration> Parser::ParseStructDeclaration() {
    auto node = std::make_shared<StructDeclaration>(Lex.CurrentToken);
    node->Spec = ParseDeclSpecifier();
    node->DecList.push_back(ParseStrucDeclarator());
    while (Lex.Match(TokenKind::Comma)) {
        Lex.SkipToken(TokenKind::Comma);
        node->DecList.push_back(ParseStrucDeclarator());
    }
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

/**
 * Parse Struct
 * struct-declarator
 *      declarator
 *      declarator? : constant-expression
 * @return StructDeclarator AST
 */
std::shared_ptr<StructDeclarator> Parser::ParseStrucDeclarator() {
    auto node = std::make_shared<StructDeclarator>(Lex.CurrentToken);
    node->Dec = ParseDeclarator();
    return node;
}

/**
 * Parse Enum Specifier
 * enum-specifier:
 *      enum identifier? { enumerator-list }
 *      enum identifier? { enumerator-list , }
 *      enum identifier
 *
 * enumerator-list:
 *      enumerator
 *      enumerator-list , enumerator
 * @return EnumSpecifier AST
 */
std::shared_ptr<EnumSpecifier> Parser::ParseEnumSpecifier() {
    auto node = std::make_shared<EnumSpecifier>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Enum);

    if (Lex.Match(TokenKind::Id)) {
        Lex.SkipToken(TokenKind::Id);
        node->Id = Lex.CurrentToken->Content;
    }
    if (!Lex.Match(TokenKind::LBrace));
    node->DecList.push_back(ParseEnumDeclarator());
    while (Lex.Match(TokenKind::Comma)) {
        Lex.SkipToken(TokenKind::Comma);
        if (Lex.Match(TokenKind::RBrace)) {
            break;
        }
        node->DecList.push_back(ParseEnumDeclarator());
    }
    Lex.ExpectToken(TokenKind::RBrace);
    return node;
}

/**
 * Parse Enum Declarator
 * enum-declarator:
 *      identifier
 * @return EnumDeclarator AST
 */
std::shared_ptr<EnumDeclarator> Parser::ParseEnumDeclarator() {
    auto node = std::make_shared<EnumDeclarator>(Lex.CurrentToken);
    if (!Lex.Match(TokenKind::Id)) {
        ParseDiag(Lex.CurrentToken->Location, "expected a identifier");
    }
    node->Id = Lex.CurrentToken->Content;
    return node;
}

/**
 * expression:
 *      assignment-expression
 *      expression, assignment-expression
 * @return
 */
std::shared_ptr<ExprNode> Parser::ParseExpr() {
    return ParseAssignExpr();
}

/**
 * Parse Constant Expression
 * constant-expression:
 *      conditional-expression
 * @return ConstantExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseConstantExpression() {
    return ParseAssignExpr();
}

/**
 * Parse Conditional Expression
 * conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 * @return ConditionalExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseConditionalExpression() {
    return nullptr;
}

/**
 * Parse Assign Expression
 * assignment-expression:
 *      conditional-expression
 *      unary-expression assignment-operator assignment-expression
 *
 * assignment-operator: one of
 *      = *= /= %= += -= <<= >>= &= ^= |=
 * @return AssignExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseAssignExpr() {
    auto left = ParseEqualExpr();

    if (Lex.Match(TokenKind::Assign)) {
        auto node = std::make_shared<AssignExpr>(Lex.CurrentToken);
        Lex.SkipToken(TokenKind::Assign);
        node->Lhs = left;
        node->Rhs = ParseAssignExpr();
        return node;
    }
    return left;
}

/**
 * Parse Equal Expression
 * equality-expression:
 *      relation-expression
 *      equality-expression == relational-expression
 *      equality-expression != relation-expression
 * @return EqualExpr AST
 */
std::shared_ptr<ExprNode> Parser::ParseEqualExpr() {
    auto left = ParseRelationalExpr();
    while (Lex.Match(TokenKind::Equal) || Lex.Match(TokenKind::PipeEqual)) {
        BinaryOperator op = BinaryOperator::Equal;
        if (Lex.Match(TokenKind::PipeEqual))
            op = BinaryOperator::PipeEqual;
        auto node = std::make_shared<BinaryExpr>(op, Lex.CurrentToken);
        Lex.GetNextToken();
        node->Lhs = left;
        node->Rhs = ParseRelationalExpr();
        left = node;
    }
    return left;
}

/**
 * Parse Relation Expression
 * relation-expression:
 *      shift-expression
 *      relation-expression < shift-expression
 *      relation-expression > shift-expression
 *      relation-expression <= shift-expression
 *      relation-expression >= shift-expression
 * @return RelationExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseRelationalExpr() {
    auto left = ParseAddExpr();
    while (Lex.Match(TokenKind::Greater) ||
        Lex.Match(TokenKind::GreaterEqual) ||
        Lex.Match(TokenKind::Lesser) ||
        Lex.Match(TokenKind::LesserEqual)) {
        BinaryOperator op = BinaryOperator::Greater;
        if (Lex.Match(TokenKind::GreaterEqual))
            op = BinaryOperator::GreaterEqual;
        else if (Lex.Match(TokenKind::Lesser))
            op = BinaryOperator::Lesser;
        else if (Lex.Match(TokenKind::LesserEqual))
            op = BinaryOperator::LesserEqual;

        auto node = std::make_shared<BinaryExpr>(op, Lex.CurrentToken);
        Lex.GetNextToken();
        node->Lhs = left;
        node->Rhs = ParseAddExpr();
        left = node;
    }
    return left;
}

/**
 * Parse Additive Expression
 * additive-expression:
 *      multiplicative-expression
 *      additive-expression + multiplication-expression
 *      additive-expression - multiplication-expression
 * @return AddExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseAddExpr() {
    std::shared_ptr<ExprNode> left = ParseMultiExpr();
    while (Lex.Match(TokenKind::Plus) || Lex.Match(TokenKind::Minus)) {
        BinaryOperator bop = BinaryOperator::Add;
        if (Lex.Match(TokenKind::Minus)) {
            bop = BinaryOperator::Sub;
        }
        auto node = std::make_shared<BinaryExpr>(bop, Lex.CurrentToken);
        Lex.GetNextToken();
        node->Lhs = left;
        node->Rhs = ParseMultiExpr();
        left = node;
    }
    return left;
}

/**
 * Parse MultiExpression
 * multiplicative-expression:
 *      cast-expression
 *      multiplicative-expression * cast-expression
 *      multiplicative-expression / cast-expression
 *      multiplicative-expression % cast-expression
 * @return MultiExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseMultiExpr() {
    std::shared_ptr<ExprNode> left = ParseUnaryExpr();
    while (Lex.Match(TokenKind::Star) || Lex.Match(TokenKind::Slash)) {
        BinaryOperator anOperator = BinaryOperator::Mul;
        if (Lex.Match(TokenKind::Slash))
            anOperator = BinaryOperator::Div;
        auto node = std::make_shared<BinaryExpr>(anOperator, Lex.CurrentToken);
        Lex.GetNextToken();
        node->Lhs = left;
        node->Rhs = ParseUnaryExpr();
        left = node;
    }
    return left;
}

/**
 * Parse Unary Expression
 * unary-expression:
 *      postfix-expression
 *      ++ unary-expression
 *      -- unary-expression
 *      unary-operator cast-expression
 *      sizeof unary-expression
 *      sizeof ( type-name )
 *
 * unary-operator:
 *      one of & * + - ~ !
 * @return UnaryExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParseUnaryExpr() {
    switch (Lex.CurrentToken->Kind) {
        case TokenKind::Plus: {
            auto node = std::make_shared<UnaryExpr>(UnaryOperator::Plus, Lex.CurrentToken);
            Lex.GetNextToken();
            node->Lhs = ParseUnaryExpr();
            return node;
        }
        case TokenKind::Minus: {
            auto node = std::make_shared<UnaryExpr>(UnaryOperator::Star, Lex.CurrentToken);
            Lex.GetNextToken();
            node->Lhs = ParseUnaryExpr();
            return node;
        }
        case TokenKind::SizeOf: {
            return ParseSizeofExpr();
        }
        default:
            return ParsePostFixExpr();
    }
}

/**
 * Parse Post Fix Expression
 * postfix-expression:
 *      primary-expression
 *      postfix-expression [ expression ]
 *      postfix-expression ( argument-expression-list? )
 *      postfix-expression . identifier
 *      postfix-expression -> identifier
 *      postfix-expression ++
 *      postfix-expression --
 *      ( type-name ) { initializer-list }
 *      ( type-name ) { initializer-list , }
 * @return PostFixExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParsePostFixExpr() {
    auto expr = ParsePrimaryExpr();
    while (true) {
        switch (Lex.CurrentToken->Kind) {
            case TokenKind::LBracket: {
                auto starNode = std::make_shared<UnaryExpr>(UnaryOperator::Star, expr->Tok);
                auto addNode = std::make_shared<BinaryExpr>(BinaryOperator::Add, expr->Tok);
                addNode->Lhs = expr;
                Lex.SkipToken(TokenKind::LBracket);
                addNode->Rhs = ParseExpr();
                Lex.SkipToken(TokenKind::RBracket);
                starNode->Lhs = addNode;

                expr = starNode;
                break;
            }
            case TokenKind::LParen: {
                expr = ParseFuncCallNode(expr);
                break;
            }
            case TokenKind::Period: {
                expr = ParseMemberNode(expr);
                break;
            }
            case TokenKind::Arrow: {
                auto starNode = std::make_shared<UnaryExpr>(UnaryOperator::Star, expr->Tok);
                starNode->Lhs = expr;
                expr = ParseMemberNode(starNode);
                break;
            }
            default:
                return expr;
        }
    }
}

/**
 * Parse Function Call Expression
 * @param expr
 * @return
 */
std::shared_ptr<ExprNode> Parser::ParseFuncCallNode(std::shared_ptr<ExprNode> expr) {
    Lex.SkipToken(TokenKind::LParen);
    auto node = std::make_shared<FuncCallExpr>(expr->Tok);
    node->FuncName = expr->Tok->Content;
    if (!Lex.Match(TokenKind::RParen)) {
        node->Args.push_back(ParseAssignExpr());
        while (Lex.Match(TokenKind::Comma)) {
            Lex.SkipToken(TokenKind::Comma);
            node->Args.push_back(ParseAssignExpr());
        }
    }
    Lex.ExpectToken(TokenKind::RParen);
    return node;
}

std::shared_ptr<ExprNode> Parser::ParseMemberNode(std::shared_ptr<ExprNode> expr) {
    auto node = std::make_shared<MemberExpr>(Lex.CurrentToken);
    Lex.GetNextToken();
    node->Lhs = expr;
    if (!Lex.Match(TokenKind::Id)) {
        ParseDiag(Lex.CurrentToken->Location, "expected a identifier as Struct or Union member");
    }
    node->RhsName = Lex.CurrentToken->Content;
    Lex.SkipToken(TokenKind::Id);
    return node;
}

/**
 * Parse Primary Expression
 * primary-expression:
 *      identifier
 *      constant
 *      string-literal
 *      ( expression )
 *      ({})
 * @return PrimaryExpression AST
 */
std::shared_ptr<ExprNode> Parser::ParsePrimaryExpr() {
    if (Lex.Match(TokenKind::LParen)) {
        Lex.BeginPeekToken();
        Lex.GetNextToken();
        if (Lex.Match(TokenKind::LBrace)) {
            Lex.EndPeekToken();
            return ParseStmtExpr();
        }
        Lex.EndPeekToken();

        Lex.GetNextToken();
        auto node = ParseExpr();
        Lex.ExpectToken(TokenKind::RParen);
        return node;
    } else if (Lex.Match(TokenKind::Num)) {
        return ParseNumExpr();
    } else if (Lex.Match(TokenKind::Id)) {
        return ParseVarExpr();
    } else {
        ParseDiag(Lex.CurrentToken->Location, "error expr or not supported");
    }
    return nullptr;
}

std::shared_ptr<ExprNode> Parser::ParseStmtExpr() {
    auto node = std::make_shared<StmtExpr>(Lex.CurrentToken);

    Lex.SkipToken(TokenKind::LParen);
    Lex.SkipToken(TokenKind::LBrace);

    while (!Lex.Match(TokenKind::RBrace)) {
        if (FirstDeclarationSet.find(Lex.CurrentToken->Kind) != FirstDeclarationSet.end()) {
            node->Decls.push_back(ParseDeclaration());
        } else {
            node->Stmts.push_back(ParseStmt());
        }
    }

    Lex.SkipToken(TokenKind::RBrace);
    Lex.ExpectToken(TokenKind::RParen);
    return node;
}

std::shared_ptr<ExprNode> Parser::ParseNumExpr() {
    auto node = std::make_shared<NumExpr>(Lex.CurrentToken);
    node->Value = Lex.CurrentToken->Value;
    node->Ty = Type::LongTy;
    Lex.GetNextToken();
    return node;
}

std::shared_ptr<ExprNode> Parser::ParseVarExpr() {
    auto node = std::make_shared<VarExpr>(Lex.CurrentToken);
    node->VarName = Lex.CurrentToken->Content;
    Lex.GetNextToken();
    return node;
}

std::shared_ptr<ExprNode> Parser::ParseSizeofExpr() {
    auto node = std::make_shared<SizeOfExpr>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::SizeOf);
    node->Lhs = ParseUnaryExpr();
    return node;
}

std::shared_ptr<StmtNode> Parser::ParseStmt() {
    if (Lex.Match(TokenKind::If)) {
        return ParseIfStmt();
    } else if (Lex.Match(TokenKind::While)) {
        return ParseWhileStmt();
    } else if (Lex.Match(TokenKind::Do)) {
        return ParseDoWhileStmt();
    } else if (Lex.Match(TokenKind::For)) {
        return ParseForStmt();
    } else if (Lex.Match(TokenKind::LBrace)) {
        return ParseBlockStmt();
    } else if (Lex.Match(TokenKind::Break)) {
        return ParseBreakStmt();
    } else if (Lex.Match(TokenKind::Continue)) {
        return ParseContinueStmt();
    } else if (Lex.Match(TokenKind::Goto)) {
        return ParseGotoStmt();
    } else if (Lex.Match(TokenKind::Case)) {
        return ParseCaseStmt();
    } else if (Lex.Match(TokenKind::Default)) {
        return ParseDefaultStmt();
    } else if (Lex.Match(TokenKind::Switch)) {
        return ParseSwitchStmt();
    } else if (Lex.Match(TokenKind::Id)) {
        return ParseLabelStmt();
    } else {
        return ParseExprStmt();
    }
}

std::shared_ptr<IfStmtNode> Parser::ParseIfStmt() {
    auto node = std::make_shared<IfStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::If);
    Lex.ExpectToken(TokenKind::LParen);
    node->Cond = ParseExpr();
    Lex.ExpectToken(TokenKind::RParen);
    node->Then = ParseStmt();
    if (Lex.Match(TokenKind::Else)) {
        Lex.SkipToken(TokenKind::Else);
        node->Else = ParseStmt();
    }
    return node;
}

std::shared_ptr<DoWhileStmtNode> Parser::ParseDoWhileStmt() {
    auto node = std::make_shared<DoWhileStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Do);
    node->Stmt = ParseStmt();
    Lex.ExpectToken(TokenKind::While);
    Lex.ExpectToken(TokenKind::LParen);
    node->Cond = ParseExpr();
    Lex.ExpectToken(TokenKind::RParen);
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<WhileStmtNode> Parser::ParseWhileStmt() {
    auto node = std::make_shared<WhileStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::While);
    Lex.ExpectToken(TokenKind::LParen);
    node->Cond = ParseExpr();
    Lex.ExpectToken(TokenKind::RParen);
    node->Then = ParseStmt();
    return node;
}

std::shared_ptr<ForStmtNode> Parser::ParseForStmt() {
    auto node = std::make_shared<ForStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::For);
    Lex.ExpectToken(TokenKind::LParen);
    if (!Lex.Match(TokenKind::Semicolon)) {
        if (FirstDeclarationSet.find(Lex.CurrentToken->Kind) != FirstDeclarationSet.end()) {
            node->InitDecl = ParseDeclaration();
        } else {
            node->InitExpr = ParseExpr();
            Lex.ExpectToken(TokenKind::Semicolon);
        }
    } else {
        Lex.ExpectToken(TokenKind::Semicolon);
    }
    if (!Lex.Match(TokenKind::Semicolon))
        node->Cond = ParseExpr();
    Lex.ExpectToken(TokenKind::Semicolon);
    if (!Lex.Match(TokenKind::RParen))
        node->Inc = ParseExpr();
    Lex.ExpectToken(TokenKind::RParen);
    node->Stmt = ParseStmt();
    return node;
}

std::shared_ptr<BlockStmtNode> Parser::ParseBlockStmt() {
    auto node = std::make_shared<BlockStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::LBrace);
    while (!Lex.Match(TokenKind::RBrace)) {
        if (FirstDeclarationSet.find(Lex.CurrentToken->Kind) != FirstDeclarationSet.end()) {
            auto decl = ParseDeclaration();
            decl->IsLocalDecl = true;
            node->Decls.push_back(decl);
        } else {
            node->Stmts.push_back(ParseStmt());
        }
    }
    Lex.ExpectToken(TokenKind::RBrace);
    return node;
}

std::shared_ptr<ReturnStmtNode> Parser::ParseReturnStmt() {
    auto node = std::make_shared<ReturnStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Return);
    node->Lhs = ParseExpr();
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<ExprStmtNode> Parser::ParseExprStmt() {
    auto node = std::make_shared<ExprStmtNode>(Lex.CurrentToken);
    if (!Lex.Match(TokenKind::Semicolon)) {
        node->Lhs = ParseExpr();
    }
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<BreakStmtNode> Parser::ParseBreakStmt() {
    auto node = std::make_shared<BreakStmtNode>(Lex.CurrentToken);
    Lex.ExpectToken(TokenKind::Break);
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<ContinueStmtNode> Parser::ParseContinueStmt() {
    auto node = std::make_shared<ContinueStmtNode>(Lex.CurrentToken);
    Lex.ExpectToken(TokenKind::Continue);
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<GotoStmtNode> Parser::ParseGotoStmt() {
    auto node = std::make_shared<GotoStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Goto);
    if (!Lex.Match(TokenKind::Id)) {
        ParseDiag(node->Tok->Location, "expected a identifier");
    }
    node->LabelName = Lex.CurrentToken->Content;
    Lex.SkipToken(TokenKind::Id);
    Lex.ExpectToken(TokenKind::Semicolon);
    return node;
}

std::shared_ptr<StmtNode> Parser::ParseLabelStmt() {
    Lex.BeginPeekToken();
    Lex.GetNextToken();
    if (Lex.Match(TokenKind::Colon)) {
        Lex.EndPeekToken();
        auto node = std::make_shared<LabelStmtNode>(Lex.CurrentToken);
        node->LabelName = Lex.CurrentToken->Content;
        Lex.SkipToken(TokenKind::Id);
        Lex.SkipToken(TokenKind::Colon);
        node->Stmt = ParseStmt();
        return node;
    }
    Lex.EndPeekToken();
    return ParseExprStmt();
}

std::shared_ptr<CaseStmtNode> Parser::ParseCaseStmt() {
    auto node = std::make_shared<CaseStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Case);
    node->Expr = ParseExpr();
    node->Stmt = ParseStmt();
    return node;
}

std::shared_ptr<DefaultStmtNode> Parser::ParseDefaultStmt() {
    auto node = std::make_shared<DefaultStmtNode>(Lex.CurrentToken);
    Lex.ExpectToken(TokenKind::Default);
    Lex.ExpectToken(TokenKind::Colon);
    node->Stmt = ParseStmt();
    return node;
}

std::shared_ptr<SwitchStmtNode> Parser::ParseSwitchStmt() {
    auto node = std::make_shared<SwitchStmtNode>(Lex.CurrentToken);
    Lex.SkipToken(TokenKind::Switch);
    Lex.ExpectToken(TokenKind::LParen);
    node->Expr = ParseExpr();
    Lex.ExpectToken(TokenKind::RParen);
    node->Stmt = ParseStmt();
    return node;
}

bool Parser::IsFunctionDeclaration(std::shared_ptr<Declarator> dec, std::string_view &funcName) {
    while (dec) {
        if (dec->Cls == DeclClass::FunctionDeclarator &&
        dec->Dec && dec->Dec->Cls == DeclClass::NameDeclarator) {
            funcName = dec->Dec->Id;
            return true;
        }
        dec = dec->Dec;
    }
    return false;
}