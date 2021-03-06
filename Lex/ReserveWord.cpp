//
// Created by BY210033 on 2022/5/23.
//

#include "Lexer.h"

namespace C100 {
    static std::unordered_map<std::string_view, TokenKind> NameToTokenKindMap = {
            {"auto", TokenKind::Auto},
            {"register", TokenKind::Register},
            {"static", TokenKind::Static},
            {"extern", TokenKind::Extern},
            {"typedef", TokenKind::Typedef},
            {"void", TokenKind::Void},
            {"bool", TokenKind::Bool},
            {"char", TokenKind::Char},
            {"short", TokenKind::Short},
            {"int", TokenKind::Int},
            {"long", TokenKind::Long},
            {"float", TokenKind::Float},
            {"double", TokenKind::Double},
            {"signed", TokenKind::Signed},
            {"unsigned", TokenKind::UnSigned},
            {"struct", TokenKind::Struct},
            {"union", TokenKind::Union},
            {"enum", TokenKind::Enum},
            {"const", TokenKind::Const},
            {"restrict", TokenKind::Restrict},
            {"volatile", TokenKind::Volatile},
            {"if", TokenKind::If},
            {"else", TokenKind::Else},
            {"return", TokenKind::Return},
            {"while", TokenKind::While},
            {"do", TokenKind::Do},
            {"for", TokenKind::For},
            {"sizeof", TokenKind::SizeOf},
            {"break", TokenKind::Break},
            {"continue", TokenKind::Continue},
            {"goto", TokenKind::Goto},
            {"switch", TokenKind::Switch},
            {"case", TokenKind::Case},
            {"default", TokenKind::Default},

            {"+", TokenKind::Plus},
            {"-", TokenKind::Minus},
            {"*", TokenKind::Star},
            {"/", TokenKind::Slash},
            {"(", TokenKind::LParen},
            {")", TokenKind::RParen},
            {"[", TokenKind::LBracket},
            {"]", TokenKind::RBracket},
            {"{", TokenKind::LBrace},
            {"}", TokenKind::RBrace},
            {";", TokenKind::Semicolon},
            {"=", TokenKind::Assign},
            {",", TokenKind::Comma},
            {"&", TokenKind::Amp},
            {"==", TokenKind::Equal},
            {"!=", TokenKind::PipeEqual},
            {">", TokenKind::Greater},
            {">=", TokenKind::GreaterEqual},
            {"<", TokenKind::Lesser},
            {"<=", TokenKind::LesserEqual},
            {".", TokenKind::PipeEqual},
            {"->", TokenKind::Arrow},
            {"...", TokenKind::Ellipsis},
            {":", TokenKind::Colon}
    };

    static std::unordered_map<TokenKind, std::string_view> TokenKindToNameMap = {
            {TokenKind::Auto,     "auto"},
            {TokenKind::Register, "register"},
            {TokenKind::Static,   "static"},
            {TokenKind::Extern,   "extern"},
            {TokenKind::Typedef,  "typedef"},
            {TokenKind::Void,     "void"},
            {TokenKind::Bool,     "_Bool"},
            {TokenKind::Char,     "char"},
            {TokenKind::Short,    "short"},
            {TokenKind::Int,      "int"},
            {TokenKind::Long,     "long"},
            {TokenKind::Float,    "float"},
            {TokenKind::Double,   "double"},
            {TokenKind::Signed,   "signed"},
            {TokenKind::UnSigned, "unsigned"},
            {TokenKind::Struct,   "struct"},
            {TokenKind::Union,    "union"},
            {TokenKind::Enum,     "enum"},
            {TokenKind::Const,    "const"},
            {TokenKind::Restrict, "restrict"},
            {TokenKind::Volatile, "volatile"},
            {TokenKind::If,       "if"},
            {TokenKind::Else,     "else"},
            {TokenKind::Return,   "return"},
            {TokenKind::While,    "while"},
            {TokenKind::Do,       "do"},
            {TokenKind::For,      "for"},
            {TokenKind::SizeOf,   "sizeof"},
            {TokenKind::Break,   "break"},
            {TokenKind::Continue,   "continue"},
            {TokenKind::Goto     ,"goto"  },
            {TokenKind::Switch   ,"switch" },
            {TokenKind::Case     ,"case" },
            { TokenKind::Default ,"default"},

            {TokenKind::Plus,     "+"},
            {TokenKind::Minus,    "-"},
            {TokenKind::Star,     "*"},
            {TokenKind::Slash,    "/"},
            {TokenKind::LParen,   "("},
            {TokenKind::RParen,   ")"},
            {TokenKind::LBracket, "["},
            {TokenKind::RBracket, "]"},
            {TokenKind::LBrace,   "{"},
            {TokenKind::RBrace,   "}"},
            {TokenKind::Semicolon, ";"},
            {TokenKind::Assign,   "="},
            {TokenKind::Comma,    ","},
            {TokenKind::Amp,      "&"},
            {TokenKind::Equal,    "=="},
            {TokenKind::PipeEqual, "!="},
            {TokenKind::Greater,   ">"},
            {TokenKind::GreaterEqual, ">="},
            {TokenKind::Lesser, " <"},
            {TokenKind::LesserEqual, "<="},
            {TokenKind::Period, "."},
            {TokenKind::Arrow, "->"},
            {TokenKind::Ellipsis, "..."} ,
            {TokenKind::Colon, ":"}
    };
}