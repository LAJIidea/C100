//
// Created by BY210033 on 2022/5/23.
//

#include "Lexer.h"
#include "Diag.h"
#include <cstring>
#include <cassert>
#include <string>
#include "ReserveWord.cpp"

using namespace C100;

void Lexer::GetNextToken() {
    // 1 skip white space
    SkipWhiteSpace();

    SourceLocation location;
    location.Line = Line;
    location.Col = Cursor - 1 - LineHead;
    location.FilePath = CurrentFilePath;
    location.Code = SourceCode;

    TokenKind kind;
    int value = 0;
    int startPos = Cursor - 1;
    std::string_view content = "";
    if (CurChar == '\0')
    {
        kind = TokenKind::Eof;
    } else if (CurChar == '+')
    {
        kind = TokenKind::Plus;
        GetNextChar();
    } else if (CurChar == '-') {
        if (PeekChar(1) == '>') {
            GetNextChar();
            kind = TokenKind::Arrow;
        } else {
            kind = TokenKind::Minus;
        }
        GetNextChar();
    } else if (CurChar == '*') {
        kind = TokenKind::Star;
        GetNextChar();
    } else if (CurChar == '/') {
        kind = TokenKind::Slash;
        GetNextChar();
    } else if (CurChar == '&') {
        kind = TokenKind::Amp;
        GetNextChar();
    } else if (CurChar == '(') {
        kind = TokenKind::LParen;
        GetNextChar();
    } else if (CurChar == ')') {
        kind = TokenKind::RParen;
        GetNextChar();
    } else if (CurChar == '[') {
        kind = TokenKind::LBracket;
        GetNextChar();
    } else if (CurChar == ']') {
        kind = TokenKind::RBracket;
        GetNextChar();
    } else if (CurChar == '{') {
        kind = TokenKind::LBrace;
        GetNextChar();
    } else if (CurChar == ';') {
        kind = TokenKind::Semicolon;
        GetNextChar();
    } else if (CurChar == ':') {
        kind = TokenKind::Colon;
        GetNextChar();
    } else if (CurChar == ',') {
        kind = TokenKind::Comma;
        GetNextChar();
    } else if (CurChar == '.') {
        if (PeekChar(1) == '.' && PeekChar(2) == '.') {
            GetNextChar();
            GetNextChar();
            kind = TokenKind::Ellipsis;
        } else {
            kind = TokenKind::Period;
        }
        GetNextChar();
    } else if (CurChar == '=') {
        if (PeekChar(1) == '=') {
            GetNextChar();
            kind = TokenKind::Equal;
        } else {
            kind = TokenKind::Assign;
        }
        GetNextChar();
    } else if (CurChar == '!') {
        if (PeekChar(1) == '=') {
            GetNextChar();
            kind = TokenKind::PipeEqual;
        } else {
            kind = TokenKind::Unknown;
            LexDiag(location, "current '%s' is illegal", CurChar);
        }
        GetNextChar();
    } else if (CurChar == '>') {
        if (PeekChar(1) == '=') {
            GetNextChar();
            kind = TokenKind::GreaterEqual;
        } else {
            kind = TokenKind::Greater;
        }
        GetNextChar();
    } else if (CurChar == '<') {
        if (PeekChar(1) == '=') {
            GetNextChar();
            kind = TokenKind::LesserEqual;
        } else {
            kind = TokenKind::Lesser;
        }
        GetNextChar();
    } else if (isdigit(CurChar)) {
        kind = TokenKind::Num;
        value = 0;
        do {
            value = value * 10 + CurChar - '0';
            GetNextChar();
        } while (isdigit(CurChar));
    } else {
        if (IsLetter()) {
            while (IsLetterOrDigit()) {
                GetNextChar();
            }
            kind = TokenKind::Id;
            content = SourceCode.substr(startPos, Cursor-1-startPos);
            if (NameToTokenKindMap.find(content) != NameToTokenKindMap.end()) {
                kind = NameToTokenKindMap[content];
            }
        } else {
            LexDiag(location, "current '%c' is illegal", CurChar);
            kind = TokenKind::Unknown;
        }
    }
    CurrentToken = std::make_shared<Token>();
    CurrentToken->Kind = kind;
    CurrentToken->Value = value;
    CurrentToken->Location = location;
    CurrentToken->Content = content;
}

void Lexer::GetNextChar() {
    if (Cursor == SourceCode.size()) {
        CurChar = '\0';
        Cursor++;
    } else {
        CurChar = SourceCode[Cursor];
    }
}

bool Lexer::IsLetter() {
    return (CurChar >= 'a' && CurChar <= 'z') || (CurChar >= 'A' && CurChar <= 'Z') || (CurChar == '_');
}

bool Lexer::IsDigit() {
    return CurChar >= '0' && CurChar <= '9';
}

bool Lexer::IsLetterOrDigit() {
    return IsLetter() || IsDigit();
}

char Lexer::PeekChar(int distance) {
    assert(distance>=0);
    if (Cursor - 1 + distance < SourceCode.size()) {
        return SourceCode[Cursor-1+distance];
    } else {
        return '\0';
    }
}

void Lexer::SkipWhiteSpace() {
    while (isspace(CurChar) || (CurChar == '/' && PeekChar(1)) || (CurChar == '/' && PeekChar(1) == '*')) {
        if (CurChar == '/') {
            SkipComment();
            continue;
        } else if (CurChar == '\n') {
            Line++;
            LineHead = Cursor;
        }
        GetNextChar();
    }
}

void Lexer::SkipComment() {
    if (CurChar == '/' && PeekChar(1) == '/') {
        while (CurChar != '\n')
            GetNextChar();
    } else {
        auto pos = SourceCode.find("*/", Cursor + 1);
        if (pos == std::string_view::npos) {
            LexDiag(GetLocation(), "unclosed \"*/\"");
            assert(0);
        } else {
            CurChar = PeekChar((pos + 2) - (Cursor - 1));
            Cursor = pos + 3;
        }
    }
}

SourceLocation Lexer::GetLocation() {
    SourceLocation location;
    location.Line = Line;
    location.Col = Cursor-1-LineHead;
    location.LineHead = LineHead;
    location.FilePath = CurrentFilePath;
    location.Code = SourceCode;
    return location;
}

const char *Lexer::GetTokenSimpleSpelling(TokenKind kind) {
    if (kind == TokenKind::Eof)
        return "eof";
    if (TokenKindToNameMap.find(kind) != TokenKindToNameMap.end()) {
        return TokenKindToNameMap[kind].data();
    }
    return nullptr;
}

void Lexer::ExpectToken(TokenKind kind) {
    if (CurrentToken->Kind == kind) {
        GetNextToken();
    } else {
        LexDiag(CurrentToken->Location, "'%s' expected", GetTokenSimpleSpelling(kind));
    }
}

void Lexer::SkipToken(TokenKind kind) {
    if (CurrentToken->Kind == kind) {
        GetNextToken();
    } else {
        LexDiag(CurrentToken->Location, "'%s' skipped", GetTokenSimpleSpelling(kind));
    }
}

bool Lexer::Match(TokenKind kind) {
    return CurrentToken->Kind == kind;
}

void Lexer::BeginPeekToken() {
    PeekPt.Cursor = Cursor;
    PeekPt.CurChar = CurChar;
    PeekPt.Line = Line;
    PeekPt.LineHead = LineHead;
    PeekPt.CurrentToken = CurrentToken;
}

void Lexer::EndPeekToken() {
    CurChar = PeekPt.CurChar;
    Cursor = PeekPt.Cursor;
    Line = PeekPt.Line;
    LineHead = PeekPt.LineHead;
    CurrentToken = PeekPt.CurrentToken;
}

