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
    } else if (CurChar == '.') {
        if (PeekChar(1) == '.' && PeekChar(2) == '.') {
            GetNextChar();
            GetNextChar();
        }
    }

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
    return false;
}

bool Lexer::IsDigit() {
    return false;
}

bool Lexer::IsLetterOrDigit() {
    return false;
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

}

void Lexer::SkipComment() {

}

SourceLocation Lexer::GetLocation() {
    return SourceLocation();
}

const char *Lexer::GetTokenSimpleSpelling(TokenKind kind) {
    return nullptr;
}

void Lexer::ExpectToken(TokenKind kind) {

}

void Lexer::SkipToken(TokenKind kind) {

}

void Lexer::Match(TokenKind kind) {

}

void Lexer::BeginPeekToken() {

}

void Lexer::EndPeekToken() {

}

