//
// Created by BY210033 on 2022/5/23.
//

#ifndef C100_LEXER_H
#define C100_LEXER_H
#include <string_view>
#include <memory>
#include <cstdio>
#include <cassert>
#include <unordered_map>

namespace C100 {
    enum class TokenKind
    {   Id,

        If, Else, While, Do, For, Return,
        Auto, Static, Extern, Typedef, Register,
        Void, Bool, Char, Short, Int, Long,
        Float, Double, Signed, UnSigned,
        Const, Restrict, Volatile,
        SizeOf, Struct, Union, Enum, Break, Continue,
        Goto, Switch, Case, Default,

        Plus, Minus, Star, Slash, LParen,
        RParen, LBracket, RBracket, LBrace, RBrace,
        Semicolon, Assign, Comma, Amp, Equal,
        PipeEqual, Greater, GreaterEqual, Lesser,
        LesserEqual, Period, Arrow, Ellipsis, LongLong, Colon,

        Num,
        Eof
    };

    struct SourceLocation
    {
        const char *FilePath;
        std::string_view Code;
        int LineHead;
        int Line;
        int Col;
    };

    class Token
    {
    public:
        TokenKind Kind;
        int Value;
        SourceLocation Location;
        std::string_view Content;
    };

    class PeekPoint
    {
    public:
        char CurChar;
        int Cursor;
        int Line;
        int LineHead;
        std::shared_ptr<Token> CurrentToken;
    };

    class Lexer {
    private:
        char CurChar{' '};
        int Cursor{0};
        int Line{0};
        int LineHead{0};
        PeekPoint PeekPt;
        std::string_view SourceCode;
        char *CodeBUff{nullptr};
        const char *CurrentFilePath{nullptr};

    public:
        std::shared_ptr<Token> CurrentToken;

    private:
        bool IsLetter();

        bool IsDigit();

        bool IsLetterOrDigit();

        char PeekChar(int distance);

        void SkipWhiteSpace();

        void SkipComment();

        SourceLocation GetLocation();

        const char *GetTokenSimpleSpelling(TokenKind kind);

    public:
        Lexer(const char *filePath) {
            CurrentFilePath = filePath;
            FILE *fp = fopen(filePath, "r");
            if (fp) {
                fseek(fp, 0, SEEK_END);
                long fileSize = ftell(fp);
                CodeBUff = (char *) malloc(fileSize + 1);
                CodeBUff[fileSize] = '\0';
                fseek(fp, 0, SEEK_SET);
                fread(CodeBUff, fileSize, 1, fp);
                fclose(fp);
            } else {
                printf("%s open failed\n", filePath);
                assert(0);
            }
            SourceCode = CodeBUff;
        }

        void GetNextToken();

        void GetNextChar();

        void ExpectToken(TokenKind kind);

        void SkipToken(TokenKind kind);

        void Match(TokenKind kind);

        void BeginPeekToken();

        void EndPeekToken();
    };
}

#endif //C100_LEXER_H
