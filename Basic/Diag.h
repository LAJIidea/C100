//
// Created by BY210033 on 2022/5/23.
//

#ifndef C100_DIAG_H
#define C100_DIAG_H

#include "Lexer.h"
#include <string_view>

namespace C100 {

#define LexDiag(loc, fmt, ...)      DiagLoc(DiagPhase::LexPhase, loc, fmt, ##__VA_ARGS__);
#define ParseDiag(loc, fmt, ...)    DiagLoc(DiagPhase::ParsePhase, loc, fmt, ##__VA_ARGS__);
#define SemaDiag(loc, fmt, ...)     DiagLoc(DiagPhase::SemaPhase, loc, fmt, ##__VA_ARGS__);
#define GenDiag(loc, fmt, ...)      DiagLoc(DiagPhase::CodeGenPhase, loc, fmt, ##__VA_ARGS__);

    enum class DiagPhase {
        LexPhase,
        ParsePhase,
        SemaPhase,
        CodeGenPhase
    };

    void DiagLoc(DiagPhase diagPhase, SourceLocation loc, const char*fmt, ...);

}

#endif //C100_DIAG_H
