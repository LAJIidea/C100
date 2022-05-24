//
// Created by BY210033 on 2022/5/23.
//

#include "Diag.h"
#include <cstdarg>
#include <iostream>
#include <string>

namespace C100 {

    std::string PhaseToString(DiagPhase diagPhase)
    {
        switch (diagPhase) {
            case DiagPhase::LexPhase:
                return "LexPhase";
            case DiagPhase::ParsePhase:
                return "ParsePhase";
            case DiagPhase::CodeGenPhase:
                return "CodeGenPhase";
            case DiagPhase::SemaPhase:
                return "SemanticsPhase";
        }
    }

    void DiagLoc(DiagPhase diagPhase, SourceLocation loc, const char *fmt, ...) {
        va_list ap;
        va_start(ap, fmt);
        int len = fprintf(stderr, "%s:%s:%d:%d", loc.FilePath, PhaseToString(diagPhase).data(), loc.Line+1, loc.Col);
        int i = loc.LineHead;
        while (loc.Code[i] != '\n') {
            fprintf(stderr, "%c", loc.Code[i]);
            i++;
        }
        fprintf(stderr, "\n");
        fprintf(stderr, "%*s", len + loc.Col, "");
        fprintf(stderr, "^");
        fprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
        va_end(ap);
        exit(0);
    }

}