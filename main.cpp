#include "Lexer.h"
#include "Parser.h"
#include "Sema.h"
#include "CodeGen.h"

using namespace C100;

int main(int argc, char *argv[]) {

    if (argc != 2) {
        printf("please input codePath\n");
        return 0;
    }

    Lexer lexer(argv[1]);
    Parser parser(lexer);
    auto translationUnit = parser.ParseTranslationUnit();
    Sema sema;
    translationUnit->Accept(&sema);
    CodeGen codeGen;
    translationUnit->Accept(&codeGen);
    return 0;
}
