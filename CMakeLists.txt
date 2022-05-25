cmake_minimum_required(VERSION 3.16)
project(C100)

set(CMAKE_CXX_STANDARD 17)

include_directories(AST Lex Parse Sema Gen Basic)

file(GLOB_RECURSE SRCS main.cpp
        Gen/*.cpp
        Lex/*.cpp
        Parse/*.cpp
        Basic/*.cpp
        AST/*.cpp
)

add_executable(${PROJECT_NAME} ${SRCS})
