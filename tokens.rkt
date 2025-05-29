#lang racket
(require parser-tools/lex)

(provide (all-defined-out))

(define-tokens value-tokens (IDENTIFIER
                             NUMBER
                             DATA-TYPE
                             ACCESS-SPECIFIER
                             LITERAL
                             INTEGER
                             FLOAT
                             STRING
                             CHAR
                             BOOLEAN
                             USING))

(define-empty-tokens op-tokens (ASSIGN
                                TERMINATOR
                                PLUS
                                MINUS
                                MUL
                                DIV
                                COLON
                                CIN
                                COUT
                                COMMA
                                SHIFT-R
                                SHIFT-L
                                LPAREN
                                RPAREN
                                RETURN
                                OR
                                AND
                                NOT
                                BREAK
                                SCOPE-RESOLUTION
                                HASH
                                INCLUDE
                                QUOTE
                                DOT
                                H
                                HH
                                HPP
                                EOF))
