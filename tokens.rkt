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
                             HEADER
                             BOOLEAN
                             ))
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
                                DEFAULT
                                BRACE-OPEN
                                BRACE-CLOSE
                                ELSE
                                IF
                                SCOPE-RESOLUTION
                                HASH
                                INCLUDE
                                QUOTE
                                USING
                                DOT
                                H
                                HH
                                HPP
                                EOF))
