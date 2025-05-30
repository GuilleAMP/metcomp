#lang racket
(require parser-tools/lex)

(provide boolean)

(define-lex-abbrev boolean (union "true" "false"))
