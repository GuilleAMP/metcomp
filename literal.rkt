#lang racket
(require parser-tools/lex
         "integer.rkt"
         "float.rkt"
         "string-char.rkt"
         "boolean.rkt")

(provide literal)

(define-lex-abbrev literal
  (union integer float string char boolean))
