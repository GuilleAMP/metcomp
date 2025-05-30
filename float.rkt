#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "integer.rkt")

(provide float)

(define-lex-abbrev float
  (:seq integer #\. integer))
