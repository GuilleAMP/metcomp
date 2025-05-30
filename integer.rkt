#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide integer)

(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev integer (:+ digit))
