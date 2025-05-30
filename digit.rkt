#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide digit)

(define-lex-abbrev digit (:/ #\0 #\9))
