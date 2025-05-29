#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide identifier)

(define-lex-abbrev letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev underscore #\_)
(define-lex-abbrev identifier-rest (:* (:or letter digit underscore)))
(define-lex-abbrev identifier (:seq letter identifier-rest))
