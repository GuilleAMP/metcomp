#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide data-type)

;; Una sola letra mayúscula (tipo genérico)
(define-lex-abbrev capital-letter (:/ #\A #\Z))

(define-lex-abbrev letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev underscore #\_)
(define-lex-abbrev identifier-rest (:* (:or letter digit underscore)))
(define-lex-abbrev identifier (:seq letter identifier-rest))

;; Identificador que comienza con mayúscula (para getter)
(define-lex-abbrev identifier-capitalized (:seq capital-letter identifier-rest))

(define-lex-abbrev data-type
  (:or "int" "void" "float" "bool" "string" capital-letter))
