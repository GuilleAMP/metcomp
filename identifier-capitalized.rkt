#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide identifier-capitalized getter-name setter-name)

;; Definiciones léxicas
(define-lex-abbrev letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
(define-lex-abbrev capital-letter (:/ #\A #\Z))
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev underscore #\_)
(define-lex-abbrev identifier-rest (:* (:or letter digit underscore)))
(define-lex-abbrev identifier (:seq letter identifier-rest))

;; Identificador que comienza con mayúscula (para getter)
(define-lex-abbrev identifier-capitalized (:seq capital-letter identifier-rest))

;; Getter-name: "get" seguido de identificador con mayúscula
(define-lex-abbrev getter-name (:seq "get" identifier-capitalized))

;; Setter name
(define-lex-abbrev setter-name (:seq "set" identifier-capitalized))
