#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide data-type)

;; Una sola letra mayúscula (tipo genérico)
(define-lex-abbrev capital-letter (:/ #\A #\Z))

(define-lex-abbrev data-type
  (:or "int" "void" "float" "bool" "string" capital-letter))
