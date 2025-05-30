#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide data-type)

(define-lex-abbrev data-type
  (:or "int" "void" "float" "bool" "string"))
