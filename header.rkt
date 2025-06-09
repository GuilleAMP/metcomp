#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide header function-object-call)

(define-lex-abbrev function-object-call
  (:seq
   (:* (:or (:/ #\a #\z) (:/ #\A #\Z) (:/ #\0 #\9)))
   "."
   (:+ (:or (:/ #\a #\z) (:/ #\A #\Z)))))

(define-lex-abbrev header
  (:seq #\" function-object-call #\"))



