#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide header)

(define-lex-abbrev header-name-core
  (:seq (:* (:or (:/ #\a #\z) (:/ #\A #\Z) (:/ #\0 #\9))) "." (:or "h" "hpp" "hh")))


(define-lex-abbrev header
  (:seq #\" header-name-core #\"))
