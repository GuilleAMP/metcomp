#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide string char)

(define-lex-abbrev any-char-except-quote
  (:* (:~ #\")))

(define-lex-abbrev string
  (:seq #\" any-char-except-quote #\"))

(define-lex-abbrev char
  (:seq #\' any-char-except-quote #\'))
