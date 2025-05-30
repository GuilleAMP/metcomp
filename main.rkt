#lang racket
(require "lexer.rkt"
         "parser.rkt")

;; Esta función recibe un input-port y parsea usando `parse` de 1 argumento
(define (run-parser input-port)
  (define (next) (next-token input-port))
  (define result (parse next))
  (close-input-port input-port)
  result)

;; Ejecuta el parser sobre un archivo real
(define input (open-input-file "code.cpp"))
(define result (run-parser input))
(displayln result)
