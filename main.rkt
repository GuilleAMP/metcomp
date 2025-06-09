#lang racket
(require "lexer.rkt"
         "parser.rkt")

;; Esta función recibe un input-port y parsea usando `parse`
(define (run-parser input-port)
  (define source (port->string input-port))
  (define result (parse source)) ; ✅ le pasa un string
  (close-input-port input-port)
  result)



;; Lista de archivos a procesar
(define archivos '("ejemplos.txt"))

;; Medir tiempo total
(define global-start (current-inexact-milliseconds))

;; Procesa cada archivo
(for ([archivo archivos])
  (displayln (format "\n📄 Procesando: ~a" archivo))
  (define input (open-input-file archivo))

  (define start-time (current-inexact-milliseconds))
  (define result (run-parser input))
  (define end-time (current-inexact-milliseconds))

  (define duration (- end-time start-time))
  (displayln result)
  (displayln (format "⏱ Tiempo de ejecución: ~a ms" duration)))

(define global-end (current-inexact-milliseconds))
(define global-duration (- global-end global-start))
(displayln (format "\n🧮 Tiempo total de ejecución: ~a ms" global-duration))
