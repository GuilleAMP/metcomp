#lang racket
(require "lexer.rkt"
         "parser.rkt"
         (only-in xml write-xexpr)
         xml)

;; Convierte la lista de tokens en una función generadora de tokens
(define (make-token-generator tokens)
  (let ([tokens tokens]) ; variable local para consumir la lista
    (lambda ()
      (cond
        [(null? tokens) 'EOF] ; devuelve símbolo EOF cuando se acaban
        [else
         (define t (car tokens))
         (set! tokens (cdr tokens))
         t]))))

;; Esta función recibe un input-port y parsea usando `run-parse` del parser
(define (run-parser input-port)
  (define source (port->string input-port))
  (define tokens (analizar source))   ; lexer -> lista tokens
  (define get-token (make-token-generator tokens)) ; función para obtener tokens
  (define result (run-parse get-token)) ; parser consume tokens
  (close-input-port input-port)
  result)

;; Lista de archivos a procesar
(define archivos '("source10.cpp"))

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

;; Función principal para analizar y generar HTML con resultados
(define (generar-html-desde-codigo codigo)
  (define tokens (analizar codigo)) ; del lexer
  (define get-token (make-token-generator tokens))
  (define resultado-sintactico
    (with-handlers ([exn:fail? (λ (e) (exn-message e))])
      (run-parse get-token))) ; parseo seguro con manejo de errores

  ;; HTML básico en formato xexpr
  (define html-xexpr
    `(html
      (head
        (title "Resultado del Análisis"))
      (body
        (h1 "Análisis Léxico")
        (ul ,@(map (λ (t)
                     `(li ,(format "Tipo: ~a, Valor: ~a"
                                   (token-tipo t) (token-valor t))))
                   tokens))
        (h1 "Resultado del Análisis Sintáctico")
        (p ,(format "~a" resultado-sintactico)))))

  ;; Escribir en archivo HTML
  (with-output-to-file "resultado.html"
    (λ () (write-xexpr html-xexpr))
    #:exists 'replace))

;; Prueba directa
(module+ main
  (define codigo-ejemplo "#include <iostream>\nclass Hola { int x; };")
  (generar-html-desde-codigo codigo-ejemplo))
