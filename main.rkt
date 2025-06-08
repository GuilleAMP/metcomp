#lang racket
(require "lexer.rkt"
         "parser.rkt"
         (only-in xml write-xepr)
         xml)

;; Esta función recibe un input-port y parsea usando `parse`
(define (run-parser input-port)
  (define source (port->string input-port))
  (define result (parse source)) ; ✅ le pasa un string
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

;; Función principal para analizar y generar HTML
(define (generar-html-desde-codigo codigo)
  (define tokens (analizar codigo)) ; del lexer
  (define resultado-sintactico (with-handlers ([exn:fail? (λ (e) (exn-message e))])
                                 (parser tokens))) ; del parser

  ;; HTML básico en formato xexpr
  (define html-xexpr
    `(html
      (head
        (title "Resultado del Análisis"))
      (body
        (h1 "Análisis Léxico")
        (ul ,@(map (λ (t)
                     `(li ,(format "Tipo: ~a, Valor: ~a" (token-tipo t) (token-valor t))))
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