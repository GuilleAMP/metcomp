#lang racket
(require "lexer.rkt"
         "parser.rkt"
         racket/future)

;; Función que evalúa el código, muestra errores o el resultado crudo
(define (run-parser-safe input-port)
  (define source (port->string input-port))
  (close-input-port input-port)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (displayln "❌ Código NO ACEPTADO:")
                     (displayln (exn-message e))
                     #f)])
    (define result (parse source))
    (displayln "✅ Código ACEPTADO")
    (displayln result)
    result))

;; Archivos fuente
(define archivos '("source10.cpp" "source20.cpp" "source30.cpp" "source50.cpp" "source100.cpp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🔵 EJECUCIÓN SECUENCIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "\n📍 EJECUCIÓN SECUENCIAL")
(define start-seq (current-inexact-milliseconds))

(define secuencial-tiempos
  (for/list ([archivo archivos])
    (displayln (format "\n📄 Procesando: ~a" archivo))
    (define input (open-input-file archivo))
    (define t0 (current-inexact-milliseconds))
    (run-parser-safe input)
    (define t1 (current-inexact-milliseconds))
    (define dur (- t1 t0))
    (displayln (format "⏱ Tiempo: ~a ms" dur))
    (list archivo dur)))

(define end-seq (current-inexact-milliseconds))
(define total-seq (- end-seq start-seq))
(displayln (format "\n⏲ Tiempo total SECUENCIAL: ~a ms" total-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🟢 EJECUCIÓN PARALELA CON FUTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;La librería de future y touch en Racket permite ejecutar código en paralelo aprovechando
;;varios núcleos del procesador. future lanza una tarea que se ejecuta de forma independiente
;;en otro hilo, mientras que touch se encarga de esperar y obtener el resultado de esa tarea cuando sea necesario.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "\n📍 EJECUCIÓN PARALELA CON FUTURE")
(define start-par (current-inexact-milliseconds))

(define futuros
  (for/list ([archivo archivos])
    (future
     (λ ()
       (define t0 (current-inexact-milliseconds))
       (define input (open-input-file archivo))
       (define output-port (open-output-string))

       ;; Redirige todo a un string
       (parameterize ([current-output-port output-port])
         (displayln (format "\n📄 Procesando: ~a" archivo))
         (run-parser-safe input))

       (define t1 (current-inexact-milliseconds))
       (define dur (- t1 t0))
       (parameterize ([current-output-port output-port])
         (displayln (format "⏱ Tiempo: ~a ms" dur)))

       (get-output-string output-port)))))

;; Mostrar resultados ordenadamente
(for ([resultado (map touch futuros)])
  (display resultado))

(define end-par (current-inexact-milliseconds))
(define total-par (- end-par start-par))
(displayln (format "\n⏲ Tiempo total PARALELO: ~a ms" total-par))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 📊 COMPARACIÓN FINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speedup (/ (exact->inexact total-seq) (exact->inexact total-par)))
(displayln (string-append "\n🚀 SPEEDUP (seq/par): "
                          (number->string speedup)
                          " veces más rápido"))