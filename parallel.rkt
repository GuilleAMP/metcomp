; Función para análisis secuencial
(define (sequential-analysis code-snippets)
  (map analyze-c++-code code-snippets))

; Función para análisis en paralelo. Aquí, "Futures" y "Touch" nos permiten automatizar la ejecución en varios procesadores. También, "code-snippets" es una lista de códigos para analizar
(define (parallel-analysis code-snippets)
  (define futures (map (lambda (code) (future (lambda () (analyze-c++-code code)))) code-snippets))
  (map touch futures))

; Función para medir el tiempo
(define (main)
  (define code-snippets (list example-c++-code1 example-c++-code2 example-c++-code3 example-c++-code4))
  ;; Ejecución paralela
  (define-values (par-time par-result)
    (time-apply (lambda () (parallel-analysis code-snippets)) '()))
  (displayln ("Resultado del análisis paralelo:"))
  (displayln par-result)
  (displayln (format "Tiempo de análisis paralelo: ~a ms" par-time)))