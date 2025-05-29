#lang racket
(require "lexer.rkt"
         "parser.rkt")

(define (run-parser input-string)
  (define in (open-input-string input-string))
  (define (next) (next-token in))
  (define result (parse next))
  (close-input-port in)
  result)

;; Prueba
(displayln (run-parser "cout << a;
                        return a + b + c;
                        cin >> b;
                        public:
                        int abuela123;
                        int num = 28;
                        num = num + 2;
                        abc = 2;"))
(displayln (run-parser "a, b, c"))

