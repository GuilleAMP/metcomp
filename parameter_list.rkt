#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide parameter-lexer)

;; Abreviaciones léxicas
(define-lex-abbrev letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev underscore #\_)
(define-lex-abbrev identifier-rest (:* (:or letter digit underscore)))
(define-lex-abbrev identifier (:seq letter identifier-rest))

;; Lexer que reconoce una lista de parámetros separados por coma
(define parameter-lexer
  (lexer
   [(:: identifier) (token 'IDENTIFIER (lexeme))]
   [#\,             (token 'COMMA ",")]
   [(:+ whitespace) (parameter-lexer input-port)] ;; Ignorar espacios
   [eof             (token 'EOF)]))