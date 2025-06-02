#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide text)

;; Subcomponentes
(define-lex-abbrev letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev space #\space)
(define-lex-abbrev special (:or #\! #\? #\. #\, #\; #\: #\- #\_ #\( #\) #\" #\' #\@ #\# #\$ #\% #\& #\/ #\= #\+ #\< #\>)) ; puedes ampliar esta lista

;; text = uno o más caracteres válidos (letras, dígitos, espacios o caracteres especiales)
(define-lex-abbrev text (:+ (:or letter digit space special)))
