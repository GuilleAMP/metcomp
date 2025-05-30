#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "tokens.rkt"
         "identifier.rkt"
         "digit.rkt"
         "data-type.rkt"
         "literal.rkt"
         "integer.rkt"
         "float.rkt"
         "string-char.rkt"
         "boolean.rkt"
         "header.rkt")

(provide next-token)

(define-lex-abbrev whitespace (:or #\space #\tab #\newline))
(define-lex-abbrev number (:+ digit))

(define next-token
  (lexer
   [(eof) (token-EOF)]
   [whitespace (next-token input-port)]
   [data-type (token-DATA-TYPE lexeme)]
   ["class" (token-CLASS)]
   ["for" (token-FOR)]
   ["while" (token-WHILE)]
   ["using" (token-USING)]
   ["include" (token-INCLUDE)]
   ["#" (token-HASH)]
   ["cin" (token-CIN)]
   ["cout" (token-COUT)]
   ["public" (token-ACCESS-SPECIFIER 'public)]
   ["private" (token-ACCESS-SPECIFIER 'private)]
   ["protected" (token-ACCESS-SPECIFIER 'protected)]
   ["true"  (token-BOOLEAN #t)]
   ["false" (token-BOOLEAN #f)]
   ["return" (token-RETURN)]
   ["break" (token-BREAK)]
   ["default" (token-DEFAULT)]
   ["else" (token-ELSE)]
   ["if" (token-IF)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   [#\" (token-QUOTE)]
   [header (token-HEADER lexeme)]
   [identifier (token-IDENTIFIER lexeme)]
   [literal (token-LITERAL lexeme)]
   [integer (token-INTEGER (string->number lexeme))]
   [float (token-FLOAT (string->number lexeme))]
   [string (token-STRING lexeme)]
   [char (token-CHAR lexeme)]
   ["::" (token-SCOPE-RESOLUTION)]
   ["<<" (token-SHIFT-L)]
   [">>" (token-SHIFT-R)]
   [#\+ (token-PLUS)]
   [#\- (token-MINUS)]
   [#\* (token-MUL)]
   [#\, (token-COMMA)]
   [#\/ (token-DIV)]
   [#\= (token-ASSIGN)]
   [#\; (token-TERMINATOR)]
   [#\: (token-COLON)]
   ["!" (token-NOT)]
   ["||" (token-OR)]
   ["&&" (token-AND)]
   ["{" (token-BRACE-OPEN)]
   ["}" (token-BRACE-CLOSE)]
   ["." (token-DOT)]
   ["h" (token-H)]
   ["hpp" (token-HPP)]
   ["hh" (token-HH)]
   ["++" (token-INCREMENTER)]
   ["--" (token-DECREMENTER)]
   ["<" (token-LESS)]
   ["<=" (token-LESS-EQUAL)]
   [">" (token-MORE)]
   [">=" (token-MORE-EQUAL)]
   ["~" (token-TILDE)]
   [number (token-NUMBER (string->number lexeme))]))
