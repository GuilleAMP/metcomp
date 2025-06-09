#lang racket
(require parser-tools/lex
         parser-tools/lex-sre
         "tokens.rkt"
         "identifier.rkt"
         "digit.rkt"
         "data-type.rkt"
         "literal.rkt"
         "integer.rkt"
         "float.rkt"
         "string-char.rkt"
         "boolean.rkt"
         "header.rkt"
         "text.rkt"
         "identifier-capitalized.rkt")

;; Definir whitespace correctamente con re+ y re-set
(define-lex-abbrev whitespace (re+ (re-set #\space #\tab #\newline #\return)))
(define-lex-abbrev number (re+ digit))

(define-tokens tokens
  (SYSTEM-HEADER
   DATA-TYPE CLASS STRUCT NAMESPACE FOR WHILE USING INCLUDE HASH
   CIN COUT ACCESS-SPECIFIER BOOLEAN RETURN BREAK DEFAULT ELSE IF
   LPAREN RPAREN TEMPLATE TYPENAME IFDEF IFNDEF ELIF ENDIF UNDEF ERROR
   WARNING PRAGMA LINE DEFINE IOSTREAM VECTOR CMATH CSTDLIB CSTDIO
   CSTRING CTIME ALGORITHM QUOTE GETTER-NAME SETTER-NAME
   FUNCTION-OBJECT-CALL STRING IDENTIFIER IDENTIFIER-CAPITALIZED THIS-IDENTIFIER
   LITERAL HEADER INTEGER FLOAT CHAR SCOPE-RESOLUTION SHIFT-L SHIFT-R
   PLUS RESIDUAL MINUS MUL COMMA DIV ASSIGN TERMINATOR COLON NOT EQUAL
   NOT-EQUAL OR AND BRACE-OPEN BRACE-CLOSE DOT H HPP HH INCREMENTER
   DECREMENTER LESS LESS-EQUAL MORE MORE-EQUAL TILDE NUMBER
  ))

(define next-token
  (lexer
   [(eof) 'EOF]
   [whitespace (next-token input-port)]
   [data-type (cons 'DATA-TYPE lexeme)]
   ["class" 'CLASS]
   ["struct" 'STRUCT]
   ["namespace" 'NAMESPACE]
   ["for" 'FOR]
   ["while" 'WHILE]
   ["using" 'USING]
   ["include" 'INCLUDE]
   ["#" 'HASH]
   [(:regexp #"<[^>]+>") (cons 'SYSTEM-HEADER lexeme)]
   ["cin" 'CIN]
   ["cout" 'COUT]
   ["public" (cons 'ACCESS-SPECIFIER 'public)]
   ["private" (cons 'ACCESS-SPECIFIER 'private)]
   ["protected" (cons 'ACCESS-SPECIFIER 'protected)]
   ["true"  (cons 'BOOLEAN #t)]
   ["false" (cons 'BOOLEAN #f)]
   ["return" 'RETURN]
   ["break" 'BREAK]
   ["default" 'DEFAULT]
   ["else" 'ELSE]
   ["if" 'IF]
   ["(" 'LPAREN]
   [")" 'RPAREN]
   ["template" 'TEMPLATE]
   ["typename" 'TYPENAME]
   ["ifdef"  'IFDEF]
   ["ifndef" 'IFNDEF]
   ["elif"   'ELIF]
   ["endif"  'ENDIF]
   ["undef"  'UNDEF]
   ["error"  'ERROR]
   ["warning" 'WARNING]
   ["pragma" 'PRAGMA]
   ["line"   'LINE]
   ["define" 'DEFINE]
   ["iostream" 'IOSTREAM]
   ["vector" 'VECTOR]
   ["cmath" 'CMATH]
   ["cstdlib" 'CSTDLIB]
   ["cstdio" 'CSTDIO]
   ["cstring" 'CSTRING]
   ["ctime" 'CTIME]
   ["algorithm" 'ALGORITHM]
   [#\" 'QUOTE]
   [getter-name (cons 'GETTER-NAME lexeme)]
   [setter-name (cons 'SETTER-NAME lexeme)]
   [function-object-call (cons 'FUNCTION-OBJECT-CALL lexeme)]
   [string (cons 'STRING lexeme)]
   [identifier (cons 'IDENTIFIER lexeme)]
   [identifier-capitalized (cons 'IDENTIFIER-CAPITALIZED lexeme)]
   [this-identifier (cons 'THIS-IDENTIFIER lexeme)]
   [literal (cons 'LITERAL lexeme)]
   [header (cons 'HEADER lexeme)]
   [integer (cons 'INTEGER (string->number lexeme))]
   [float (cons 'FLOAT (string->number lexeme))]
   [char (cons 'CHAR lexeme)]
   ["::" 'SCOPE-RESOLUTION]
   ["<<" 'SHIFT-L]
   [">>" 'SHIFT-R]
   [#\+ 'PLUS]
   [#\% 'RESIDUAL]
   [#\- 'MINUS]
   [#\* 'MUL]
   [#\, 'COMMA]
   [#\/ 'DIV]
   [#\= 'ASSIGN]
   [#\; 'TERMINATOR]
   [#\: 'COLON]
   ["!" 'NOT]
   ["==" 'EQUAL]
   ["!=" 'NOT-EQUAL]
   ["||" 'OR]
   ["&&" 'AND]
   ["{" 'BRACE-OPEN]
   ["}" 'BRACE-CLOSE]
   ["." 'DOT]
   ["h" 'H]
   ["hpp" 'HPP]
   ["hh" 'HH]
   ["++" 'INCREMENTER]
   ["--" 'DECREMENTER]
   ["<" 'LESS]
   ["<=" 'LESS-EQUAL]
   [">" 'MORE]
   [">=" 'MORE-EQUAL]
   ["~" 'TILDE]
   [number (cons 'NUMBER (string->number lexeme))]
  ))

(define (token-tipo token)
  (if (pair? token)
      (car token)
      token))

(define (token-valor token)
  (if (pair? token)
      (cdr token)
      #f))

(define (analizar source)
  (define in (open-input-string source))
  (let loop ([tokens '()])
    (define token (next-token in))
    (if (eq? token 'EOF)
        (reverse tokens)
        (loop (cons token tokens)))))

(provide next-token analizar token-tipo token-valor)
