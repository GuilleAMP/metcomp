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
         "header.rkt"
         "text.rkt"
         "identifier-capitalized.rkt")

(provide next-token tokenize)

(define-lex-abbrev whitespace (:or #\space #\tab #\newline #\return))
(define-lex-abbrev number (:+ digit))

(define next-token
  (lexer
   [(eof) (token-EOF)]
   [whitespace (next-token input-port)]
   [data-type (token-DATA-TYPE lexeme)]
   ["class" (token-CLASS)]
   ["struct" (token-STRUCT)]
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
   ["template" (token-TEMPLATE)]
   ["typename" (token-TYPENAME)]
   ["ifdef"  (token-IFDEF)]
   ["ifndef" (token-IFNDEF)]
   ["elif"   (token-ELIF)]
   ["endif"  (token-ENDIF)]
   ["undef"  (token-UNDEF)]
   ["error"  (token-ERROR)]
   ["warning" (token-WARNING)]
   ["pragma" (token-PRAGMA)]
   ["line"   (token-LINE)]
   ["define" (token-DEFINE)]
   ["iostream" (token-IOSTREAM)]
   ["vector" (token-VECTOR)]
   ["cmath" (token-CMATH)]
   ["cstdlib" (token-CSTDLIB)]
   ["cstdio" (token-CSTDIO)]
   ["cstring" (token-CSTRING)]
   ["ctime" (token-CTIME)]
   ["algorithm" (token-ALGORITHM)]
   [#\" (token-QUOTE)]
   [getter-name (token-GETTER-NAME lexeme)]
   [setter-name (token-SETTER-NAME lexeme)]
   [function-object-call (token-FUNCTION-OBJECT-CALL lexeme)]
   [string (token-STRING lexeme)]
   [identifier (token-IDENTIFIER lexeme)]
   [identifier-capitalized (token-IDENTIFIER-CAPITALIZED lexeme)]
   [this-identifier (token-THIS-IDENTIFIER lexeme)]
   [literal (token-LITERAL lexeme)]
   [header (token-HEADER lexeme)]
   [integer (token-INTEGER (string->number lexeme))]
   [float (token-FLOAT (string->number lexeme))]
   [char (token-CHAR lexeme)]
   ["::" (token-SCOPE-RESOLUTION)]
   ["<<" (token-SHIFT-L)]
   [">>" (token-SHIFT-R)]
   [#\+ (token-PLUS)]
   [#\% (token-RESIDUAL)]
   [#\- (token-MINUS)]
   [#\* (token-MUL)]
   [#\, (token-COMMA)]
   [#\/ (token-DIV)]
   [#\= (token-ASSIGN)]
   [#\; (token-TERMINATOR)]
   [#\: (token-COLON)]
   ["!" (token-NOT)]
   ["==" (token-EQUAL)]
   ["!=" (token-NOT-EQUAL)]
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

(define (tokenize source)
  (define in (open-input-string source))
  (define tokens '())
  (let loop ()
    (define tok (next-token in))
    (if (eq? (token-name tok) 'EOF)
        (reverse (cons tok tokens))
        (begin
          (set! tokens (cons tok tokens))
          (loop)))))
