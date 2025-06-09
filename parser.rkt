#lang racket
(require parser-tools/yacc
         "tokens.rkt"
         "lexer.rkt")

(provide parse)

(define current-source (make-parameter ""))
(define token-count (make-parameter 0))
(define prev-token (make-parameter #f))
(define current-token (make-parameter #f))
(define next-token-cache (make-parameter #f)) ; opcional, para ver uno adelante

(define raw-parser
  (parser
   [start program]
   [end EOF]
   [tokens value-tokens op-tokens]
   [error
    (lambda (tok-name tok-val token)
      (define previous (prev-token))
      (define current (current-token))
      (define previous-str (if previous (format "~a" previous) "Ninguno"))
      (define current-str (format "~a" current))
      (error (format "❌ Syntax error at token #~a: '~a'\n🔙 Anterior: ~a\n📍 Actual: ~a"
                     (token-count)
                     (or tok-val tok-name)
                     previous-str
                     current-str)))]
   [grammar

    ;; Programa = lista de sentencias
    [program [(sentencias) $1]]

    ;; Sentencias múltiples (recursivo)
    [sentencias
     [() '()]
     [(sentencia sentencias) (cons $1 $2)]]

    ;; Sentencias posibles
    [sentencia
     [(function) (list $1)]
     [(struct) (list $1)]
     [(pre-compiler-system-include) (list $1)]
     [(pre-compiler-local-include) (list $1)]
     [(using-directive) (list $1)]
     [(input-output) (list $1)]
     [(access-specifier) (list $1)]
     [(variable-declaration) (list $1)]
     [(variable-assignation) (list $1)]
     [(return-statement) (list $1)]
     [(if-statement) (list $1)]
     [(loop-structure) (list $1)]
     [(class-definition) (list $1)]
     [(constructor) (list $1)]
     [(destructor) (list $1)]
     [(class-header) (list $1)]
     [(class-function) (list $1)]
     [(object-assignation) (list $1)]
     [(object-function-call) (list $1)]
     [(template-declaration) (list $1)]]

    ;; Expresiones básicas y binarias
    [expression
     [(LITERAL) $1]
     [(IDENTIFIER) $1]
     [(expression arithmetic-operator expression) (list $2 $1 $3)]
     [(STRING) $1]
     [(BOOLEAN) $1]
     [(THIS-IDENTIFIER) $1]
     [(object-function-call) $1]]

    ;; COUT CIN OPERAND
    [cout-cin-operand
     [(IDENTIFIER) $1]
     [(STRING) $1]
     [(THIS-IDENTIFIER) $1]
     [(function-call) $1]]

    ;; LOGICAL OPERATOR
    [logical-operator
     [(AND) 'and]
     [(OR) 'or]
     [(MORE) 'more]
     [(LESS) 'less]
     [(MORE-EQUAL) 'more-equal]
     [(LESS-EQUAL) 'less-equal]
     [(EQUAL) 'equal]
     [(NOT-EQUAL) 'not-equal]]

    ;; ARITHMETIC OPERATOR
    [arithmetic-operator
     [(PLUS) '+]
     [(MINUS) '-]
     [(MUL) '*]
     [(DIV) '/]
     [(RESIDUAL) '%]]

    ;; LOGICAL EXPRESSION
    [logical-expression
     [(IDENTIFIER logical-operator IDENTIFIER) (list $2 $1 $3)]
     [(expression logical-operator LITERAL) (list $2 $1 $3)]
     [(IDENTIFIER logical-operator LITERAL) (list $2 $1 $3)]
     [(BOOLEAN logical-operator BOOLEAN) (list $2 $1 $3)]
     [(IDENTIFIER) (list $1)]
     [(object-function-call) (list $1)]]

    ;; RETURN EXPRESSION
    [return-expression
     [(expression) $1]
     [(logical-expression) $1]
     [(function-call) $1]]

    ;; ARGUMENT LIST (Corrección para lista recursiva)
    [argument-list
     [(expression) (list $1)]
     [(argument-list COMMA expression) (append $1 (list $3))]]

    ;; FUNCTION CALL
    [function-call
     [(IDENTIFIER LPAREN RPAREN) (list $1)]
     [(IDENTIFIER LPAREN argument-list RPAREN) (list $1 $3)]
     [(IDENTIFIER LPAREN RPAREN TERMINATOR) (list $1)]
     [(IDENTIFIER LPAREN argument-list RPAREN TERMINATOR) (list $1 $3)]
     [(SETTER-NAME LPAREN argument-list RPAREN TERMINATOR) (list 'function-setter-call $1 $3)]
     [(GETTER-NAME LPAREN RPAREN TERMINATOR) (list 'function-getter-call $1)]]

    ;; VARIABLE ASSIGNATION
    [variable-assignation
     [(IDENTIFIER ASSIGN expression TERMINATOR) (list 'variable-assignation $1 $3)]]

    ;; VARIABLE DECLARATION
    [variable-declaration
     [(DATA-TYPE IDENTIFIER TERMINATOR) (list 'declare $1 $2)]
     [(IDENTIFIER IDENTIFIER TERMINATOR) (list 'declare-object $1 $2)]
     [(DATA-TYPE IDENTIFIER ASSIGN expression TERMINATOR) (list 'assign-declare $1 $2 $4)]]

    ;; ACCESS SPECIFIER
    [access-specifier
     [(ACCESS-SPECIFIER COLON) (list 'access-specifier $1)]]

    ;; INPUT OUTPUT
    [input-output
     [(COUT SHIFT-L cout-cin-operand TERMINATOR) (list 'cout $3)]
     [(CIN SHIFT-R cout-cin-operand TERMINATOR) (list 'cin $3)]]

    ;; RETURN STATEMENT
    [return-statement
     [(RETURN return-expression TERMINATOR) (list 'return $2)]
     [(RETURN TERMINATOR) (list 'return)]]

    ;; STATEMENT
    [statement
     [(variable-declaration) $1]
     [(variable-assignation) $1]
     [(function-call) $1]
     [(return-statement) $1]
     [(if-statement) $1]
     [(input-output) $1]
     [(loop-structure) $1]
     [(object-assignation) $1]
     [(object-function-call) $1]]

    ;; BLOCK
    [block
     [(BRACE-OPEN statement-list BRACE-CLOSE) (list 'block $2)]]

    [statement-list
     [() '()]
     [(statement statement-list) (cons $1 $2)]]

    ;; ELSE BLOCK
    [else-statement
     [(ELSE block) (list 'else)]]

    ;; IF STATEMENT
    [if-statement
     [(IF LPAREN logical-expression RPAREN block) (list 'if $3 $5)]
     [(IF LPAREN logical-expression RPAREN block else-statement) (list 'if-else $3 $5 $6)]]

    ;; scope-resolution
    [scope-resolution [(SCOPE-RESOLUTION) '::]]

    ;; namespace-name (recursiva)
    [namespace-name
     [(IDENTIFIER) $1]
     [(IDENTIFIER scope-resolution namespace-name) (list $1 ':: $3)]]

    ;; using-directive
    [using-directive
     [(USING namespace-name TERMINATOR) (list 'using $2)]]

    ;; PRECOMPILER LOCAL
    [pre-compiler-local-include
     [(HASH INCLUDE HEADER) (list 'include-local $3)]]

    ;; INCREMENT OPERATOR
    [increment-operator
     [(INCREMENTER) '++]
     [(DECREMENTER) '--]]

    ;; UPDATE EXPRESSION
    [update-expression
     [(IDENTIFIER increment-operator) $1]]

    ;; FOR INITIALIZATION
    [for-intialization
     [(variable-declaration) $1]
     [(variable-assignation) $1]]

    ;; FOR LOOP
    [for-loop
     [(FOR LPAREN for-intialization logical-expression TERMINATOR update-expression RPAREN block) (list 'for-loop $3 $4 $8)]]

    ;; WHILE LOOP
    [while-loop
     [(WHILE LPAREN logical-expression RPAREN block) (list 'while-loop $3 $5)]]

    ;; LOOP STRUCTURE
    [loop-structure
     [(while-loop) $1]
     [(for-loop) $1]]

    ;; PARAMETER
    [parameter
     [(DATA-TYPE IDENTIFIER) (list 'parameter $2)]]

    ;; PARAMETER LIST
    [parameter-list
     [(parameter) (list $1)]
     [(parameter COMMA parameter) (cons $1 $3)]]

    ;; DESTRUCTOR
    [destructor
     [(TILDE IDENTIFIER LPAREN RPAREN block) (list 'destructor $2 $5)]
     [(TILDE IDENTIFIER LPAREN RPAREN TERMINATOR) (list 'destructor $2)]]

    ;; CONSTRUCTOR
    [constructor
     [(IDENTIFIER LPAREN RPAREN block) (list 'constructor $1 $4)]
     [(IDENTIFIER LPAREN parameter-list RPAREN block) (list 'constructor $1 $3 $5)]]

    ;; FUNCTION
    [function
     [(DATA-TYPE IDENTIFIER LPAREN RPAREN block) (list 'function $1 $2 $5)]
     [(DATA-TYPE IDENTIFIER LPAREN parameter-list RPAREN block) (list 'function $1 $2 $4 $6)]
     [(DATA-TYPE IDENTIFIER LPAREN RPAREN TERMINATOR) (list 'function $1 $2)]
     [(DATA-TYPE IDENTIFIER LPAREN parameter-list RPAREN TERMINATOR) (list 'function $1 $2 $4)]]

    ;; CLASS HEADER
    [class-header
     [(CLASS IDENTIFIER BRACE-OPEN class-content BRACE-CLOSE) (list 'class-header $2 $4)]
     [(CLASS IDENTIFIER COLON ACCESS-SPECIFIER IDENTIFIER BRACE-OPEN class-content BRACE-CLOSE)
      (list 'class-header-inheritance $2 $4 $5 $7)]]

    ;; CLASS CONTENT
    [class-content
     [() '()]
     [(class-member class-content) (cons $1 $2)]]

    ;; CLASS MEMBER
    [class-member
     [(access-specifier) $1]
     [(variable-declaration) $1]
     [(function) $1]
     [(constructor) $1]
     [(destructor) $1]]

    ;; CLASS DEFINITION (simplificada)
    [class-definition
     [(class-header) $1]]

    ;; STRUCT
    [struct
     [(STRUCT IDENTIFIER BRACE-OPEN struct-content BRACE-CLOSE) (list 'struct $2 $4)]]

    ;; STRUCT CONTENT
    [struct-content
     [() '()]
     [(variable-declaration struct-content) (cons $1 $2)]]

    ;; OBJECT ASSIGNATION
    [object-assignation
     [(IDENTIFIER DOT IDENTIFIER ASSIGN expression TERMINATOR) (list 'object-assignation $1 $3 $5)]]

    ;; OBJECT FUNCTION CALL
    [object-function-call
     [(IDENTIFIER DOT IDENTIFIER LPAREN RPAREN TERMINATOR) (list 'object-function-call $1 $3)]
     [(IDENTIFIER DOT IDENTIFIER LPAREN argument-list RPAREN TERMINATOR) (list 'object-function-call $1 $3 $5)]]

    ;; TEMPLATE DECLARATION
    [template-declaration
     [(TEMPLATE LT DATA-TYPE GT) (list 'template $3)]]
    ]))

;; Función para parsear y capturar tokens actuales para debug
(define (parse input)
  (current-source input)
  (token-count 0)
  (prev-token #f)
  (current-token #f)
  (next-token-cache #f)
  (raw-parser input))
