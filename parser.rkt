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
     [() '()] ; caso base
     [(sentencia sentencias) (cons $1 $2)]]

    ;; Sentencias posibles
    [sentencia
     ;; ------------------------------------  2. FUNCTION  ----------------------------------------------------
     [(function)
      (list $1)]
     ;; -------------------------------------- 3. STRUCT --------------------------------------------------------
     [(struct)
      (list $1)]
      ;; ------------------------------------- 4. PRE COMPILER SYSTEM INCLUDE --------------------------------------------------------
     [(pre-compiler-system-include)
      (list $1)]
      ;; ------------------------------------  6. PRE COMPILER LOCAL INCLUDE  ----------------------------------------------------
     [(pre-compiler-local-include)
      (list $1)]
          ;; ------------------------------------  7.USING DIRECTIVE  ----------------------------------------------------
     [(using-directive)
      (list $1)]
     ;; ------------------------------------  8. INPUT OUTPUT  ----------------------------------------------------
     [(input-output)
      (list $1)]
     ;; ------------------------------------ 9. ACCESS MODIFIER ----------------------------------------------------
     [(access-specifier)
      (list $1)]
     ;; --------------------------------- 10. VARIABLE DECLARATIONS -------------------------------------------------
     ;; Simple
     [(variable-declaration)
      (list $1)]
     ;; --------------------------------- 11. VARIABLE ASSIGNATION-------------------------------------------------
     [(variable-assignation)
      (list $1)]
     ;;  --------------------------------- 12. STATEMENT -------------------------------------------------
     ;;[(statement)
      ;;(list $1)]
         ;;  --------------------------------- 13. RETURN STATEMENT -------------------------------------------------
     [(return-statement)
      (list $1)]
     ;;  --------------------------------- 14. IF STATEMENT -------------------------------------------------
     [(if-statement)
      (list $1)]
      ;;  --------------------------------- 15. LOOP STRUCTURE -------------------------------------------------
     [(loop-structure)
      (list $1)]
      ;;  --------------------------------- 16. CLASS DEFINITION -------------------------------------------------
     [(class-definition)
      (list $1)]
     ;;  --------------------------------- 17. CONSTRUCTOR -------------------------------------------------
     [(constructor)
      (list $1)]
     ;;  --------------------------------- 18. DESTRUCTOR -------------------------------------------------
     [(destructor)
      (list $1)]
     ;;  --------------------------------- 19. GETTER -------------------------------------------------
     ;[(getter)
     ; (list $1)]
     ;;  --------------------------------- 20. SETTER -------------------------------------------------
     ;[(setter)
     ; (list $1)]
     ;;  --------------------------------- 21. CLASS HEADER -------------------------------------------------
     [(class-header)
      (list $1)]
          ;;  --------------------------------- 22. CLASS FUNCTION -------------------------------------------------
     [(class-function)
      (list $1)]
     [(object-assignation)
      (list $1)]
     [(object-function-call)
      (list $1)]
               ;;  --------------------------------- 23. BOOLEAN FUNCTION -------------------------------------------------
     ;;  --------------------------------- 30. TEMPLATE DECLARATION -------------------------------------------------
     [(template-declaration)
      (list $1)]]
    
    ;;
     
    ;; Expresiones básicas y binarias
    ;; EXPRESSION
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
     [(OR)  'or]
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
     [(BOOLEAN logical-operator BOOLEAN)       (list $2 $1 $3)]
     [(IDENTIFIER) (list $1)]
     [(object-function-call) (list $1)]]
    ;; RETURN EXPRESSION
    [return-expression
     [(expression) $1]
     [(logical-expression) $1]
     [(function-call) $1]]
    ;; ARGUMENT LIST
    [argument-list
     [(expression) (list $1)]
     [(expression COMMA expression) (cons $1 $3)]]
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
     [(DATA-TYPE IDENTIFIER TERMINATOR)
      (list 'declare $1 $2)]
     [(IDENTIFIER IDENTIFIER TERMINATOR)
      (list 'declare-object $1 $2)]
     [(DATA-TYPE IDENTIFIER ASSIGN expression TERMINATOR)
      (list 'assign-declare $1 $2 $4)]]
    ;; ACCESS SPECIFIER
    [access-specifier
     [(ACCESS-SPECIFIER COLON) (list 'access-specifier $1)]]
    ;; INPUT OUTPUT
    [input-output
     [(COUT SHIFT-L cout-cin-operand TERMINATOR)
      (list 'cout $3)]
     [(CIN SHIFT-R cout-cin-operand TERMINATOR)
      (list 'cin $3)]]
    ;; RETURN STATEMENT
    [return-statement
     [(RETURN return-expression TERMINATOR)
      (list 'return $2)]
     [(RETURN TERMINATOR)
      (list 'return)]]
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
     [(BRACE-OPEN statement-list BRACE-CLOSE)
      (list 'block $2)]]
    [statement-list
     [() '()] ; bloque vacío
     [(statement statement-list) (cons $1 $2)]]
    ;; ELSE BLOCK
    [else-statement
     [(ELSE  block) (list 'else)]]
    ;; IF STATEMENT
    [if-statement
     [(IF LPAREN logical-expression RPAREN block) (list 'if $3 $5)]
     [(IF LPAREN logical-expression RPAREN block else-statement) (list 'if-else $3 $5 $6)]]
    ;; scope-resolution
    [scope-resolution
     [ (SCOPE-RESOLUTION) '::] ]
    ;; namespace-name (recursiva)
    [namespace-name
     [ (IDENTIFIER) $1 ]
     [ (IDENTIFIER scope-resolution namespace-name) (list $1 ':: $3)] ]
    ;; using-directive
    [using-directive
     [ (USING namespace-name TERMINATOR) (list 'using $2)]]
    ;; PRECOMPILER LOCAL
    [pre-compiler-local-include
     [(HASH INCLUDE HEADER)
      (list 'include-local $3)]]
    ;; INCREMENT OPRATOR
    [increment-operator
     [(INCREMENTER) '++]
     [(DECREMENTER) '--]]
    ;; UPDATE EXPRESSION
    [update-expression
     [(IDENTIFIER increment-operator) $1]]
    ;; FOR INTIALIZATION
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
     [(TILDE IDENTIFIER LPAREN RPAREN) (list 'destructor)]]
    ;; CONSTRUCTOR
    [constructor
     [(IDENTIFIER LPAREN RPAREN block) (list 'constructor $1 $4)]
     [(IDENTIFIER LPAREN parameter-list RPAREN block) (list 'constructor $1 $3 $5)]]
    ; FUNCTION
    [function
     [(DATA-TYPE IDENTIFIER LPAREN parameter-list RPAREN block) (list 'function $2 $4 $6)]
     [(DATA-TYPE IDENTIFIER LPAREN RPAREN block) (list 'function $2 $5)]
     [(DATA-TYPE IDENTIFIER LPAREN parameter-list RPAREN TERMINATOR) (list 'function $2 $4)]
     [(DATA-TYPE IDENTIFIER LPAREN RPAREN TERMINATOR) (list 'function-no-par $2)]]
    ;; CLASS MEMBER
    [class-member
     [(variable-declaration) $1]
     [(function) $1]
     [(constructor) $1]
     [(destructor) $1]
     [(getter) $1]
     [(setter) $1]]
    ;; CLASS MEMBER LIST
    [class-member-list
     [() '()]
     [(class-member class-member-list) (cons $1 $2)]]
    ;; CLASS BODY
    [class-body
     [(access-specifier class-member-list) (list 'class-body $2)]]
    [class-body-list
     [() '()]
     [(class-body class-body-list) (cons $1 $2)]]
    ;; CLASS BLOCL
    [class-block
     [(BRACE-OPEN class-body-list BRACE-CLOSE TERMINATOR) (list 'class-block $2)]]
    ;; CLASS DEFINITION
    [class-definition
     [(class-header class-block) (list 'class-definition $1 $2)]]
    ;; SYSTEM HEADER
    [system-header-name
     [(IOSTREAM) 'iostream]
     [(STRING) 'string]
     [(VECTOR) 'vector]
     [(CMATH) 'cmath]
     [(CSTDLIB) 'cstdlib]
     [(CSTDIO) 'cstdio]
     [(CSTRING) 'cstring]
     [(CTIME) 'ctime]
     [(ALGORITHM) 'algorithm]]
    ;; SYSTEM HEADER
    [system-header
     [(LESS system-header-name MORE)
      (list 'system-header $2)]
     [(LESS DATA-TYPE MORE) (list 'system-header $2)]]
    ;; MACRO BODY
    [macro-body
     [(TEXT) (list $1)]
     [(TEXT macro-body) (cons $1 $2)]]

    ;; PRE-COMPILER SYSTEM INCLUDE
    [pre-compiler-system-include
     [(HASH INCLUDE system-header)
      (list 'include $3)]]
       ;; STRUCT MEMBER
   [struct-member
    [(DATA-TYPE IDENTIFIER TERMINATOR)
     (list 'struct-member-declare $1 $2)]
    [(DATA-TYPE IDENTIFIER ASSIGN expression TERMINATOR)
     (list 'struct-member-assign $1 $2 $4)]]

   ;; STRUCT BODY
   [struct-body
    [(struct-member) (list $1)]
    [(struct-member struct-body) (cons $1 $2)]]

   ;; STRUCT
   [struct
    [(STRUCT IDENTIFIER BRACE-OPEN struct-body BRACE-CLOSE TERMINATOR)
     (list 'struct $2 $4)]]

      ;; GETTER
   [getter
    [(DATA-TYPE GETTER-NAME LPAREN RPAREN block)
     (list 'getter $2 $1 $5)]]

   ;; SETTER
   [setter
    [(DATA-TYPE SETTER-NAME LPAREN parameter RPAREN block)
     (list 'setter $2 $1 $4 $6)]]
   
   ;; TEMPLATE PARAMETER
   [template-parameter
    [(TYPENAME IDENTIFIER )
     (list 'template-parameter $2)]
    [(CLASS IDENTIFIER )
     (list 'template-parameter $2)]
    [(DATA-TYPE IDENTIFIER )
     (list 'template-parameter $2)]
    [(TYPENAME DATA-TYPE )
     (list 'template-parameter $2)]
    [(CLASS DATA-TYPE )
     (list 'template-parameter $2)]
    [(DATA-TYPE DATA-TYPE )
     (list 'template-parameter $2)]]

   ;;TEMPLATE DECLARATION
   [template-declaration
    [(TEMPLATE LESS template-parameter MORE)
     (list 'template-declaration $3)]]

   ;; BASE CLASS LIST
   [base-class-list
    [(IDENTIFIER) (list $1)]
    [(IDENTIFIER COMMA base-class-list) (cons $1 $3)]]

   ;; INHERITANCE
   [inheritance
    [(COLON ACCESS-SPECIFIER base-class-list) (list 'inheritance $3)]]

   ;; CLASS HEADER
   [class-header
    [(CLASS IDENTIFIER inheritance) (list 'class-header $2 $3)]
    [(CLASS IDENTIFIER) (list 'class-header $2)]]

   ;; CLASS FUNCTIONl
   [class-function-name
    [(IDENTIFIER SCOPE-RESOLUTION IDENTIFIER) (list 'class-function-name $1 $3)]]
   [class-function
    [(DATA-TYPE class-function-name LPAREN parameter-list RPAREN block) (list 'class-function $2 $4 $6)]
    [(DATA-TYPE class-function-name LPAREN RPAREN block) (list 'class-function $2 $5)]]

   ;; BOOLEAN EXPRESSION
   [boolean-expression
    [(BOOLEAN) (list 'boolean $1)]
    [(IDENTIFIER logical-operator expression) (list 'boolean-expression $1 $3)]]
   ;; OBJECT ASSIGNATION
   [object-assignation
    [(IDENTIFIER IDENTIFIER LPAREN argument-list RPAREN TERMINATOR) (list 'object-assignation $1 $2 $4)]]
   ;; OBJECT FUNCTION CALL
   [object-function-call
    [(FUNCTION-OBJECT-CALL LPAREN argument-list RPAREN TERMINATOR) (list 'object-function-call $1 $3)]
    [(FUNCTION-OBJECT-CALL LPAREN RPAREN) (list 'object-function-call $1)]
    [(FUNCTION-OBJECT-CALL LPAREN RPAREN TERMINATOR) (list 'object-function-call $1)]]
    ]))

(define (parse source)
  (current-source source)
  (token-count 0)
  (prev-token #f)
  (current-token #f)
  (next-token-cache #f)

  (define port (open-input-string source))

  (define (counted-next-token)
    ;; Guarda tokens previo y actual
    (prev-token (current-token))
    (define tok (next-token port))
    (current-token tok)
    (token-count (add1 (token-count)))
    tok)

  (raw-parser counted-next-token))

(define (run-parse tokens)
  (parameterize ((token-count 0)
                 (prev-token #f)
                 (current-token #f))
    (raw-parser tokens)))

(provide run-parse)
