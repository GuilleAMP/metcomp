#lang racket
(require parser-tools/yacc
         "tokens.rkt")

(provide parse)

(define parse
  (parser
   [start program]
   [end EOF]
   [tokens value-tokens op-tokens]
   [error
    (lambda (tok-name tok-val start end)
      (error "Syntax error: unexpected token ~a at ~a-~a"
             tok-name start end))]

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
      (list $1)]]
    ;;
     
    ;; Expresiones básicas y binarias
    ;; EXPRESSION
    [expression
     [(LITERAL) $1]
     [(IDENTIFIER) $1]
     [(expression arithmetic-operator expression) (list $2 $1 $3)]]
    ;; COUT CIN OPERAND
    [cout-cin-operand
     [(IDENTIFIER) $1]
     [(STRING) $1]]
    ;; LOGICAL OPERATOR
    [logical-operator
     [(AND) 'and]
     [(OR)  'or]
     [(MORE) 'more]
     [(LESS) 'less]
     [(MORE-EQUAL) 'more-equal]
     [(LESS-EQUAL) 'less-equal]]
    ;; ARITHMETIC OPERATOR
    [arithmetic-operator
     [(PLUS) '+]
     [(MINUS) '-]
     [(MUL) '*]
     [(DIV) '/]]
    ;; LOGICAL EXPRESSION
    [logical-expression
     [(IDENTIFIER logical-operator IDENTIFIER) (list $2 $1 $3)]
     [(BOOLEAN logical-operator BOOLEAN)       (list $2 $1 $3)]]
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
     [(IDENTIFIER LPAREN argument-list RPAREN TERMINATOR) (list $1 $3)]]
    ;; VARIABLE ASSIGNATION
    [variable-assignation
     [(IDENTIFIER ASSIGN expression TERMINATOR) (list 'variable-assignation $1 $3)]]
    ;; VARIABLE DECLARATION
    [variable-declaration
     [(DATA-TYPE IDENTIFIER TERMINATOR)
      (list 'declare $1 $2)]
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
     [(input-output) $1]]
    ;; BLOCK
    [block
     [(BRACE-OPEN statement-list BRACE-CLOSE)
      (list 'block $2)]]
    [statement-list
     [() '()] ; bloque vacío
     [(statement statement-list) (cons $1 $2)]]
    ;; ELSE BLOCK
    [else-statement
     [(ELSE COLON block) (list 'else $3)]]
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
     [(DATA-TYPE IDENTIFIER LPAREN parameter-list RPAREN block) (list 'function $2 $4 $6)]]
    ;; CLASS MEMBER
    [class-member
     [(variable-declaration) $1]
     [(function) $1]
     [(constructor) $1]
     [(destructor) $1]]
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
     [(BRACE-OPEN class-body-list BRACE-CLOSE) (list 'class-block $2)]]
    ;; CLASS DEFINITION
    [class-definition
     [(CLASS IDENTIFIER class-block) (list 'class-definition $2 $3)]]
    ]))
