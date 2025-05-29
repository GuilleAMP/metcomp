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
    [program [(statements) $1]]

    ;; PARAMETER
    [parameter
     [(IDENTIFIER) $1]]

    ;; PARAMETER LIST
    [parameter-list
     [(parameter)(list $1)]
     [(parameter COMMA parameter-list)(cons $1 $3)]]

;; -------------------- 6. Pre-Compiler-Local-Include -----------------
    (pre-compiler-local-include
     [(HASH INCLUDE local-header) (list 'include-local $3)])

    (local-header
     [(QUOTE header-filename QUOTE) $2])

    (header-filename
     [(IDENTIFIER DOT header-extension) (string-append $1 "." $3)])

    (header-extension
     [("h") "h"]
     [("hpp") "hpp"]
     [("hh") "hh"])


    ;; ------------- 7. Using directive ---------------------------
    ;; scope-resolution
    [scope-resolution
     [ (SCOPE-RESOLUTION) '::] ]

    ;; namespace-name (recursiva)
    [namespace-name
     [ (IDENTIFIER) $1 ]
     [ (IDENTIFIER scope-resolution namespace-name) (list $1 ':: $3)] ]

    ;; using-directive
    [using-directive
     [ (USING namespace-name TERMINATOR) (list 'using $2)] ]
    
    ;; Sentencias múltiples (recursivo)
    [statements
     [() '()] ; caso base
     [(statement statements) (cons $1 $2)]]

    ;; Sentencias posibles
    [statement
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
     ;;  --------------------------------- 12. RETURN STATEMENT -------------------------------------------------
     [(return-statement)
      (list $1)]]
     
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
     [(OR)  'or]]
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
     [(IDENTIFIER LPAREN argument-list RPAREN) (list $1 $3)]]
    ;; BREAK STATEMENT
    [break-statement
     [(BREAK TERMINATOR) (list 'break)]]
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
    ]))
