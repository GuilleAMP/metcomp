#lang racket
(require "lexer.rkt" "parser.rkt")

;; Escapa HTML y corrige "<" con ">"
(define (html-escape str)
  (let* ([step1 (regexp-replace* #rx"&" str "&amp;")]
         [step2 (regexp-replace* #rx"<" step1 "&lt;>")]
         [step3 (regexp-replace* #rx">" step2 "&gt;")])
    step3))

;; Analiza archivo fuente
(define (analyze-file path)
  (define source-code (file->string path))
  (define tokens (tokenize source-code))
  (parse-html tokens))

;; Convierte AST a HTML
(define (ast->html ast)
  (define (flatten lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst)))]
      [else (cons (car lst) (flatten (cdr lst)))]))

  (define (render-token token)
    (define s (format "~a" token))
    (cond
      [(member s '("class" "struct" "for" "while" "return" "include" "using"
                   "break" "default" "else" "if" "public" "private" "protected"
                   "define" "ifdef" "ifndef" "endif" "elif" "pragma" "typename" "template" "function"))
       (format "<span class='keyword'>~a</span>" s)]
      [(member s '("int" "float" "double" "char" "bool" "void"))
       (format "<span class='type'>~a</span>" s)]
      [(member s '("true" "false"))
       (format "<span class='boolean'>~a</span>" s)]
      [(member s '("+" "-" "*" "/" "=" "==" "!=" "<" "<=" ">" ">=" "&&" "||" "%" "<<"
                   ">>" "++" "--" "::" "!"))
       (format "<span class='operator'>~a</span>" s)]
      [(regexp-match #px"^[0-9]+(\\.[0-9]+)?$" s)
       (format "<span class='literal'>~a</span>" s)]
      [(regexp-match #px"^\".*\"$" s)
       (format "<span class='literal'>~a</span>" s)]
      [(member s '("(" ")" "{" "}" ";" ":" "," "." "#" "\""))
       (format "<span class='punctuation'>~a</span>" s)]
      [else (format "<span class='identifier'>~a</span>" s)]))

  (define (render-line item)
    (cond
      [(list? item)
       (string-append "<div>"
                      (string-join (map render-token (flatten item)) " ")
                      "</div>")]
      [else (string-append "<div>" (render-token item) "</div>")]))

  (cond
    [(list? ast) (apply string-append (map render-line ast))]
    [else (string-append "<div>" (render-token ast) "</div>")]))

;; Genera HTML
(define (generate-html-file ast source-code output-path error-msg)
  (define html-content
    (string-append
     "<!DOCTYPE html>\n<html>\n<head><meta charset=\"UTF-8\"><title>AST</title>\n"
     "<style>
        body {
          font-family: 'Fira Code', monospace;
          background: #f4f4f4;
          color: #222;
          padding: 40px;
          line-height: 1.6;
        }
        h1, h2 {
          color: #333;
        }
        .section {
          margin-bottom: 40px;
          background: white;
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        }
        pre {
          background: #272822;
          color: #f8f8f2;
          padding: 20px;
          border-radius: 5px;
          overflow-x: auto;
        }
        pre.error {
          color: red;
          background: #ffe6e6;
          border: 1px solid #cc0000;
        }
        .keyword     { color: #268bd2; font-weight: bold; }
        .type        { color: #b58900; font-weight: bold; }
        .boolean     { color: #6c71c4; }
        .operator    { color: #cb4b16; }
        .literal     { color: #dc322f; }
        .identifier  { color: #2aa198; }
        .punctuation { color: #93a1a1; }
      </style>\n</head>\n<body>\n"

     "<h1>Resultado del análisis</h1>\n"

     ;; Vista 1
     "<div class='section'>\n<h2>1. Código fuente original</h2>\n"
     "<pre class='" (if error-msg "error" "") "'>"
     (html-escape source-code)
     "</pre>\n</div>\n"

     ;; Vista 2
     "<div class='section'>\n<h2>2. Análisis estructurado (AST)</h2>\n"
     (if error-msg
         (string-append "<div style='color:red; font-weight:bold;'>Error de análisis: "
                        (html-escape error-msg)
                        "</div>")
         (ast->html ast))
     "\n</div>\n"

     "</body></html>"))

  (call-with-output-file output-path
    (lambda (out)
      (display html-content out))
    #:exists 'replace))

;; Main
(module+ main
  (define input-file "source100.cpp")
  (define output-html "output.html")
  (define source-code (file->string input-file))

  (define-values (ast error-msg)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (values #f (exn-message e)))])
      (values (parse-html (tokenize source-code)) #f)))

  (generate-html-file ast source-code output-html error-msg)
  (printf "Archivo HTML generado: ~a\n" output-html))
