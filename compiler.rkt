;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBALS AND IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide (all-defined-out)) 
(require racket/match)
(require "mips.rkt")
(define-struct token (kind lexeme) #:transparent)

(define INT 0)
(define INT* 1)
(define VOID -1)
(define master (make-hash))
(define action "")
(define first-int 0)
(define second-int 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-error type msg)
   (fprintf (current-error-port) (string-append type " Error: " msg "\n"))
   (exit))

(define (show-error-basic msg)
   (fprintf (current-error-port) (string-append "Error: " msg "\n"))
   (exit))

(define (read-all-input)
(local ((define line (read-line)))
    (if (eof-object? line) empty
    (if (equal? line "") (read-all-input) 
        (cons line (read-all-input))))))

(define (read-all-input-split)
    (local ((define line (read-line)))
      (if (eof-object? line) empty
        (cons (string-split line) (read-all-input)))))

(define (print-lines ls)
    (if (empty? ls) (void)
        (begin
            (displayln (first ls))
            (print-lines (rest ls)))))

(define (print-tokens ls)
    (if (empty? ls) (void)
        (begin
            (displayln (string-append (symbol->string (token-kind (first ls))) " " (token-lexeme (first ls))))
            (print-tokens (rest ls)))))

(define (set-action inputs)
    (if (empty? inputs) (show-error "Scan" "Input file is empty")
        (set-action-not-empty inputs)))

(define (set-action-not-empty inputs)
    (define potential (string-split (first inputs)))
    (define error-msg "Compilation action (first line) does not follow proper format")

    (if (and (or (equal? (length potential) 1) (equal? (length potential) 2) (equal? (length potential) 3))
            (> (string-length (first potential)) 0)
            (equal? (substring (first potential) 0 1) "#"))
            
            (begin
                (set! action (first potential))
                (if (equal? (length potential) 2)
                    (if (number? (string->number (second potential)))
                        (set! first-int (string->number (second potential)))
                    (show-error "Scan" error-msg))
                    (void))
                (if (equal? (length potential) 3)
                    (if (and (number? (string->number (second potential)))
                            (number? (string->number (third potential))))
                    (begin
                        (set! first-int (string->number (second potential)))
                        (set! second-int (string->number (third potential))))
                    (show-error "Scan" error-msg))
                    (void)))
            (show-error "Scan" error-msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 1: SCANNER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct DFA (start accepting transition))

(define asm-kinds
  (set 'ID 'NUM 'LPAREN 'RPAREN 'LBRACE 'RBRACE 'LBRACK 'RBRACK 'BECOMES 'EQ 'NE 'LT 'GT 'LE 'GE 'PLUS 'MINUS 'STAR 'SLASH 'PCT 'COMMA 'SEMI 'AMP 'COMMENT))

(define (scan input)
  (define (token-postprocess tok)
    (match tok
      [(token 'DOTID ".word") (make-token 'WORD ".word")]
      [(token 'DOTID _)       (raise-user-error 'ERROR "Unrecognized DOTID token lexeme.")]
      [(token 'ZERO  lex)     (make-token 'INT lex)]
      [_                       tok]))

  (let* [(tokens (simplified-maximal-munch asm-dfa (string->list input)))
         (fixed-tokens
           (filter (lambda (x) (set-member? asm-kinds (token-kind x)))
                   tokens))]
    fixed-tokens))  

(define asm-dfa
  (make-DFA
    'START
    (set-union (set 'WHITESPACE 'COMMENT) asm-kinds)
    (lambda (state letter)
      (match (cons state letter)
        [(not (cons _ (? char?)))                                #f]
        [(cons 'START (? char-alphabetic?))                     'ID]
        [(cons 'ID (or (? char-alphabetic?)
                        (? char-numeric?)))                     'ID]
        [(cons 'START (? char-numeric?))                        'NUM]
        [(cons 'NUM (? char-numeric?))                          'NUM]
        [(cons 'START #\()                                      'LPAREN]
        [(cons 'START #\))                                      'RPAREN]
        [(cons 'START #\{)                                      'LBRACE]
        [(cons 'START #\})                                      'RBRACE]
        [(cons 'START #\[)                                      'LBRACK]
        [(cons 'START #\])                                      'RBRACK]
        [(cons 'START #\=)                                      'BECOMES]
        [(cons 'BECOMES #\=)                                    'EQ]
        [(cons 'START #\!)                                      'EX]
        [(cons 'EX #\=)                                       'NE]
        [(cons 'START #\<)                                      'LT]
        [(cons 'LT #\=)                                         'LE]
        [(cons 'START #\>)                                      'GT]
        [(cons 'GT #\=)                                         'GE]
        [(cons 'START #\+)                                      'PLUS]
        [(cons 'START #\-)                                      'MINUS]
        [(cons 'START #\*)                                      'STAR]
        [(cons 'START #\/)                                      'SLASH]
        [(cons 'SLASH #\/)                                      'COMMENT]
        [(cons 'COMMENT (? char?))                                      'COMMENT]
        [(cons 'START #\%)                                      'PCT]
        [(cons 'START #\,)                                      'COMMA]
        [(cons 'START #\;)                                      'SEMI]
        [(cons 'START #\&)                                      'AMP]
        [(cons 'START (? char-whitespace?))                     'WHITESPACE]
        [(cons 'WHITESPACE (? char-whitespace?))                'WHITESPACE]
        [(cons 'START #\=)                                      'EQ]
        [_                                                       #f]))))

(define (simplified-maximal-munch dfa input)
  (define (scan-one input state consumed-input)
    (let [(transition ((DFA-transition dfa)
                       state
                       (if (empty? input) null (car input))))]
      (cond [(and (not transition) (set-member? (DFA-accepting dfa) state))
             (cons input (make-token state
                                     (list->string (reverse consumed-input))))]
            [(not transition)
             (raise-user-error 'ERROR
                    (string-append
                      "Simplified maximal munch failed on input: "
                      (string-append (list->string (reverse consumed-input))
                                     (list->string input))))]
            [else (scan-one (cdr input) transition
                            (cons (car input) consumed-input))])))

  (define (scan-all input accumulator)
    (if (empty? input)
      (reverse accumulator)
      (let [(result (scan-one input (DFA-start dfa) null))]
        (scan-all (car result) (cons (cdr result) accumulator)))))

  (scan-all input null))

(define (clean-tokens tokens)
  (if (or (empty? tokens) (equal? (token-kind (first tokens)) 'COMMENT)) empty
    (cons (first tokens) (clean-tokens (rest tokens)))))

(define (get-tokens inputs)
  (if (empty? inputs) empty
    (append (clean-tokens (scan (first inputs))) (get-tokens (rest inputs)))))

(define (simplify-tokens tokens)
  (if (empty? tokens) empty
    (cons 
      (if (equal? (token-kind (first tokens)) 'ID) (token (next-keyword (first tokens)) (token-lexeme (first tokens)))
      (if (equal? (token-kind (first tokens)) 'NUM) (check-num-limit (first tokens))
      (first tokens)))
      (simplify-tokens (rest tokens)))))

(define (check-num-limit token)
  (define lexeme (token-lexeme token))
  (define number (string->number lexeme))
  (if (and (> (string-length lexeme) 1) (equal? (substring lexeme 0 1) "0")) (show-error "Scan" (string-append "Numeric Token " lexeme " has Leading 0"))
  (if (<= number 2147483647) token (show-error "Scan" (string-append "Numeric Token " number " Exceeds Limit of 2^31")))))

(define (next-keyword token)
  (define lexeme (token-lexeme token))
  (if (equal? lexeme "return") 'RETURN
  (if (equal? lexeme "if") 'IF
  (if (equal? lexeme "else") 'ELSE
  (if (equal? lexeme "while") 'WHILE
  (if (equal? lexeme "println") 'PRINTLN
  (if (equal? lexeme "main") 'MAIN
  (if (equal? lexeme "int") 'INT
  (if (equal? lexeme "new") 'NEW
  (if (equal? lexeme "delete") 'DELETE
  (if (equal? lexeme "NULL") 'NULL 'ID)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 2: PARSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reduced-productions (list))
(define parse-tree (list))
(define seen-input 0)
(define productions (make-hash))
(define reduce (make-hash))
(define shift (make-hash))
(define state-stack (list 0))
(define symbol-stack (list))
(define alphabet-hash (make-hash))
(define parsed (list))

(define (make-productions ls index)
    (if (empty? ls) (void)
    (begin
        (hash-set! productions index (list (first (first ls)) (second (first ls))))
        (make-productions (rest ls) (add1 index)))))

(define (add-reduce i)
    (define temp-hash (if (hash-has-key? reduce (first i)) (hash-ref reduce (first i)) (make-hash)))
    (hash-set! temp-hash (second i) (fourth i))
    (hash-set! reduce (first i) temp-hash))

(define (add-shift i)
    (define temp-hash (if (hash-has-key? shift (first i)) (hash-ref shift (first i)) (make-hash)))
    (hash-set! temp-hash (second i) (fourth i))
    (hash-set! shift (first i) temp-hash))

(define alphabet (list "AMP" "BECOMES" "BOF" "COMMA" "DELETE" "ELSE" "EOF" "EQ" "GE" "GT" "ID" "IF" "INT" "LBRACE" "LBRACK" "LE" "LPAREN" "LT" "MINUS" "NE" "NEW" "NULL" "NUM" "PCT" "PLUS" "PRINTLN" "RBRACE" "RBRACK" "RETURN" "RPAREN" "SEMI" "SLASH" "STAR" "MAIN" "WHILE"))
(define non-terminals (list "start" "dcl" "dcls" "expr" "factor" "lvalue" "procedure" "procedures" "main" "params" "paramlist" "statement" "statements" "term" "test" "type" "arglist"))
(define start-state "start")
(define production-list (list (list "start" (list "BOF" "procedures" "EOF"))
                            (list "procedures" (list "procedure" "procedures"))
                            (list "procedures" (list "main")) 
                            (list "procedure" (list "INT" "ID" "LPAREN" "params" "RPAREN" "LBRACE" "dcls" "statements" "RETURN" "expr" "SEMI" "RBRACE")) 
                            (list "main" (list "INT" "MAIN" "LPAREN" "dcl" "COMMA" "dcl" "RPAREN" "LBRACE" "dcls" "statements" "RETURN" "expr" "SEMI" "RBRACE")) 
                            (list "params" (list)) 
                            (list "params" (list "paramlist")) 
                            (list "paramlist" (list "dcl")) 
                            (list "paramlist" (list "dcl" "COMMA" "paramlist")) 
                            (list "type" (list "INT")) 
                            (list "type" (list "INT" "STAR")) 
                            (list "dcls" (list)) 
                            (list "dcls" (list "dcls" "dcl" "BECOMES" "NUM" "SEMI")) 
                            (list "dcls" (list "dcls" "dcl" "BECOMES" "NULL" "SEMI")) 
                            (list "dcl" (list "type" "ID")) 
                            (list "statements" (list)) 
                            (list "statements" (list "statements" "statement")) 
                            (list "statement" (list "lvalue" "BECOMES" "expr" "SEMI")) 
                            (list "statement" (list "IF" "LPAREN" "test" "RPAREN" "LBRACE" "statements" "RBRACE" "ELSE" "LBRACE" "statements" "RBRACE")) 
                            (list "statement" (list "WHILE" "LPAREN" "test" "RPAREN" "LBRACE" "statements" "RBRACE")) 
                            (list "statement" (list "PRINTLN" "LPAREN" "expr" "RPAREN" "SEMI")) 
                            (list "statement" (list "DELETE" "LBRACK" "RBRACK" "expr" "SEMI")) 
                            (list "test" (list "expr" "EQ" "expr")) 
                            (list "test" (list "expr" "NE" "expr")) 
                            (list "test" (list "expr" "LT" "expr")) 
                            (list "test" (list "expr" "LE" "expr")) 
                            (list "test" (list "expr" "GE" "expr")) 
                            (list "test" (list "expr" "GT" "expr")) 
                            (list "expr" (list "term")) 
                            (list "expr" (list "expr" "PLUS" "term")) 
                            (list "expr" (list "expr" "MINUS" "term")) 
                            (list "term" (list "factor")) 
                            (list "term" (list "term" "STAR" "factor")) 
                            (list "term" (list "term" "SLASH" "factor")) 
                            (list "term" (list "term" "PCT" "factor")) 
                            (list "factor" (list "ID")) 
                            (list "factor" (list "NUM")) 
                            (list "factor" (list "NULL")) 
                            (list "factor" (list "LPAREN" "expr" "RPAREN")) 
                            (list "factor" (list "AMP" "lvalue")) 
                            (list "factor" (list "STAR" "factor")) 
                            (list "factor" (list "NEW" "INT" "LBRACK" "expr" "RBRACK")) 
                            (list "factor" (list "ID" "LPAREN" "RPAREN")) 
                            (list "factor" (list "ID" "LPAREN" "arglist" "RPAREN")) 
                            (list "arglist" (list "expr")) 
                            (list "arglist" (list "expr" "COMMA" "arglist")) 
                            (list "lvalue" (list "ID")) 
                            (list "lvalue" (list "STAR" "factor")) 
                            (list "lvalue" (list "LPAREN" "lvalue" "RPAREN"))))
(define dfa-state-count 132)
(define dfa (list (list 72 "RPAREN" "reduce" 45) (list 112 "paramlist" "shift" 1) (list 85 "LPAREN" "shift" 2) (list 40 "factor" "shift" 3) (list 3 "EQ" "reduce" 47) (list 74 "EQ" "reduce" 46) (list 21 "INT" "shift" 4) (list 100 "EQ" "reduce" 48) (list 106 "STAR" "shift" 5) (list 86 "STAR" "shift" 5) (list 88 "STAR" "shift" 5) (list 95 "SEMI" "shift" 6) (list 51 "SEMI" "shift" 7) (list 118 "NULL" "shift" 8) (list 128 "STAR" "reduce" 11) (list 2 "NULL" "shift" 8) (list 108 "GT" "shift" 9) (list 57 "RETURN" "reduce" 16) (list 6 "INT" "reduce" 13) (list 7 "INT" "reduce" 12) (list 19 "factor" "shift" 10) (list 71 "factor" "shift" 10) (list 14 "term" "shift" 11) (list 44 "term" "shift" 11) (list 17 "term" "shift" 11) (list 25 "term" "shift" 11) (list 13 "term" "shift" 11) (list 9 "term" "shift" 11) (list 117 "BECOMES" "reduce" 14) (list 94 "LPAREN" "reduce" 11) (list 100 "GT" "reduce" 48) (list 3 "GT" "reduce" 47) (list 74 "GT" "reduce" 46) (list 85 "expr" "shift" 12) (list 108 "GE" "shift" 13) (list 74 "GE" "reduce" 46) (list 3 "GE" "reduce" 47) (list 100 "GE" "reduce" 48) (list 128 "PRINTLN" "reduce" 11) (list 108 "EQ" "shift" 14) (list 2 "STAR" "shift" 5) (list 118 "STAR" "shift" 5) (list 106 "NEW" "shift" 15) (list 86 "NEW" "shift" 15) (list 88 "NEW" "shift" 15) (list 123 "RPAREN" "shift" 16) (list 108 "LT" "shift" 17) (list 23 "procedure" "shift" 18) (list 70 "term" "shift" 11) (list 31 "PLUS" "shift" 19) (list 32 "PLUS" "shift" 19) (list 33 "PLUS" "shift" 19) (list 34 "PLUS" "shift" 19) (list 35 "PLUS" "shift" 19) (list 36 "PLUS" "shift" 19) (list 94 "WHILE" "reduce" 11) (list 29 "COMMA" "shift" 20) (list 94 "dcls" "shift" 21) (list 70 "ID" "shift" 22) (list 0 "BOF" "shift" 23) (list 5 "NULL" "shift" 8) (list 56 "LPAREN" "shift" 24) (list 108 "LE" "shift" 25) (list 101 "COMMA" "shift" 26) (list 121 "SEMI" "shift" 27) (list 83 "DELETE" "reduce" 18) (list 59 "RETURN" "reduce" 15) (list 57 "WHILE" "reduce" 16) (list 18 "main" "shift" 28) (list 82 "DELETE" "reduce" 19) (list 127 "STAR" "reduce" 39) (list 102 "STAR" "reduce" 40) (list 74 "LE" "reduce" 46) (list 30 "STAR" "reduce" 36) (list 8 "STAR" "reduce" 37) (list 112 "dcl" "shift" 29) (list 90 "STAR" "reduce" 43) (list 93 "STAR" "reduce" 42) (list 97 "STAR" "reduce" 38) (list 116 "STAR" "reduce" 41) (list 73 "NUM" "shift" 30) (list 14 "expr" "shift" 31) (list 44 "expr" "shift" 32) (list 17 "expr" "shift" 33) (list 25 "expr" "shift" 34) (list 13 "expr" "shift" 35) (list 9 "expr" "shift" 36) (list 37 "EOF" "reduce" 4) (list 87 "RBRACE" "shift" 37) (list 26 "INT" "shift" 4) (list 24 "INT" "shift" 4) (list 5 "STAR" "shift" 5) (list 59 "STAR" "reduce" 15) (list 130 "DELETE" "reduce" 17) (list 68 "DELETE" "reduce" 20) (list 131 "DELETE" "reduce" 21) (list 59 "WHILE" "reduce" 15) (list 11 "LT" "reduce" 28) (list 61 "LT" "reduce" 30) (list 60 "LT" "reduce" 29) (list 129 "LPAREN" "shift" 38) (list 121 "PLUS" "shift" 19) (list 46 "NEW" "shift" 15) (list 52 "NEW" "shift" 15) (list 66 "NEW" "shift" 15) (list 4 "ID" "reduce" 9) (list 38 "LPAREN" "shift" 38) (list 85 "STAR" "shift" 5) (list 18 "procedures" "shift" 39) (list 85 "NUM" "shift" 30) (list 40 "NULL" "shift" 8) (list 61 "RPAREN" "reduce" 30) (list 60 "RPAREN" "reduce" 29) (list 11 "RPAREN" "reduce" 28) (list 59 "ID" "reduce" 15) (list 59 "IF" "reduce" 15) (list 57 "RBRACE" "reduce" 16) (list 61 "NE" "reduce" 30) (list 60 "NE" "reduce" 29) (list 11 "NE" "reduce" 28) (list 77 "IF" "reduce" 15) (list 76 "IF" "reduce" 15) (list 115 "STAR" "shift" 40) (list 78 "ID" "reduce" 15) (list 78 "IF" "reduce" 15) (list 74 "RBRACK" "reduce" 46) (list 3 "RBRACK" "reduce" 47) (list 100 "RBRACK" "reduce" 48) (list 128 "LPAREN" "reduce" 11) (list 77 "ID" "reduce" 15) (list 76 "ID" "reduce" 15) (list 15 "INT" "shift" 41) (list 113 "STAR" "shift" 40) (list 114 "STAR" "shift" 40) (list 20 "type" "shift" 42) (list 22 "RPAREN" "reduce" 35) (list 24 "type" "shift" 42) (list 26 "type" "shift" 42) (list 129 "WHILE" "shift" 43) (list 108 "NE" "shift" 44) (list 11 "LE" "reduce" 28) (list 70 "factor" "shift" 10) (list 105 "NEW" "shift" 15) (list 104 "NEW" "shift" 15) (list 80 "term" "shift" 11) (list 80 "expr" "shift" 12) (list 113 "LPAREN" "shift" 38) (list 114 "LPAREN" "shift" 38) (list 117 "RPAREN" "reduce" 14) (list 115 "LPAREN" "shift" 38) (list 61 "LE" "reduce" 30) (list 60 "LE" "reduce" 29) (list 63 "BECOMES" "shift" 45) (list 90 "PCT" "reduce" 43) (list 116 "PCT" "reduce" 41) (list 30 "PCT" "reduce" 36) (list 8 "PCT" "reduce" 37) (list 127 "PCT" "reduce" 39) (list 102 "PCT" "reduce" 40) (list 93 "PCT" "reduce" 42) (list 97 "PCT" "reduce" 38) (list 47 "MINUS" "reduce" 32) (list 48 "MINUS" "reduce" 33) (list 49 "MINUS" "reduce" 34) (list 10 "MINUS" "reduce" 31) (list 10 "STAR" "reduce" 31) (list 11 "STAR" "shift" 46) (list 61 "STAR" "shift" 46) (list 60 "STAR" "shift" 46) (list 70 "LPAREN" "shift" 2) (list 46 "factor" "shift" 47) (list 52 "factor" "shift" 48) (list 66 "factor" "shift" 49) (list 14 "STAR" "shift" 5) (list 44 "STAR" "shift" 5) (list 17 "STAR" "shift" 5) (list 25 "STAR" "shift" 5) (list 13 "STAR" "shift" 5) (list 9 "STAR" "shift" 5) (list 80 "arglist" "shift" 50) (list 12 "RPAREN" "reduce" 44) (list 19 "NEW" "shift" 15) (list 71 "NEW" "shift" 15) (list 45 "NUM" "shift" 51) (list 61 "GE" "reduce" 30) (list 60 "GE" "reduce" 29) (list 11 "GE" "reduce" 28) (list 10 "RBRACK" "reduce" 31) (list 47 "RBRACK" "reduce" 32) (list 48 "RBRACK" "reduce" 33) (list 49 "RBRACK" "reduce" 34) (list 90 "COMMA" "reduce" 43) (list 116 "COMMA" "reduce" 41) (list 61 "GT" "reduce" 30) (list 60 "GT" "reduce" 29) (list 11 "SLASH" "shift" 52) (list 11 "GT" "reduce" 28) (list 61 "SLASH" "shift" 52) (list 60 "SLASH" "shift" 52) (list 30 "COMMA" "reduce" 36) (list 8 "COMMA" "reduce" 37) (list 127 "COMMA" "reduce" 39) (list 102 "COMMA" "reduce" 40) (list 47 "STAR" "reduce" 32) (list 48 "STAR" "reduce" 33) (list 49 "STAR" "reduce" 34) (list 80 "ID" "shift" 22) (list 93 "COMMA" "reduce" 42) (list 97 "COMMA" "reduce" 38) (list 70 "NULL" "shift" 8) (list 59 "RBRACE" "reduce" 15) (list 46 "AMP" "shift" 53) (list 52 "AMP" "shift" 53) (list 66 "AMP" "shift" 53) (list 23 "main" "shift" 28) (list 94 "ID" "reduce" 11) (list 78 "RBRACE" "reduce" 15) (list 94 "IF" "reduce" 11) (list 94 "PRINTLN" "reduce" 11) (list 23 "INT" "shift" 54) (list 5 "ID" "shift" 22) (list 46 "STAR" "shift" 5) (list 52 "STAR" "shift" 5) (list 66 "STAR" "shift" 5) (list 113 "DELETE" "shift" 55) (list 114 "DELETE" "shift" 55) (list 116 "SLASH" "reduce" 41) (list 129 "STAR" "shift" 40) (list 90 "SLASH" "reduce" 43) (list 112 "RPAREN" "reduce" 5) (list 54 "MAIN" "shift" 56) (list 127 "SLASH" "reduce" 39) (list 102 "SLASH" "reduce" 40) (list 93 "SLASH" "reduce" 42) (list 97 "SLASH" "reduce" 38) (list 30 "SLASH" "reduce" 36) (list 8 "SLASH" "reduce" 37) (list 19 "STAR" "shift" 5) (list 71 "STAR" "shift" 5) (list 118 "NUM" "shift" 30) (list 109 "statement" "shift" 57) (list 2 "NUM" "shift" 30) (list 73 "LPAREN" "shift" 2) (list 53 "LPAREN" "shift" 38) (list 47 "COMMA" "reduce" 32) (list 48 "COMMA" "reduce" 33) (list 49 "COMMA" "reduce" 34) (list 14 "factor" "shift" 10) (list 44 "factor" "shift" 10) (list 17 "factor" "shift" 10) (list 25 "factor" "shift" 10) (list 13 "factor" "shift" 10) (list 9 "factor" "shift" 10) (list 10 "COMMA" "reduce" 31) (list 100 "STAR" "reduce" 48) (list 3 "STAR" "reduce" 47) (list 74 "STAR" "reduce" 46) (list 115 "DELETE" "shift" 55) (list 109 "PRINTLN" "shift" 58) (list 78 "WHILE" "reduce" 15) (list 77 "RBRACE" "reduce" 15) (list 76 "RBRACE" "reduce" 15) (list 113 "statement" "shift" 57) (list 114 "statement" "shift" 57) (list 22 "PLUS" "reduce" 35) (list 115 "statement" "shift" 57) (list 105 "AMP" "shift" 53) (list 104 "AMP" "shift" 53) (list 77 "WHILE" "reduce" 15) (list 76 "WHILE" "reduce" 15) (list 128 "dcls" "shift" 59) (list 118 "term" "shift" 11) (list 2 "term" "shift" 11) (list 19 "term" "shift" 60) (list 71 "term" "shift" 61) (list 128 "WHILE" "reduce" 11) (list 19 "LPAREN" "shift" 2) (list 71 "LPAREN" "shift" 2) (list 128 "RETURN" "reduce" 11) (list 57 "ID" "reduce" 16) (list 57 "IF" "reduce" 16) (list 23 "procedures" "shift" 62) (list 2 "LPAREN" "shift" 2) (list 118 "LPAREN" "shift" 2) (list 21 "dcl" "shift" 63) (list 19 "NULL" "shift" 8) (list 71 "NULL" "shift" 8) (list 78 "PRINTLN" "reduce" 15) (list 77 "PRINTLN" "reduce" 15) (list 76 "PRINTLN" "reduce" 15) (list 2 "expr" "shift" 64) (list 118 "expr" "shift" 65) (list 11 "PCT" "shift" 66) (list 21 "ID" "reduce" 15) (list 21 "IF" "reduce" 15) (list 6 "LPAREN" "reduce" 13) (list 7 "LPAREN" "reduce" 12) (list 113 "lvalue" "shift" 67) (list 114 "lvalue" "shift" 67) (list 14 "NULL" "shift" 8) (list 44 "NULL" "shift" 8) (list 17 "NULL" "shift" 8) (list 25 "NULL" "shift" 8) (list 13 "NULL" "shift" 8) (list 9 "NULL" "shift" 8) (list 118 "AMP" "shift" 53) (list 115 "lvalue" "shift" 67) (list 2 "AMP" "shift" 53) (list 16 "SEMI" "shift" 68) (list 80 "factor" "shift" 10) (list 94 "INT" "reduce" 11) (list 5 "NEW" "shift" 15) (list 73 "STAR" "shift" 5) (list 57 "PRINTLN" "reduce" 16) (list 80 "NEW" "shift" 15) (list 74 "LT" "reduce" 46) (list 3 "LT" "reduce" 47) (list 46 "NUM" "shift" 30) (list 52 "NUM" "shift" 30) (list 66 "NUM" "shift" 30) (list 10 "PCT" "reduce" 31) (list 3 "LE" "reduce" 47) (list 100 "LE" "reduce" 48) (list 85 "NEW" "shift" 15) (list 21 "WHILE" "reduce" 15) (list 47 "PCT" "reduce" 32) (list 48 "PCT" "reduce" 33) (list 49 "PCT" "reduce" 34) (list 100 "LT" "reduce" 48) (list 21 "RBRACE" "reduce" 15) (list 22 "COMMA" "reduce" 35) (list 129 "PRINTLN" "shift" 58) (list 22 "SEMI" "reduce" 35) (list 100 "NE" "reduce" 48) (list 3 "NE" "reduce" 47) (list 74 "NE" "reduce" 46) (list 40 "AMP" "shift" 53) (list 14 "ID" "shift" 22) (list 44 "ID" "shift" 22) (list 17 "ID" "shift" 22) (list 25 "ID" "shift" 22) (list 13 "ID" "shift" 22) (list 9 "ID" "shift" 22) (list 94 "RETURN" "reduce" 11) (list 46 "NULL" "shift" 8) (list 52 "NULL" "shift" 8) (list 66 "NULL" "shift" 8) (list 19 "AMP" "shift" 53) 
    (list 71 "AMP" "shift" 53) (list 118 "ID" "shift" 22) (list 2 "ID" "shift" 22) (list 54 "ID" "shift" 69) (list 80 "NULL" "shift" 8) (list 129 "RETURN" "shift" 70) (list 61 "PLUS" "reduce" 30) (list 60 "PLUS" "reduce" 29) (list 11 "PLUS" "reduce" 28) (list 59 "DELETE" "reduce" 15) (list 22 "PCT" "reduce" 35) (list 31 "MINUS" "shift" 71) (list 32 "MINUS" "shift" 71) (list 33 "MINUS" "shift" 71) (list 34 "MINUS" "shift" 71) (list 35 "MINUS" "shift" 71) (list 36 "MINUS" "shift" 71) (list 105 "NUM" "shift" 30) (list 104 "NUM" "shift" 30) (list 85 "arglist" "shift" 72) (list 22 "RBRACK" "reduce" 35) (list 6 "DELETE" "reduce" 13) (list 7 "DELETE" "reduce" 12) (list 109 "RETURN" "shift" 73) (list 88 "LPAREN" "shift" 2) (list 12 "PLUS" "shift" 19) (list 53 "ID" "shift" 74) (list 105 "LPAREN" "shift" 2) (list 104 "LPAREN" "shift" 2) (list 106 "LPAREN" "shift" 2) (list 86 "LPAREN" "shift" 2) (list 22 "MINUS" "reduce" 35) (list 90 "BECOMES" "reduce" 43) (list 93 "BECOMES" "reduce" 42) (list 97 "BECOMES" "reduce" 38) (list 127 "BECOMES" "reduce" 39) (list 102 "BECOMES" "reduce" 40) (list 30 "BECOMES" "reduce" 36) (list 8 "BECOMES" "reduce" 37) (list 70 "NUM" "shift" 30) (list 116 "BECOMES" "reduce" 41) (list 19 "NUM" "shift" 30) (list 71 "NUM" "shift" 30) (list 73 "expr" "shift" 75) (list 94 "STAR" "reduce" 11) (list 126 "LBRACE" "shift" 76) (list 125 "LBRACE" "shift" 77) (list 100 "SLASH" "reduce" 48) (list 3 "SLASH" "reduce" 47) (list 96 "LBRACE" "shift" 78) (list 74 "SLASH" "reduce" 46) (list 55 "LBRACK" "shift" 79) (list 40 "STAR" "shift" 5) (list 128 "INT" "reduce" 11) (list 2 "NEW" "shift" 15) (list 22 "LPAREN" "shift" 80) (list 127 "NE" "reduce" 39) (list 102 "NE" "reduce" 40) (list 93 "NE" "reduce" 42) (list 97 "NE" "reduce" 38) (list 90 "NE" "reduce" 43) (list 116 "NE" "reduce" 41) (list 116 "MINUS" "reduce" 41) (list 38 "STAR" "shift" 40) (list 78 "RETURN" "reduce" 15) (list 112 "INT" "shift" 4) (list 30 "NE" "reduce" 36) (list 8 "NE" "reduce" 37) (list 30 "MINUS" "reduce" 36) (list 8 "MINUS" "reduce" 37) (list 118 "NEW" "shift" 15) (list 127 "MINUS" "reduce" 39) (list 102 "MINUS" "reduce" 40) (list 93 "MINUS" "reduce" 42) (list 97 "MINUS" "reduce" 38) (list 90 "MINUS" "reduce" 43) (list 77 "RETURN" "reduce" 15) (list 76 "RETURN" "reduce" 15) (list 39 "EOF" "reduce" 1) (list 6 "WHILE" "reduce" 13) (list 7 "WHILE" "reduce" 12) (list 28 "EOF" "reduce" 2) (list 59 "PRINTLN" "reduce" 15) (list 113 "RBRACE" "shift" 81) (list 114 "RBRACE" "shift" 82) (list 127 "RBRACK" "reduce" 39) (list 102 "RBRACK" "reduce" 40) (list 93 "RBRACK" "reduce" 42) (list 97 "RBRACK" "reduce" 38) (list 109 "lvalue" "shift" 67) (list 90 "RBRACK" "reduce" 43) (list 116 "RBRACK" "reduce" 41) (list 115 "RBRACE" "shift" 83) (list 30 "RBRACK" "reduce" 36) (list 8 "RBRACK" "reduce" 37) (list 21 "PRINTLN" "reduce" 15) (list 14 "LPAREN" "shift" 2) (list 44 "LPAREN" "shift" 2) (list 17 "LPAREN" "shift" 2) (list 25 "LPAREN" "shift" 2) (list 13 "LPAREN" "shift" 2) (list 9 "LPAREN" "shift" 2) (list 112 "params" "shift" 84) (list 12 "COMMA" "shift" 85) (list 75 "MINUS" "shift" 71) (list 22 "SLASH" "reduce" 35) (list 103 "RPAREN" "reduce" 8) (list 5 "NUM" "shift" 30) (list 67 "BECOMES" "shift" 86) (list 11 "RBRACK" "reduce" 28) (list 61 "COMMA" "reduce" 30) (list 60 "COMMA" "reduce" 29) (list 61 "RBRACK" "reduce" 30) (list 60 "RBRACK" "reduce" 29) (list 11 "COMMA" "reduce" 28) (list 74 "PCT" "reduce" 46) (list 3 "PCT" "reduce" 47) (list 100 "PCT" "reduce" 48) (list 75 "PLUS" "shift" 19) (list 5 "LPAREN" "shift" 2) (list 112 "type" "shift" 42) (list 85 "ID" "shift" 22) (list 3 "BECOMES" "reduce" 47) (list 74 "BECOMES" "reduce" 46) (list 100 "BECOMES" "reduce" 48) (list 85 "term" "shift" 11) (list 116 "SEMI" "reduce" 41) (list 64 "MINUS" "shift" 71) (list 93 "SEMI" "reduce" 42) (list 97 "SEMI" "reduce" 38) (list 65 "MINUS" "shift" 71) (list 90 "SEMI" "reduce" 43) (list 80 "LPAREN" "shift" 2) (list 75 "SEMI" "shift" 87) (list 130 "RBRACE" "reduce" 17) (list 68 "RBRACE" "reduce" 20) (list 131 "RBRACE" "reduce" 21) (list 14 "AMP" "shift" 53) (list 44 "AMP" "shift" 53) (list 17 "AMP" "shift" 53) (list 25 "AMP" "shift" 53) (list 13 "AMP" "shift" 53) (list 9 "AMP" "shift" 53) (list 82 "RBRACE" "reduce" 19) (list 79 "RBRACK" "shift" 88) (list 127 "SEMI" "reduce" 39) (list 102 "SEMI" "reduce" 40) (list 30 "SEMI" "reduce" 36) (list 8 "SEMI" "reduce" 37) (list 83 "RBRACE" "reduce" 18) (list 68 "PRINTLN" "reduce" 20) (list 131 "PRINTLN" "reduce" 21) (list 130 "PRINTLN" "reduce" 17) (list 47 "PLUS" "reduce" 32) (list 48 "PLUS" "reduce" 33) (list 49 "PLUS" "reduce" 34) (list 82 "PRINTLN" "reduce" 19) (list 10 "PLUS" "reduce" 31) (list 109 "WHILE" "shift" 43) (list 83 "PRINTLN" "reduce" 18) (list 65 "PLUS" "shift" 19) (list 64 "PLUS" "shift" 19) (list 77 "STAR" "reduce" 15) (list 76 "STAR" "reduce" 15) (list 10 "SEMI" "reduce" 31) (list 78 "STAR" "reduce" 15) (list 73 "NULL" "shift" 8) (list 46 "LPAREN" "shift" 2) (list 52 "LPAREN" "shift" 2) (list 66 "LPAREN" "shift" 2) (list 4 "STAR" "shift" 89) (list 47 "SEMI" "reduce" 32) (list 48 "SEMI" "reduce" 33) (list 49 "SEMI" "reduce" 34) (list 116 "RPAREN" "reduce" 41) (list 40 "NUM" "shift" 30) (list 30 "RPAREN" "reduce" 36) (list 8 "RPAREN" "reduce" 37) (list 127 "RPAREN" "reduce" 39) (list 102 "RPAREN" "reduce" 40) (list 50 "RPAREN" "shift" 90) (list 93 "RPAREN" "reduce" 42) (list 97 "RPAREN" "reduce" 38) (list 105 "NULL" "shift" 8) (list 104 "NULL" "shift" 8) (list 90 "RPAREN" "reduce" 43) (list 109 "DELETE" "shift" 55) (list 18 "procedure" "shift" 18) (list 100 "PLUS" "reduce" 48) (list 74 "PLUS" "reduce" 46) (list 3 "PLUS" "reduce" 47) (list 85 "factor" "shift" 10) (list 10 "NE" "reduce" 31) (list 47 "NE" "reduce" 32) (list 48 "NE" "reduce" 33) (list 49 "NE" "reduce" 34) (list 94 "DELETE" "reduce" 11) (list 116 "EQ" "reduce" 41) (list 100 "COMMA" "reduce" 48) (list 30 "GT" "reduce" 36) (list 8 "GT" "reduce" 37) (list 3 "COMMA" "reduce" 47) (list 93 "GT" "reduce" 42) (list 97 "GT" "reduce" 38) (list 127 "GT" "reduce" 39) (list 102 "GT" "reduce" 40) (list 1 "RPAREN" "reduce" 6) (list 74 "COMMA" "reduce" 46) (list 27 "RBRACE" "shift" 91) (list 90 "GE" "reduce" 43) (list 93 "GE" "reduce" 42) (list 97 "GE" "reduce" 38) (list 116 "GE" "reduce" 41) (list 83 "WHILE" "reduce" 18) (list 30 "GE" "reduce" 36) (list 8 "GE" "reduce" 37) (list 127 "GE" "reduce" 39) (list 102 "GE" "reduce" 40) (list 21 "LPAREN" "reduce" 15) (list 6 "STAR" "reduce" 13) (list 7 "STAR" "reduce" 12) (list 82 "WHILE" "reduce" 19) (list 68 "WHILE" "reduce" 20) (list 131 "WHILE" "reduce" 21) (list 130 "WHILE" "reduce" 17) (list 61 "PCT" "shift" 66) (list 60 "PCT" "shift" 66) (list 80 "NUM" "shift" 30) (list 20 "dcl" "shift" 29) (list 99 "RPAREN" "shift" 92) (list 128 "ID" "reduce" 11) (list 57 "LPAREN" "reduce" 16) (list 80 "RPAREN" "shift" 93) (list 92 "LBRACE" "shift" 94) (list 14 "NUM" "shift" 30) (list 44 "NUM" "shift" 30) (list 17 "NUM" "shift" 30) (list 25 "NUM" "shift" 30) (list 13 "NUM" "shift" 30) (list 9 "NUM" "shift" 30) (list 90 "GT" "reduce" 43) (list 116 "GT" "reduce" 41) (list 45 "NULL" "shift" 95) (list 128 "IF" "reduce" 11) (list 81 "ELSE" "shift" 96) (list 57 "DELETE" "reduce" 16) (list 64 "RPAREN" "shift" 97) (list 73 "AMP" "shift" 53) (list 62 "EOF" "shift" 98) (list 21 "STAR" "reduce" 15) (list 118 "factor" "shift" 10) (list 105 "factor" "shift" 10) (list 104 "factor" "shift" 10) (list 2 "factor" "shift" 10) (list 29 "RPAREN" "reduce" 7) (list 100 "RPAREN" "reduce" 48) (list 105 "ID" "shift" 22) (list 104 "ID" "shift" 22) (list 82 "LPAREN" "reduce" 19) (list 20 "INT" "shift" 4) (list 88 "factor" "shift" 10) (list 130 "LPAREN" "reduce" 17) (list 106 "factor" "shift" 10) (list 86 "factor" "shift" 10) (list 68 "LPAREN" "reduce" 20) (list 131 "LPAREN" "reduce" 21) (list 26 "dcl" "shift" 99) (list 19 "ID" "shift" 22) (list 71 "ID" "shift" 22) (list 74 "RPAREN" "reduce" 46) (list 111 "RPAREN" "shift" 100) (list 3 "RPAREN" "reduce" 47) (list 83 "LPAREN" "reduce" 18) (list 24 "dcl" "shift" 101) (list 5 "factor" "shift" 102) (list 20 "paramlist" "shift" 103) (list 107 "LPAREN" "shift" 104) (list 43 "LPAREN" "shift" 105) (list 58 "LPAREN" "shift" 106) (list 12 "MINUS" "shift" 71) (list 11 "MINUS" "reduce" 28) (list 61 "MINUS" "reduce" 30) (list 60 "MINUS" "reduce" 29) (list 59 "INT" "shift" 4) (list 93 "LT" "reduce" 42) (list 97 "LT" "reduce" 38) (list 90 "LT" "reduce" 43) (list 30 "LT" "reduce" 36) (list 8 "LT" "reduce" 37) (list 127 "LT" "reduce" 39) (list 102 "LT" "reduce" 40) (list 91 "INT" "reduce" 3) (list 116 "LT" "reduce" 41) (list 93 "LE" "reduce" 42) (list 97 "LE" "reduce" 38) (list 108 "PLUS" "shift" 19) (list 127 "LE" "reduce" 39) (list 102 "LE" "reduce" 40) (list 30 "LE" "reduce" 36) (list 8 "LE" "reduce" 37) (list 129 "ID" "shift" 74) (list 116 "LE" "reduce" 41) (list 129 "IF" "shift" 107) (list 90 "LE" "reduce" 43) (list 46 "ID" "shift" 22) (list 52 "ID" "shift" 22) (list 66 "ID" "shift" 22) (list 70 "NEW" "shift" 15) (list 88 "NULL" "shift" 8) (list 106 "NULL" "shift" 8) (list 86 "NULL" "shift" 8) (list 10 "EQ" "reduce" 31) (list 47 "EQ" "reduce" 32) (list 48 "EQ" "reduce" 33) (list 49 "EQ" "reduce" 34) (list 80 "STAR" "shift" 5) (list 6 "PRINTLN" "reduce" 13) (list 7 "PRINTLN" "reduce" 12) (list 22 "LT" "reduce" 35) (list 59 "LPAREN" "reduce" 15) (list 59 "type" "shift" 42) (list 73 "NEW" "shift" 15) (list 22 "LE" "reduce" 35) (list 109 "STAR" "shift" 40) (list 105 "expr" "shift" 108) (list 104 "expr" "shift" 108) (list 93 "PLUS" "reduce" 42) (list 97 "PLUS" "reduce" 38) (list 90 "PLUS" "reduce" 43) (list 116 "PLUS" "reduce" 41) (list 30 "PLUS" "reduce" 36) (list 8 "PLUS" "reduce" 37) (list 127 "PLUS" "reduce" 39) (list 102 "PLUS" "reduce" 40) (list 10 "GT" "reduce" 31) (list 47 "GT" "reduce" 32) (list 48 "GT" "reduce" 33) (list 49 "GT" "reduce" 34) (list 109 "IF" "shift" 107) (list 109 "ID" "shift" 74) (list 80 "AMP" "shift" 53) (list 108 "MINUS" "shift" 71) (list 10 "GE" "reduce" 31) (list 47 "GE" "reduce" 32)
    (list 48 "GE" "reduce" 33) (list 49 "GE" "reduce" 34) (list 21 "statements" "shift" 109) (list 109 "LPAREN" "shift" 38) (list 59 "dcl" "shift" 63) (list 128 "DELETE" "reduce" 11) (list 40 "ID" "shift" 22) (list 84 "RPAREN" "shift" 110) (list 88 "NUM" "shift" 30) (list 106 "NUM" "shift" 30) (list 86 "NUM" "shift" 30) (list 40 "LPAREN" "shift" 2) (list 38 "lvalue" "shift" 111) (list 22 "BECOMES" "reduce" 35) (list 40 "NEW" "shift" 15) (list 69 "LPAREN" "shift" 112) (list 77 "statements" "shift" 113) (list 76 "statements" "shift" 114) (list 73 "term" "shift" 11) (list 78 "statements" "shift" 115) (list 53 "STAR" "shift" 40) (list 57 "STAR" "reduce" 16) (list 93 "EQ" "reduce" 42) (list 97 "EQ" "reduce" 38) (list 90 "EQ" "reduce" 43) (list 30 "EQ" "reduce" 36) (list 8 "EQ" "reduce" 37) (list 127 "EQ" "reduce" 39) (list 102 "EQ" "reduce" 40) (list 85 "AMP" "shift" 53) (list 47 "LT" "reduce" 32) (list 48 "LT" "reduce" 33) (list 49 "LT" "reduce" 34) (list 10 "LT" "reduce" 31) (list 117 "COMMA" "reduce" 14) (list 18 "INT" "shift" 54) (list 83 "RETURN" "reduce" 18) (list 10 "LE" "reduce" 31) (list 3 "MINUS" "reduce" 47) (list 129 "statement" "shift" 57) (list 74 "MINUS" "reduce" 46) (list 22 "NE" "reduce" 35) (list 10 "RPAREN" "reduce" 31) (list 100 "MINUS" "reduce" 48) (list 47 "LE" "reduce" 32) (list 48 "LE" "reduce" 33) (list 49 "LE" "reduce" 34) (list 106 "term" "shift" 11) (list 86 "term" "shift" 11) (list 130 "RETURN" "reduce" 17) (list 47 "RPAREN" "reduce" 32) (list 48 "RPAREN" "reduce" 33) (list 49 "RPAREN" "reduce" 34) (list 70 "AMP" "shift" 53) (list 88 "term" "shift" 11) (list 68 "RETURN" "reduce" 20) (list 131 "RETURN" "reduce" 21) (list 82 "RETURN" "reduce" 19) (list 105 "term" "shift" 11) (list 104 "term" "shift" 11) (list 82 "STAR" "reduce" 19) (list 22 "STAR" "reduce" 35) (list 83 "STAR" "reduce" 18) (list 77 "LPAREN" "reduce" 15) (list 76 "LPAREN" "reduce" 15) (list 65 "RBRACK" "shift" 116) (list 70 "STAR" "shift" 5) (list 68 "STAR" "reduce" 20) (list 131 "STAR" "reduce" 21) (list 78 "LPAREN" "reduce" 15) (list 130 "STAR" "reduce" 17) (list 129 "DELETE" "shift" 55) (list 121 "MINUS" "shift" 71) (list 78 "DELETE" "reduce" 15) (list 42 "ID" "shift" 117) (list 77 "DELETE" "reduce" 15) (list 76 "DELETE" "reduce" 15) (list 113 "ID" "shift" 74) (list 114 "ID" "shift" 74) (list 113 "IF" "shift" 107) (list 114 "IF" "shift" 107) (list 115 "ID" "shift" 74) (list 115 "IF" "shift" 107) (list 38 "ID" "shift" 74) (list 123 "PLUS" "shift" 19) (list 124 "PLUS" "shift" 19) (list 122 "PLUS" "shift" 19) (list 47 "SLASH" "reduce" 32) (list 48 "SLASH" "reduce" 33) (list 49 "SLASH" "reduce" 34) (list 10 "SLASH" "reduce" 31) (list 41 "LBRACK" "shift" 118) (list 14 "NEW" "shift" 15) (list 44 "NEW" "shift" 15) (list 17 "NEW" "shift" 15) (list 25 "NEW" "shift" 15) (list 13 "NEW" "shift" 15) (list 9 "NEW" "shift" 15) (list 105 "STAR" "shift" 5) (list 104 "STAR" "shift" 5) (list 105 "test" "shift" 119) (list 104 "test" "shift" 120) (list 5 "AMP" "shift" 53) (list 122 "MINUS" "shift" 71) (list 123 "MINUS" "shift" 71) (list 124 "MINUS" "shift" 71) (list 70 "expr" "shift" 121) (list 100 "SEMI" "reduce" 48) (list 73 "ID" "shift" 22) (list 3 "SEMI" "reduce" 47) (list 74 "SEMI" "reduce" 46) (list 113 "WHILE" "shift" 43) (list 114 "WHILE" "shift" 43) (list 88 "ID" "shift" 22) (list 106 "ID" "shift" 22) (list 86 "ID" "shift" 22) (list 22 "GE" "reduce" 35) (list 115 "WHILE" "shift" 43) (list 88 "expr" "shift" 122) (list 106 "expr" "shift" 123) (list 86 "expr" "shift" 124) (list 120 "RPAREN" "shift" 125) (list 119 "RPAREN" "shift" 126) (list 22 "GT" "reduce" 35) (list 129 "lvalue" "shift" 67) (list 11 "SEMI" "reduce" 28) (list 6 "IF" "reduce" 13) (list 7 "IF" "reduce" 12) (list 61 "SEMI" "reduce" 30) (list 60 "SEMI" "reduce" 29) (list 53 "lvalue" "shift" 127) (list 6 "ID" "reduce" 13) (list 7 "ID" "reduce" 12) (list 88 "AMP" "shift" 53) (list 89 "ID" "reduce" 10) (list 110 "LBRACE" "shift" 128) (list 106 "AMP" "shift" 53) (list 86 "AMP" "shift" 53) (list 83 "IF" "reduce" 18) (list 83 "ID" "reduce" 18) (list 82 "IF" "reduce" 19) (list 130 "IF" "reduce" 17) (list 82 "ID" "reduce" 19) (list 68 "IF" "reduce" 20) (list 131 "IF" "reduce" 21) (list 31 "RPAREN" "reduce" 22) (list 32 "RPAREN" "reduce" 23) (list 33 "RPAREN" "reduce" 24) (list 34 "RPAREN" "reduce" 25) (list 35 "RPAREN" "reduce" 26) (list 36 "RPAREN" "reduce" 27) (list 21 "type" "shift" 42) (list 6 "RETURN" "reduce" 13) (list 7 "RETURN" "reduce" 12) (list 59 "statements" "shift" 129) (list 68 "ID" "reduce" 20) (list 131 "ID" "reduce" 21) (list 130 "ID" "reduce" 17) (list 22 "EQ" "reduce" 35) (list 11 "EQ" "reduce" 28) (list 73 "factor" "shift" 10) (list 61 "EQ" "reduce" 30) (list 60 "EQ" "reduce" 29) (list 85 "NULL" "shift" 8) (list 115 "PRINTLN" "shift" 58) (list 124 "SEMI" "shift" 130) (list 122 "SEMI" "shift" 131) (list 21 "RETURN" "reduce" 15) (list 113 "PRINTLN" "shift" 58) (list 114 "PRINTLN" "shift" 58) (list 21 "DELETE" "reduce" 15)))

(define (parse-token ls)
    (if (empty? ls) empty
        (cons (token (symbol->string (token-kind (first ls))) (token-lexeme (first ls))) (parse-token (rest ls)))))

(define (set-hashes)
    (make-productions production-list 0)

    (for ([i dfa-state-count])
        (hash-set! reduce i (make-hash))
        (hash-set! shift i (make-hash)))
    (for ([i dfa])
        (if (equal? (third i) "reduce")
            (add-reduce i) (add-shift i))))

(set-hashes)

(define (pop-stack stack n)
    (if (zero? n) stack
    (if (empty? stack) (show-error "Parse" "Symbol Stack Failure in LR(0) Parsing")
    (pop-stack (rest stack) (sub1 n)))))

(define (one-shift a)
    (if (hash-has-key? (hash-ref shift (first state-stack)) a)
        (set! state-stack (append (list (hash-ref (hash-ref shift (first state-stack)) a)) state-stack))
    (show-error "Parse" "State Stack Failure in LR(0) Parsing")))

(define (check-shift a)
    (if (or (empty? state-stack)
            (not (hash-has-key? (hash-ref shift (first state-stack)) a)))
        (show-error "Parse" "State Stack Failure in LR(0) Parsing")
        (begin
            (set! symbol-stack (append (list a) symbol-stack))
            (one-shift a)
            (if (or (equal? a "BOF") (equal? a "EOF")) (void)
            (set! seen-input (add1 seen-input))))))

(define (separate-list-acc ls count acc)
    (if (zero? count) (list acc ls)
        (separate-list-acc (rest ls) (sub1 count) (append acc (list (first ls))))))

(define (separate-list ls len)
    (separate-list-acc ls (- (length ls) len) (list)))

(define (update-parse-tree parse-trees parent) 
    (define new-node (list parent (second parse-trees)))
    (set! parse-tree (append (first parse-trees) (list new-node))))

(define (one-reduce a)
    (define production-index (hash-ref (hash-ref reduce (first state-stack)) a))
    (define production (hash-ref productions production-index))
    (define production-left (first production))
    (define production-right (second production))
    (define production-length (length production-right))

    (update-parse-tree (separate-list parse-tree production-length) production-left)
    (set! reduced-productions (append reduced-productions (list (append (list production-left) production-right))))

    (set! state-stack (pop-stack state-stack production-length))
    (set! symbol-stack (pop-stack symbol-stack production-length))

    (set! symbol-stack (append (list production-left) symbol-stack))
    
    (one-shift production-left))

(define (check-reduce a)
    (if (or (empty? state-stack)
            (not (hash-has-key? (hash-ref reduce (first state-stack)) a)))
        (void)
        (begin
            (one-reduce a)
            (check-reduce a))))

(define (print-first node)
(if (token? (first node)) (set! parsed (append parsed (list (string-append (token-kind (first node)) " " (token-lexeme (first node))))))
    (print-line (append (list (first node)) (map (lambda (lst) (if (token? (first lst)) (token-kind (first lst)) (first lst))) (second node))) "")))

(define (print-parse parse-tree)
    (if (empty? parse-tree) (void)
    (begin
        (print-first parse-tree)
        (print-children (second parse-tree)))))

(define (print-children children)
    (if (empty? children) (void)
    (begin
        (print-parse (first children))
        (print-children (rest children)))))

(define (modify-parse-tree start)
    (define first-ignore (separate-list parse-tree 1))
    (define first-part (first (first first-ignore)))
    (define second-part (first (rest (first first-ignore))))
    (define third-part (first (second first-ignore)))

    (set! parse-tree (list start (list first-part second-part third-part))))

(define (print-line ls acc)
    (if (empty? (rest ls)) (set! parsed (append parsed (list (string-append acc (first ls)))))
    (begin
        (print-line (rest ls) (string-append acc (first ls) " ")))))

(define (confirm-stacks)
    (define production-index (last state-stack))
    (define production (hash-ref productions production-index))
    (define production-left (first production))
    (define production-right (second production))

    (if (equal? production-left start-state)
        (begin
            (modify-parse-tree production-left)
            (print-parse parse-tree)
            parsed)
        (show-error "Parse" "Parsing Termination is Unsuccessful")))

(define (list->hash ls hash)
    (if (empty? ls) (void)
        (begin
            (hash-set! hash (first ls) 0)
            (list->hash (rest ls) hash))))
(list->hash alphabet alphabet-hash)

(define (derive input)
    (if (empty? input) (confirm-stacks)
    (if (hash-has-key? alphabet-hash (token-kind (first input)))
    (begin
        (check-reduce (token-kind (first input)))
        (check-shift (token-kind (first input)))
        (set! parse-tree (append parse-tree (list (list (first input) (list)))))
        (set! reduced-productions (append reduced-productions  (list (list (token-kind (first input)) (token-lexeme (first input))))))
        (derive (rest input)))
    (show-error "Parse" (string-append "Token Failure at " (number->string seen-input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 3: CONTEXT SENSITIVE ANALYZER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define offset 0)
(define label-counter 0)
(define first-arg INT)

(define (recurse-parse-tree children)
    (if (empty? children) empty
    (cons (make-parse-tree) (recurse-parse-tree (rest children)))))

(define (add-parse-tree line)
    (list (first line) (rest line) (recurse-parse-tree (rest line))))

(define (make-parse-tree-checked)
    (define line (first parsed))
    (define strings (string-split line))
    (set! parsed (rest parsed))
    (if (char-upper-case? (first (string->list line))) (token (first strings) (second strings))
    (add-parse-tree strings)))

(define (make-parse-tree)
    (if (empty? parsed) (show-error "Context Analysis" "Insufficient tokens are parsed")
        (make-parse-tree-checked)))

(define (get-type dcl)
    (if (equal? 1 (length (second (first dcl))))
    INT INT*))

(define (get-id dcl)
    (token-lexeme (second dcl)))

(define (add-dcl hash parameter)
    (define id (get-id parameter))
    (if (hash-has-key? hash id)
        (show-error "Context Analysis" (string-append id " is Already Declared"))
    (begin
        (hash-set! hash (get-id parameter) (list (get-type parameter) offset))
        (set! offset (- offset 4)))))

(define (add-paramlist signature variables paramlist)
    (define parameter (third (first (third paramlist))))
    (add-dcl variables parameter)
    (set! signature (append signature (list (get-type parameter))))

    (if (equal? 1 (length (second paramlist))) signature
    (add-paramlist signature variables (third (third paramlist)))))

(define (add-procedure procedure)
    (define name (string-append "ff" (token-lexeme (second procedure))))
    (define variables (make-hash))
    (define params (fourth procedure))
    (define signature (list))

    (if (hash-has-key? master name) (show-error "Context Analysis" (string-append name " is Already Declared"))
    (if (empty? (second params)) (void)
        (set! signature (add-paramlist signature variables (first (third params))))))

    (make-variables variables (seventh procedure))
    (hash-set! master name (list signature variables))

    (check-use variables (eighth procedure))
    (check-use variables (tenth procedure))

    (check-statements-type variables (eighth procedure))
    (is-expr-type variables (tenth procedure) INT true))

(define (add-body-dcl hash dcl)
    (define parameter (third (second dcl)))
    (define set-value (if (equal? (token-kind (fourth dcl)) "NUM") INT INT*))
    (define dcl-type (get-type parameter))
    (if (equal? set-value dcl-type)
        (add-dcl hash parameter) (show-error "Context Analysis" "Invalid Declaration of Variable Type")))

(define (make-variables hash dcls)
    (if (empty? (second dcls))
        (void)
        (begin
        (make-variables hash (first (third dcls)))
        (add-body-dcl hash (third dcls)))))

(define (check-children hash ls)
    (if (empty? ls) (void)
    (begin
    (check-use hash (first ls))
    (check-children hash (rest ls)))))

(define (check-procedure hash ls)
    (define used-name (token-lexeme (first (third ls))))
    (if (hash-has-key? hash used-name) (show-error "Context Analysis" (string-append used-name " is Recognized as Variables"))
    (if (not (hash-has-key? master used-name))
        (show-error "Context Analysis" (string-append "\"" used-name "\"" " Function is Not Defined")) (void))))

(define (check-use hash ls)
    (if (not (token? ls))
    (if (and (equal? (first ls) "factor") (> (length (second ls)) 1) (equal? (first (second ls)) "ID") (equal? (second (second ls)) "LPAREN")) (check-procedure hash ls)
    (check-children hash (third ls)))
    (if (and (equal? (token-kind ls) "ID") (not (hash-has-key? hash (token-lexeme ls)))) (show-error "Context Analysis" "Failure in Recursive Verification") (void))))

(define (convert-type type)
    (if (equal? type INT) "INT" "INT*"))

(define (add-main main)
    (define name "ffmain")
    (define variables (make-hash))
    (define parameter1 (third (fourth main)))
    (define parameter2 (third (sixth main)))
    (define signature (list (get-type parameter1) (get-type parameter2)))
    (if (or (equal? (first signature) INT*) (equal? (second signature) INT*)) (show-error "Type" "MAIN parameters must be of type INT") (void))

    (add-dcl variables parameter1)
    (add-dcl variables parameter2)
    (make-variables variables (ninth main))
    (hash-set! master name (list signature variables))

    (check-use variables (tenth main))
    (check-use variables (tenth (rest (rest main))))
    
    (check-statements-type variables (tenth main))
    (is-expr-type variables (tenth (rest (rest main))) INT true))

(define (make-master procedures)
    (if (equal? 1 (length (second procedures)))
        (add-main (third (first (third procedures))))
        (begin
        (add-procedure (third (first (third procedures))))
        (make-master (second (third procedures))))))

(define (parse-types types)
    (if (empty? types) empty
    (cons
        (if (zero? (first types)) "int" "int*")
        (parse-types (rest types)))))

(define (print-master)
    (hash-for-each master (lambda (procedure value)
        (fprintf (current-error-port) (string-append procedure ": " (string-join (parse-types (first value))) "\n"))
        
        (hash-for-each (second value) (lambda (variable value)
            (fprintf (current-error-port) (string-append variable " " (if (zero? value) "int" "int*") "\n"))))
        (void))))

(define (get-id-type variables id)
    (first (hash-ref variables (token-lexeme id))))

(define (get-arglist-types variables arglist)
    (cons (get-expr-type variables (first (third arglist)))
    (if (equal? (length (second arglist)) 1)
         empty
         (get-arglist-types variables (third (third arglist))))))

(define (get-factor-type variables factor)
    (define first-child (first (second factor)))
    (if (equal? first-child "NUM") INT
    (if (equal? first-child "NULL") INT*
    (if (equal? first-child "LPAREN") (get-expr-type variables (second (third factor)))
    (if (equal? first-child "AMP") INT*
    (if (and (equal? first-child "STAR") (is-factor-type variables (second (third factor)) INT*)) INT
    (if (and (equal? first-child "NEW") (is-expr-type variables (fourth (third factor)) INT false)) INT*
    (if (equal? first-child "ID")
        (if (empty? (rest (second factor))) (get-id-type variables (first (third factor)))
        (if (and (equal? (third (second factor)) "RPAREN") (is-procedure variables (first (third factor)) (list))) INT
        (if (and (equal? (third (second factor)) "arglist") (is-procedure variables (first (third factor)) (third (third factor)))) INT (show-error "Context Analysis" "Unable to get type of a factor"))))
    (show-error "Context Analysis" "Unable to get type of a factor")))))))))

(define (get-term-type variables term)
    (define first-child (first (second term)))
    (if (equal? first-child "factor") (get-factor-type variables (first (third term)))
    (if (and (is-term-type variables (first (third term)) INT) (is-factor-type variables (third (third term)) INT)) INT
    (show-error "Context Analysis" "Unable to get type of a term"))))

(define (get-case-expr-type variables expr)
    (define first-type (get-expr-type variables (first (third expr))))
    (define operator (token-kind (second (third expr))))
    (define second-type (get-term-type variables (third (third expr))))

    (if (equal? first-type second-type)
        (if (and (equal? operator "PLUS") (equal? first-type INT*)) (show-error "Context Analysis" "Unable to get type of expression") INT)
        (if (and (equal? operator "MINUS") (equal? first-type INT)) (show-error "Context Analysis" "Unable to get type of expression") INT*)))

(define (get-expr-type variables expr)
    (define first-child (first (second expr)))
    (if (equal? first-child "term") (get-term-type variables (first (third expr)))
    (get-case-expr-type variables expr)))

(define (get-lvalue-type variables lvalue)
    (define first-child (first (second lvalue)))
    (if (equal? first-child "ID") (get-id-type variables (first (third lvalue)))
    (if (and (equal? first-child "STAR") (is-factor-type variables (second (third lvalue)) INT*)) INT
    (if (equal? first-child "LPAREN") (get-lvalue-type variables (second (third lvalue)))
    (show-error "Context Analysis" "Unable to get type of lvalue")))))

(define (is-lvalue-type variables lvalue expect-type)
    (if (equal? (get-lvalue-type variables lvalue) expect-type) true (show-error "Context Analysis" (string-append "lvalue type does not evaluate to " (convert-type expect-type)))))

(define (is-procedure variables procedure-id expect-arglist)
    (define signature (first (hash-ref master (token-lexeme procedure-id))))
    (if (empty? expect-arglist)
        (if (empty? signature) true (show-error "Context Analysis" "Function must not be given any arguments,"))
    (if (equal? signature (get-arglist-types variables expect-arglist)) true (show-error "Context Analysis" (string-append "Function argument types do not match signature")))))

(define (is-term-type variables term expect-type)
    (if (equal? (get-term-type variables term) expect-type) true (show-error "Context Analysis" (string-append "Term type does not evaluate to " (convert-type expect-type)))))

(define (is-factor-type variables factor expect-type)
    (if (equal? (get-factor-type variables factor) expect-type) true (show-error "Context Analysis" (string-append "Factor type does not evaluate to " (convert-type expect-type)))))
    
(define (is-expr-type variables expr expect-type is-retval)
    (if (equal? (get-expr-type variables expr) expect-type) true (show-error "Context Analysis" (if is-retval (string-append "Return value of a function must be " (convert-type expect-type)) (string-append "Expression type does not evaluate to " (convert-type expect-type))))))
    
(define (check-statements-type variables ls)
    (if (empty? (second ls)) (void)
    (begin
        (check-statements-type variables (first (third ls)))
        (check-statement-type variables (second (third ls))))))

(define (check-test-type variables test)
    (is-expr-type variables (first (third test)) (get-expr-type variables (third (third test))) false))

(define (check-statement-type variables ls)
    (define first-child (first (second ls)))
    (if (equal? first-child "lvalue")
        (is-expr-type variables (third (third ls)) (get-lvalue-type variables (first (third ls))) false)
    (if (equal? first-child "IF")
        (and (check-test-type variables (third (third ls))) (check-statements-type variables (sixth (third ls))) (check-statements-type variables (tenth (third ls))))
    (if (equal? first-child "WHILE")
        (and (check-test-type variables (third (third ls))) (check-statements-type variables (sixth (third ls))))
    (if (equal? first-child "PRINTLN")
        (is-expr-type variables (third (third ls)) INT false)
    (if (equal? first-child "DELETE")
        (is-expr-type variables (fourth (third ls)) INT* false)
    (show-error "Context Analysis" "Invalid statement structure")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 4: CODE GENERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assembly-code (list))

(define (add-code line)
    (set! assembly-code (append assembly-code (list line))))

(define (get-id-offset variables id)
    (second (hash-ref variables (token-lexeme id))))

(define (get-variables procedure)
    (second (hash-ref master procedure)))

(define (code-num num)
    (add-code "lis $3")
    (add-code (string-append ".word " (token-lexeme num))))

(define (code-null)
    (add-code "add $3, $11, $0"))

(define (code-term variables term)
    (define rhs (second term))
    (define rhs-parse (third term))
    (if (equal? (first rhs) "factor")
        (code-factor variables (first (third term)))
    (if (equal? (second rhs) "STAR")
        (begin
            (code-term variables (first rhs-parse))
            (push 3)
            (code-factor variables (third rhs-parse))
            (pop 5)
            (add-code "mult $5, $3")
            (add-code "mflo $3")
        )
    (if (equal? (second rhs) "SLASH")
        (begin
            (code-term variables (first rhs-parse))
            (push 3)
            (code-factor variables (third rhs-parse))
            (pop 5)
            (add-code "div $5, $3")
            (add-code "mflo $3")
        )
    (if (equal? (second rhs) "PCT")
        (begin
            (code-term variables (first rhs-parse))
            (push 3)
            (code-factor variables (third rhs-parse))
            (pop 5)
            (add-code "div $5, $3")
            (add-code "mfhi $3")
        )
    (void))))))

(define (arithmetic-add-int-int variables expr term)
    (code-expr variables expr)
    (push 3)
    (code-term variables term)
    (pop 5)
    (add-code "add $3, $5, $3"))

(define (arithmetic-add-int*-int variables expr term)
    (code-term variables term)
    (add-code "mult $3, $4")
    (add-code "mflo $3")
    (push 3)
    (code-expr variables expr)
    (pop 5)
    (add-code "add $3, $3, $5"))

(define (arithmetic-add-int-int* variables expr term)
    (code-expr variables expr)
    (add-code "mult $3, $4")
    (add-code "mflo $3")
    (push 3)
    (code-term variables term)
    (pop 5)
    (add-code "add $3, $5, $3"))

(define (arithmetic-sub-int-int variables expr term)
    (code-expr variables expr)
    (push 3)
    (code-term variables term)
    (pop 5)
    (add-code "sub $3, $5, $3"))

(define (arithmetic-sub-int*-int variables expr term)
    (code-term variables term)
    (add-code "mult $3, $4")
    (add-code "mflo $3")
    (push 3)
    (code-expr variables expr)
    (pop 5)
    (add-code "sub $3, $3, $5"))

(define (arithmetic-sub-int*-int* variables expr term)
    (code-term variables term)
    (push 3)
    (code-expr variables expr)
    (pop 5)
    (add-code "sub $3, $3, $5")
    (add-code "div $3, $4")
    (add-code "mflo $3"))

(define (code-print variables print)
    (push 1)
    (code-expr variables (third print))
    (add-code "add $1, $3, $0")
    (push 31)
    (add-code "lis $5")
    (add-code ".word print")
    (add-code "jalr $5")
    (pop 31)
    (pop 1))

(define (code-statements variables statements)
    (define rhs (second statements))
    (define rhs-parse (third statements))
    (if (empty? rhs) (void)
        (begin
            (code-statements variables (first rhs-parse))
            (code-statement variables (second rhs-parse)))))

(define (code-while variables test statements)
    (define temp-label-counter label-counter)
    (set! label-counter (add1 label-counter))
    
    (add-code (string-append "while" (number->string temp-label-counter) "start" ":"))
    (code-test variables test)

    (add-code (string-append "beq $3, $0, while" (number->string temp-label-counter) "end"))
    (code-statements variables statements)
    (add-code (string-append "beq $0, $0, while" (number->string temp-label-counter) "start"))
    (add-code (string-append "while" (number->string temp-label-counter) "end" ":")))

(define (code-if variables test statements1 statements2)
    (define temp-label-counter label-counter)
    (set! label-counter (add1 label-counter))
    
    (code-test variables test)

    (add-code (string-append "beq $3, $0, if" (number->string temp-label-counter) "else"))
    (code-statements variables statements1)
    (add-code (string-append "beq $0, $0, if" (number->string temp-label-counter) "end"))
    (add-code (string-append "if" (number->string temp-label-counter) "else" ":"))
    (code-statements variables statements2)
    (add-code (string-append "if" (number->string temp-label-counter) "end" ":")))

(define (code-expr variables expr)
    (define rhs (second expr))
    (define rhs-parse (third expr))
    (if (equal? (first rhs) "term")
        (code-term variables (first (third expr)))
    (if (equal? (second rhs) "PLUS")
        (if (zero? (get-expr-type variables (first (third expr))))
            (if (zero? (get-term-type variables (third (third expr))))
                (arithmetic-add-int-int variables (first rhs-parse) (third rhs-parse))
                (arithmetic-add-int-int* variables (first rhs-parse) (third rhs-parse)))
            (arithmetic-add-int*-int variables (first rhs-parse) (third rhs-parse)))
    (if (equal? (second rhs) "MINUS")
        (if (zero? (get-expr-type variables (first (third expr))))
            (arithmetic-sub-int-int variables (first rhs-parse) (third rhs-parse))
            (if (zero? (get-term-type variables (third (third expr))))
                (arithmetic-sub-int*-int variables (first rhs-parse) (third rhs-parse))
                (arithmetic-sub-int*-int* variables (first rhs-parse) (third rhs-parse))))
    (void)))))

(define (code-id variables id)
    (define id-offset (get-id-offset variables id))
    (add-code (string-append "lw $3, " (number->string id-offset) "($29)")))

(define (code-test variables test)
    (define op (second (second test)))
    (define is-int (zero? (get-expr-type variables (first (third test)))))

    (code-expr variables (first (third test)))
    (push 3)
    (code-expr variables (third (third test)))
    (pop 5)

    (if (equal? op "LT")
        (if is-int
            (add-code "slt $3, $5, $3")
            (add-code "sltu $3, $5, $3"))
    (if (equal? op "GT")
        (if is-int
            (add-code "slt $3, $3, $5")
            (add-code "sltu $3, $3, $5"))
    (if (equal? op "LE")
        (if is-int
        (begin
            (add-code "slt $3, $3, $5")
            (add-code "sub $3, $11, $3")
        )
        (begin
            (add-code "sltu $3, $3, $5")
            (add-code "sub $3, $11, $3")
        ))
    (if (equal? op "GE")
        (if is-int
        (begin
            (add-code "slt $3, $5, $3")
            (add-code "sub $3, $11, $3")
        )
        (begin
            (add-code "sltu $3, $5, $3")
            (add-code "sub $3, $11, $3")
        ))
    (if (equal? op "NE")
        (begin
            (if is-int
                (begin
                    (add-code "slt $6, $5, $3")
                    (add-code "slt $7, $3, $5")
                )
                (begin
                    (add-code "sltu $6, $5, $3")
                    (add-code "sltu $7, $3, $5")
            ))

            (add-code "add $3, $6, $7")
        )
        (begin
            (if is-int
                (begin
                    (add-code "slt $6, $5, $3")
                    (add-code "slt $7, $3, $5")
                )
                (begin
                    (add-code "sltu $6, $5, $3")
                    (add-code "sltu $7, $3, $5")
            ))

            (add-code "add $3, $6, $7")
            (add-code "sub $3, $11, $3"))))))))

(define (code-new variables expr)
    (code-expr variables expr)
    (add-code "add $1, $3, $0")
    (push 31)
    (add-code "lis $5")
    (add-code ".word new")
    (add-code "jalr $5")
    (pop 31)
    (add-code "bne $3, $0, 1")
    (add-code "add $3, $11, $0"))

(define (code-delete variables expr)
    (code-expr variables expr)
    (add-code (string-append "beq $3, $11, skipDelete" (number->string label-counter)))
    (add-code "add $1, $3, $0")
    (push 31)
    (add-code "lis $5")
    (add-code ".word delete")
    (add-code "jalr $5")
    (pop 31)
    (add-code (string-append "skipDelete" (number->string label-counter) ":"))
    (set! label-counter (add1 label-counter)))

(define (code-statement variables statement)
    (define rhs (second statement))
    (define rhs-parse (third statement))
    (if (equal? (first rhs) "PRINTLN")
        (code-print variables rhs-parse)
    (if (equal? (first rhs) "lvalue")
        (begin
            (code-expr variables (third (third statement)))
            (push 3)
            (code-lvalue variables (first (third statement)))
            (pop 5)
            (add-code "add $3, $3, $29")
            (add-code "sw $5, 0($3)")
        )
    (if (equal? (first rhs) "WHILE")
        (code-while variables (third (third statement)) (sixth (third statement)))
    (if (equal? (first rhs) "IF")
        (code-if variables (third (third statement)) (sixth (third statement)) (tenth (third statement)))
    (if (equal? (first rhs) "DELETE")
        (code-delete variables (fourth (third statement)))
    (void)))))))

(define (code-lvalue-id-case variables lvalue)
    (define id (first (third lvalue)))
    (define id-offset (get-id-offset variables id))
    (add-code "lis $3")
    (add-code (string-append ".word " (number->string id-offset))))

(define (code-lvalue variables lvalue)
    (define rhs (second lvalue))
    (if (equal? (first rhs) "ID")
        (code-lvalue-id-case variables lvalue)
    (if (equal? (first rhs) "LPAREN")
        (code-lvalue variables (second (third lvalue)))
        (begin
            (code-factor variables (second (third lvalue)))
            (add-code "sub $3, $3, $29")
))))

(define (call-procedure factor)
    (add-code "lis $5")
    (add-code (string-append ".word ff" (token-lexeme (first (third factor)))))
    (add-code "jalr $5")

    (pop 29)
    (pop 31))

(define (code-arglist variables arglist)
    (define expr (first (third arglist)))
    (code-expr variables expr)
    (push 3)

    (if (equal? 1 (length (second arglist))) (void)
        (code-arglist variables (third (third arglist)))))

(define (code-factor variables factor)
    (define rhs (second factor))
    (if (and (equal? (length rhs) 1) (equal? (first rhs) "ID"))
        (code-id variables (first (third factor)))
    (if (equal? (first rhs) "LPAREN")
        (code-expr variables (second (third factor)))
    (if (equal? (first rhs) "NUM")
        (code-num (first (third factor)))
    (if (equal? (first rhs) "NULL")
        (code-null)
    (if (equal? (first rhs) "AMP")
        (begin
            (code-lvalue variables (second (third factor)))
            (add-code "add $3, $3, $29")
        )
    (if (equal? (first rhs) "STAR")
        (begin
            (code-factor variables (second (third factor)))
            (add-code "lw $3, 0($3)")
        )
    (if (equal? (first rhs) "NEW")
        (code-new variables (fourth (third factor)))
    (if (equal? (third rhs) "RPAREN")
        (begin
            (push 31)
            (push 29)
            (call-procedure factor)
        )
    (if (equal? (third rhs) "arglist")
        (begin
            (push 31)
            (push 29)
            (add-code "sub $6, $30, $4")
            (code-arglist variables (third (third factor)))
            (add-code "add $29, $6, $0")
            (call-procedure factor)
        )
    (void)))))))))))

(define (code-dcls variables dcls)
    (if (empty? (second dcls)) (void)
    (if (equal? (fourth (second dcls)) "NUM")
        (begin
            (code-dcls variables (first (third dcls)))
            (code-num (fourth (third dcls)))
            (add-code (string-append "sw $3, " (number->string (get-id-offset variables (second (third (second (third dcls)))))) "($29)"))
            (add-code "sub $30, $30, $4")
        )
    (if (equal? (fourth (second dcls)) "NULL")
        (begin
            (code-dcls variables (first (third dcls)))
            (code-null)
            (add-code (string-append "sw $3, " (number->string (get-id-offset variables (second (third (second (third dcls)))))) "($29)"))
            (add-code "sub $30, $30, $4")
        )
    (void)))))

(define (code-main main)
    (define variables (get-variables "ffmain"))
    
    (generate-prologue)
    
    (if (zero? first-arg)
        (begin
            (push 2)
            (add-code "add $2, $0, $0")
            (call-init)
            (pop 2)
        )

        (call-init)
    )

    (code-dcls variables (ninth (third main)))
    (code-statements variables (tenth (third main)))
    (code-expr variables (tenth (rest (rest (third main)))))

    (generate-epilogue))

(define (restore-30 variables)
    (define count (length (hash-keys variables)))
    (for ([i count])
        (add-code "add $30, $30, $4")))

; (define (code-dcl procedure dcl offset)
;     (pop 3)
;     (add-code (string-append "sw $3, " (number->string offset) "($29)"))
; )

; (define (code-paramlist procedure paramlist offset)
;     (code-dcl procedure (first (third paramlist)) offset)
;     (if (equal? 1 (length (second paramlist))) (void)
;         (code-paramlist procedure (third (third paramlist)) (+ offset 4))))

; (define (code-params procedure params)
;     (define param-count (sub1 (length (first (hash-ref master procedure)))))
;     (if (empty? (second params))
;         (void) (code-paramlist procedure (first (third params)) (- (* 4 param-count))))
; )

(define (code-procedure procedure)
    (define name (string-append "ff" (token-lexeme (second (third procedure)))))
    (define variables (get-variables name))
    (define params (fourth (third procedure)))
    (define dcls (seventh (third procedure)))
    (define statements (eighth (third procedure)))
    (define expr (tenth (third procedure)))

    (add-code "")
    (add-code (string-append name ":"))
    (code-dcls variables dcls)
    (code-statements variables statements)
    (code-expr variables expr)

    (restore-30 variables)
    (add-code "jr $31"))

(define (code-procedures procedures first-call)
    (define rhs (second procedures))

    (if (equal? (first rhs) "main")
        (if first-call (code-main (first (third procedures)))
            (void))
    (if (equal? (first rhs) "procedure")
        (begin
            (if first-call (void)
                (code-procedure (first (third procedures))))
            
            (code-procedures (second (third procedures)) first-call)
        )
    (void))))

(define (push r)
    (add-code "sub $30, $30, $4")
    (add-code (string-append "sw $" (number->string r) ", 0($30)")))

(define (pop r)
    (add-code (string-append "lw $" (number->string r) ", 0($30)"))
    (add-code "add $30, $30, $4"))

(define (generate-code)
    (code-procedures (second (third parse-tree)) true)
    (code-procedures (second (third parse-tree)) false))

(define (call-init)
    (void))
    ; (push 31)
    ; (add-code "lis $5")
    ; (add-code ".word init")
    ; (add-code "jalr $5")
    ; (pop 31))

(define (generate-prologue)
    (add-code "ffmain:")
    ; (add-code ".import init")
    ; (add-code ".import new")
    ; (add-code ".import delete")
    ; (add-code ".import print")
    (add-code "lis $1")
    (add-code (string-append ".word " (number->string first-int)))
    (add-code "lis $2")
    (add-code (string-append ".word " (number->string second-int)))
    (add-code "lis $4")
    (add-code ".word 4")
    (add-code "lis $11")
    (add-code ".word 1")

    (add-code "sub $29, $30, $4")
    (add-code "sw $1, 0($29)")
    (add-code "sub $30, $30, $4")
    (add-code "sw $2, -4($29)")
    (add-code "sub $30, $30, $4"))

(define (generate-epilogue)
    (restore-30 (get-variables "ffmain"))
    (add-code "jr $31"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 5: COMBINE ALL STEPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
    (define inputs (read-all-input))
    (set-action inputs)
    (set! inputs (rest inputs))

    (if (or (equal? action "#scan") (equal? action "#parse") (equal? action "#analyze")
            (equal? action "#assembly") (equal? action "#binary")) (void) (show-error-basic "Compilation action is not recognized"))

    (define scanner-output (simplify-tokens (get-tokens inputs)))

    (if (equal? action "#scan")
        (begin
            (print-tokens scanner-output)
            (exit))
        (void))

    (define sequence (append (list (token "BOF" "BOF")) (parse-token scanner-output) (list (token "EOF" "EOF"))))
    (define parser-output (derive sequence))

    (if (equal? action "#parse")
        (begin
            (print-lines parser-output)
            (exit))
        (void))

    (set! parse-tree (make-parse-tree))
    (make-master (second (third parse-tree)))

    (if (equal? action "#analyze")
        (begin
            (displayln "The program successfully passes the context sensitive analysis")
            (exit))
        (void))

    (generate-code)

    (if (equal? action "#assembly")
        (begin
            (print-lines assembly-code)
            (exit))
        (void))

    (mips-main assembly-code)
    (exit))
    
(main)