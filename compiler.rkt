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

(define inputs (read-all-input))

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

(define (set-action)
    (if (empty? inputs) (show-error "Scan" "Input file is empty")
        (set-action)))

(define (set-action-not-empty)
    (define potential (string-split (first inputs)))
    (define error-msg "Compilation action (first line) does not follow proper format")

    (if (and (or (equal? (length potential) 1) (equal? (length potential) 3))
            (> (string-length (first potential)) 0)
            (equal? (substring (first potential) 0 1) "#"))
            
            (begin
                (set! action (first potential))
                (if (equal? (length potential) 3)
                    (if (and (number? (string->number (second potential)))
                            (number? (string->number (third potential))))
                    (begin
                        (set! first-int (string->number (second potential)))
                        (set! second-int (string->number (third potential))))
                    (show-error "Scan" error-msg))
                    (void)))
            (show-error "Scan" error-msg)))

(set-action)
(set! inputs (rest inputs))

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
