;; compiler for the mips assembly language and ouputs the binary for each mips instructions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBALS AND IMPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide (all-defined-out)) 
(require racket/match)
(define-struct token (kind lexeme) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 1: SCANNER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (token-int-value tok)
  (match tok
    [(token 'INT    lexeme)
     (string->number lexeme)]
    [(token 'HEXINT lexeme)
     (string->number (substring lexeme 2) 16)]
    [(token 'REG    lexeme)
     (string->number (substring lexeme 1))]
    [_ (raise-user-error 'ERROR "Cannot get int value of token")]))
    
(define asm-kinds
  (set 'ID 'LABEL 'WORD 'COMMA 'LPAREN 'RPAREN 'INT 'HEXINT 'REG 'ZERO 'DOTID))

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
                   (map token-postprocess tokens)))]
    fixed-tokens))
  
(define-struct DFA (start accepting transition))

(define (char-hexdigit? ch)
  (string-contains? "abcdefABCDEF0123456789" (string ch)))

(define asm-dfa
  (make-DFA
    'START
    (set-union (set 'WHITESPACE 'COMMENT) (set-remove asm-kinds 'WORD))
    (lambda (state letter)
      (match (cons state letter)
        [(not (cons _ (? char?)))                                #f]
        [(cons 'START (? char-alphabetic?))                     'ID]
        [(cons 'START #\.)                                      'DOT]
        [(cons 'START #\0)                                      'ZERO]
        [(cons 'START (? char-numeric?))                        'INT]
        [(cons 'START #\-)                                      'MINUS]
        [(cons 'START #\;)                                      'COMMENT]
        [(cons 'START (? char-whitespace?))                     'WHITESPACE]
        [(cons 'START #\$)                                      'DOLLARS]
        [(cons 'START #\,)                                      'COMMA]
        [(cons 'START #\()                                      'LPAREN]
        [(cons 'START #\))                                      'RPAREN]
        [(cons 'ID (or (? char-alphabetic?)
                        (? char-numeric?)))                     'ID]
        [(cons 'ID #\:)                                         'LABEL]
        [(cons (or 'DOT 'DOTID) (? char-alphabetic?))           'DOTID]
        [(cons 'ZERO #\x)                                       'ZEROX]
        [(cons (or 'ZEROX 'HEXINT) (? char-hexdigit?))          'HEXINT]
        [(cons (or 'ZERO 'MINUS 'INT) (? char-numeric?))        'INT]
        [(cons 'COMMENT (? (lambda (c) (not (eq? c #\newline))))) 'COMMENT]
        [(cons 'WHITESPACE (? char-whitespace?))                'WHITESPACE]
        [(cons (or 'DOLLARS 'REG) (? char-numeric?))            'REG]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 2: ASSEMBLER (SEMANTIC ANALYSIS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-scan input)
    (if (check-instruction input)
      (set! counter (+ counter 4))
        (if (check-label (first input))
          (if (empty? (rest input)) true (check-scan (rest input))) (show-error))))

(define (check-instruction input)
  (if (empty? (rest input)) false
    (or (check-word input) (check-jump input) (check-arithmetic input) (check-branch input) (check-move input) (check-arithmetic-complex input) (check-transfer input))))

(define (check-word input)
  (if (is-word (first input))
    (if (check-word-wrapper input) true (show-error)) false))

(define (check-word-wrapper input)
  (and (empty? (rest (rest input)))
    (or (is-integer (second input))
      (is-hex (second input))
      (label-in-instruction (second input)))))

(define (check-jump input)
  (if (is-jump (first input))
    (if (check-jump-wrapper input) true (show-error)) false))

(define (check-jump-wrapper input)
  (and (empty? (rest (rest input)))
    (is-reg (second input))))

(define (check-move input)
  (if (is-move (first input))
    (if (check-move-wrapper input) true (show-error)) false))

(define (check-move-wrapper input)
  (and (empty? (rest (rest input)))
    (is-reg (second input))))

(define (check-arithmetic-complex input)
  (if (is-arithmetic-complex (first input))
    (if (check-arithmetic-complex-wrapper input) true (show-error)) false))

(define (check-arithmetic-complex-wrapper input)
  (and (not (empty? (rest (rest input)))) (not (empty? (rest (rest (rest input)))))
    (empty? (rest (rest (rest (rest input)))))
    (is-reg (second input)) (is-comma (third input)) (is-reg (fourth input))))

(define (check-arithmetic input)
  (if (is-arithmetic (first input))
    (if (check-arithmetic-wrapper input) true (show-error)) false))

(define (check-arithmetic-wrapper input)
  (and (has-five-op input)
    (is-reg (second input)) (is-comma (third input)) (is-reg (fourth input)) (is-comma (fifth input)) (is-reg (sixth input))))

(define (check-transfer input)
  (if (is-transfer (first input))
    (if (check-transfer-wrapper input) true (show-error)) false))

(define (check-transfer-wrapper input)
  (and (has-six-op input)
    (is-reg (second input)) (is-comma (third input)) (or (is-integer-other (fourth input)) (is-hex-other (fourth input)))
    (is-open-bracket (fifth input)) (is-reg (sixth input)) (is-close-bracket (seventh input))))

(define (has-atleast-five-op input)
  (and (not (empty? (rest (rest input)))) 
    (not (empty? (rest (rest (rest input))))) (not (empty? (rest (rest (rest (rest input))))))
    (not (empty? (rest (rest (rest (rest (rest input)))))))))

(define (has-five-op input)
  (and (has-atleast-five-op input) (empty? (rest (rest (rest (rest (rest (rest input)))))))))

(define (has-six-op input)
  (and (has-atleast-five-op input) (not (empty? (rest (rest (rest (rest (rest (rest input))))))))
    (empty? (rest (rest (rest (rest (rest (rest (rest input))))))))))

(define (check-branch input)
  (if (is-branch (first input))
    (if (check-branch-wrapper input) true (show-error)) false))

(define (check-branch-wrapper input)
  (and (has-five-op input)
    (is-reg (second input)) (is-comma (third input)) (is-reg (fourth input)) (is-comma (fifth input))
    (or (is-integer-other (sixth input))
      (is-hex-other (sixth input))
      (label-in-instruction (sixth input)))))

(define (check-label token)
  (if (is-label token) 
    (check-symbol-table (token-lexeme token)) false))

(define (shorten label)
  (substring label 0 (sub1 (string-length label))))

(define (check-symbol-table lexeme)
  (local [(define label (shorten lexeme))]
    (if (hash-has-key? symbol-table label) (show-error)
     (hash-set! symbol-table label counter))))

(define (check-symbol-table-end label)
  (if (hash-has-key? symbol-table label) (void) (show-error)))

(define (is-word token)
  (equal? (token-kind token) 'WORD))

(define (is-open-bracket token)
  (equal? (token-kind token) 'LPAREN))

(define (is-close-bracket token)
  (equal? (token-kind token) 'RPAREN))

(define (is-jump token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "jr")
        (equal? lexeme "jalr"))))

(define (is-transfer token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "lw")
        (equal? lexeme "sw"))))

(define (is-move token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "lis")
        (equal? lexeme "mfhi")
        (equal? lexeme "mflo"))))

(define (is-branch token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "beq")
        (equal? lexeme "bne"))))

(define (is-arithmetic token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "slt")
        (equal? lexeme "sltu")
        (equal? lexeme "add")
        (equal? lexeme "sub"))))

(define (is-arithmetic-complex token)
  (define lexeme (token-lexeme token))
  (and (equal? (token-kind token) 'ID) 
    (or (equal? lexeme "mult")
        (equal? lexeme "multu")
        (equal? lexeme "div")
        (equal? lexeme "divu"))))

(define (is-reg token)
  (and (equal? (token-kind token) 'REG) 
      (in-reg-interval (token-lexeme token))))

(define (is-comma token)
  (equal? (token-kind token) 'COMMA))

(define (get-register lexeme)
  (string->number (substring lexeme 1)))

(define (in-reg-interval lexeme)
  (define num (get-register lexeme))
  (and (integer? num) (>= num 0) (<= num 31)))

(define (is-integer token)
  (and (equal? (token-kind token) 'INT)
    (>= (token-int-value token) (- (expt 2 31)))
    (<= (token-int-value token) (sub1 (expt 2 32)))))

(define (is-hex token)
  (and (equal? (token-kind token) 'HEXINT) 
  (<= (token-int-value token) #xffffffff)))

(define (is-integer-other token)
  (and (equal? (token-kind token) 'INT)
    (>= (token-int-value token) -32768)
    (<= (token-int-value token) 32767)))

(define (is-hex-other token)
  (and (equal? (token-kind token) 'HEXINT) 
  (<= (token-int-value token) #xffff)))

(define (label-in-instruction token)
  (if (equal? (token-kind token) 'ID) (set! used-label (append (list (token-lexeme token)) used-label)) false))

(define (is-label token)
  (equal? (token-kind token) 'LABEL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 3: CODE GENERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-byte input)
  (if (empty? input) (void) 
    (if (is-label (first input)) 
    (get-byte (rest input)) (get-instruction-byte input))))

(define (get-instruction-byte input)
  (set! counter (+ counter 4))
  (if (is-word (first input)) (get-word-byte input)
  (if (is-jump (first input))
    (if (equal? (token-lexeme (first input)) "jr") (get-jump-byte input 8) (get-jump-byte input 9))
  (if (is-transfer (first input))
    (if (equal? (token-lexeme (first input)) "lw") (get-transfer-byte input 35) (get-transfer-byte input 43))
  (if (is-arithmetic (first input)) (get-arithmetic-byte input)
  (if (is-arithmetic-complex (first input)) (get-arithmetic-complex-byte input)
  (if (is-move (first input)) (get-move-byte input)
  (if (is-branch (first input))
    (if (equal? (token-lexeme (first input)) "beq") (get-branch-byte input 4) (get-branch-byte input 5))
  (show-error)))))))))

(define (write-bytes instruction)
  (write-byte (bitwise-and (arithmetic-shift instruction -24) #xff))
  (write-byte (bitwise-and (arithmetic-shift instruction -16) #xff))
  (write-byte (bitwise-and (arithmetic-shift instruction -8) #xff))
  (write-byte (bitwise-and instruction #xff)))

(define (get-jump-byte input rep)
  (define register (get-register (token-lexeme (second input))))
  (write-bytes (bitwise-ior (arithmetic-shift register 21) rep)))

(define (get-transfer-byte input rep)
  (define t (get-register (token-lexeme (second input))))
  (define i (token-int-value (fourth input)))
  (define s (get-register (token-lexeme (sixth input))))
  (define instruction (bitwise-ior 
      (arithmetic-shift rep 26)
      (arithmetic-shift s 21)
      (arithmetic-shift t 16) (bitwise-and i #xffff)))
  (write-bytes instruction))

(define (get-arithmetic-byte input)
  (define command (token-lexeme (first input)))
  (define rep (if (equal? command "add") 32
            (if (equal? command "sub") 34
            (if (equal? command "slt") 42 43))))
  (define d (get-register (token-lexeme (second input))))
  (define s (get-register (token-lexeme (fourth input))))
  (define t (get-register (token-lexeme (sixth input))))
  (define instruction (bitwise-ior (arithmetic-shift s 21)
      (arithmetic-shift t 16)
      (arithmetic-shift d 11) rep))
  (write-bytes instruction))

(define (get-arithmetic-complex-byte input)
  (define command (token-lexeme (first input)))
  (define rep (if (equal? command "mult") 24
            (if (equal? command "multu") 25
            (if (equal? command "div") 26 27))))
  (define s (get-register (token-lexeme (second input))))
  (define t (get-register (token-lexeme (fourth input))))
  (define instruction (bitwise-ior (arithmetic-shift s 21)
      (arithmetic-shift t 16) rep))
  (write-bytes instruction))

(define (get-move-byte input)
  (define command (token-lexeme (first input)))
  (define rep (if (equal? command "mfhi") 16
            (if (equal? command "mflo") 18 20)))
  (define register (get-register (token-lexeme (second input))))
  (define instruction (bitwise-ior (arithmetic-shift register 11) rep))
  (write-bytes instruction))

(define (get-offset token)
  (/ (- (hash-ref symbol-table (token-lexeme token)) counter) 4))

(define (get-branch-byte input rep)
  (define token (sixth input))
  (define s (get-register (token-lexeme (second input))))
  (define t (get-register (token-lexeme (fourth input))))
  (define i (if (equal? (token-kind token) 'ID) (get-offset token) (token-int-value token)))
  (define instruction (bitwise-ior 
      (arithmetic-shift rep 26)
      (arithmetic-shift s 21)
      (arithmetic-shift t 16) (bitwise-and i #xffff)))
  (write-bytes instruction))

(define (get-word-byte input)
  (define token (second input))
  (define token-int (if (equal? (token-kind token) 'ID) (hash-ref symbol-table (token-lexeme token)) (token-int-value token)))
  (define instruction (bitwise-and token-int #xffffffff))
  (write-bytes instruction))

(define (print-symbol)
  (print-each-symbol (hash-keys symbol-table)))

 (define (print-each-symbol symbols)
 (if (empty? symbols) (void)
  (begin (fprintf (current-error-port) (string-append (first symbols) " " (number->string (hash-ref symbol-table (first symbols))) "\n" ))
  (print-each-symbol (rest symbols)))))

(define (show-error)
  (fprintf (current-error-port) (string-append "ERROR\n"))
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-all-input)
    (local ((define line (read-line)))
      (if (eof-object? line) empty
        (cons line (read-all-input)))))

(define (scan-inputs ls)
  (if (empty? ls) empty
    (cons (scan (first ls)) (scan-inputs (rest ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STEP 4: COMBINE ALL STEPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol-table (make-hash))
(define counter 0)
(define used-label (list))

(define (mips-main ls)
  (define inputs (scan-inputs ls))

  ;; check if valid inputs
    (for ([line inputs])
      (or (empty? line) (check-scan line)))

  ;; check if valid assembly (labels)
    (for ([label used-label])
      (check-symbol-table-end label))

  ;; generate byte codes
    (set! counter 0)
    (for ([line inputs])
      (get-byte line)))

(define (main)
  (define inputs (read-all-input))
  (mips-main (inputs))
)