#lang racket
(provide to-bf
         )

(define (to-bf . structure)
  (apply string-append (flatten structure)))

(define-syntax defop
  (syntax-rules ()
    [(defop (name args ...) body ...)
     (define (name args ...) (list body ...))]
    [(defop (name args ... . x) body ...)
     (define (name args ... . x) (list body ...))]))

;; Stack manipulation
(define (incr) "+")
(define (decr) "-")
(define (loop-while-nonzero . body) `("[" ,@body "]"))
(define (next!) ">") ; exclamation points say things that manipulate the stack.
(define (prev!) "<")
(define (output) ".")
(define (input-char) ",")
(define (next-n! n) (make-string n #\>))
(define (prev-n! n) (make-string n #\<))
(define (set-0) (loop-while-nonzero (decr)))
(define (free!) `(,(set-0) ,(prev!)))

(defop (with-next . body)
  (next!) body (prev!))
(defop (with-prev . body)
  (prev!) body (next!))
(defop (with-next-n n . body)
  (next-n! n) body (prev-n! n))
(defop (with-prev-n n . body)
  (prev-n! n) body (next-n! n))

;;; More advanced stack manipulation. Forth notation, written ( consumes -- produces )


;; ( -- )
(defop (output-char n)
  (push-constant (char->integer n))
  (output)
  (free!))

;; ( -- ) TODO! INEFFICIENT
(defop (output-string s)
  (map (λ(c) (output-char c)) (string->list s)))

;; ( a b c -- a+c b )
(defop (add-to n)
  (loop-while-nonzero
   (decr)
   (with-prev-n n
                (incr)))
  (prev!))

;; ( a -- 10a )
(defop (mult-10)
  "[>++++++++++<-]"
  (next!) (add-to-prev))

;; ( -- k )
(defop (push-constant n)
  (next!)
  (let each-digit ([n n])
    (let-values ([(q r) (quotient/remainder n 10)])
      (list
       (if (> q 0)
           (list (each-digit q) (mult-10))
           '())
       (make-string r #\+)))))
          

;; ( a b c -- a b c a )
(defop (dup-n n)
  (with-prev-n n
               (loop-while-nonzero
                (decr)
                (with-next-n (+ 1 n)
                             (incr)
                             (with-next (incr)))))
  ; the stack is ( *0 a a )
  
  (next!) (next!)       ; ( 0 ... a *a )
  (add-to (+ n 2)))    ; ( a ... a )

;; ( a -- a a )
(defop (dup)
  (dup-n 0))

;; ( a b c -- c b )
(defop (move-to n)
  (with-prev-n n (set-0))
  (add-to n))


;; ( a b -- a+b )
(defop (add-to-prev)
  (add-to 1))

;; ( a -- 2a )
(defop (double)
  (loop-while-nonzero
   (decr)
   (with-next (incr) (incr)))
  (next!) (add-to-prev))

;; ( a -- ) consumes one cell, then executes body. Be sure you have a balanced stack!
(defop (if-nonzero . body)
  (loop-while-nonzero
   (set-0)
   (with-prev body))
  (prev!))

;; ( 0 -- 0 )
;; ( 35 -- 1 )
(defop (ensure-1)
  (loop-while-nonzero
   (loop-while-nonzero (decr))
   (with-next (incr)))
  (next!) (add-to 1))

;; ( 0 -- 1 )
;; ( 35 -- 0 )
(defop (not)
  (ensure-1) (dup)
  (with-prev (incr)) ; stack is either 2 1 or 1 0 
  (if-nonzero (decr) (decr)))

;; ( a -- ) consumes one cell, then executes body. Be sure you have a balanced stack!
(defop (if-zero . body)
  (not) (if-nonzero body))

;; consumes one cell AFTER running then or else.
;; if nonzero, then "then" will be run; if zero, "else" will be run.
(defop (if-branch then else)
  (dup)
  (if-nonzero then)
  (if-zero (with-next else)))
  
  
  
  