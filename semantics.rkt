#lang racket
 
(require rackunit)                ;; for unit testing
(provide (all-defined-out))
 
 
;; Our state contains two pieces.
(define-struct state (data ptr)
  #:mutable)
 
(define *SIZE* 30000)
;; Creates a new state, with a byte array of 30000 zeros, and
;; the pointer at index 0.
(define (new-state)
  (make-state (make-vector *SIZE* 0)
              0))
 
;; increment the data pointer
(define (increment-ptr a-state)
  (set-state-ptr! a-state (add1 (state-ptr a-state))))
 
;; decrement the data pointer
(define (decrement-ptr a-state)
  (set-state-ptr! a-state (sub1 (state-ptr a-state))))
 
;; increment the byte at the data pointer
(define (increment-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (add1 (vector-ref v i)))))
 
;; decrement the byte at the data pointer
(define (decrement-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (sub1 (vector-ref v i)))))
 
;; print the byte at the data pointer
(define (write-byte-to-stdout a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (write-byte (vector-ref v i) (current-output-port))))
 
;; read a byte from stdin into the data pointer
(define (read-byte-from-stdin a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (vector-set! v i (read-byte (current-input-port)))))

;; Displaying things
(define (get-state-near-cells a-state)
  (let* ([left (max 0 (- (state-ptr a-state) 10))]
         [ptr (state-ptr a-state)]
         [right (min *SIZE* (+ (state-ptr a-state) 20))]
         [begin (if (= left 0) '() '(("..." "")))]
         [end (if (= right *SIZE*) '() '(("..." "")))]
         [cells (for/list ([ref (in-range left right)])
                  (let* ([v (vector-ref (state-data a-state) ref)]
                         [v-str (number->string v)]
                         [nstr (if (= ptr ref) "^" " ")])
                    (list v-str nstr)))])
    (append (list (list "" (format "~a:" ptr)))
            begin cells end)))

(define (show-state a-state)
  (let* ([cells (get-state-near-cells a-state)]
         [normalized-cells
          (map (λ(cell)
                 ;(printf "mapping cell ~a\n" cell)
                 (let ([max-length (add1 (apply max (map string-length cell)))])
                   (map (λ(item)
                          (string-append
                           item
                           (make-string (- max-length (string-length item)) #\space)))
                        cell)))
               cells)])
    (displayln "")
    (displayln (apply string-append (map first normalized-cells)))
    (displayln (apply string-append (map second normalized-cells)))))
    

;; we know how to do loops!
(define-syntax-rule (loop a-state body ...)
  (let loop ()
    (unless (= (vector-ref (state-data a-state)
                           (state-ptr a-state))
               0)
      body ...
      (loop))))

;; Simple exercises.
(let ([s (new-state)])
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (increment-byte s)
  (check-equal? 2 (vector-ref (state-data s) 0))
  (decrement-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0)))
 
;; pointer movement
(let ([s (new-state)])
  (increment-ptr s)
  (increment-byte s)
  (check-equal? 0 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (decrement-ptr s)
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1)))
 
;; make sure standard input is doing something
(let ([s (new-state)])
  (parameterize ([current-input-port
                  (open-input-bytes (bytes 3 1 4))])
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s)
    (increment-ptr s)
    (read-byte-from-stdin s))
  (check-equal? 3 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (check-equal? 4 (vector-ref (state-data s) 2)))
 
 
;; make sure standard output is doing something
(let ([s (new-state)])
  (set-state-data! s (vector 80 76 84))
  (let ([simulated-stdout (open-output-string)])
    (parameterize ([current-output-port simulated-stdout])
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s)
      (increment-ptr s)
      (write-byte-to-stdout s))
    (check-equal? "PLT" (get-output-string simulated-stdout))))
 
 
;; Let's see that we can clear.
(let ([s (new-state)])
  (set-state-data! s (vector 0 29 92 14 243 1 6 92))
  (set-state-ptr! s 7)
  ;; [ [-] < ]
  (loop s
        (loop s (decrement-byte s))
        (decrement-ptr s))
 
  (check-equal? 0 (state-ptr s))
  (check-equal? (vector 0 0 0 0 0 0 0 0) (state-data s)))
