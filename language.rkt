#lang racket
 
(require "semantics.rkt")
 
(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         show
         #%app #%top-interaction ;(rename-out [my-app #%app])
         (rename-out [my-module-begin #%module-begin])
         #;(rename-out [top-interaction #%top-interaction]))

;; The current-state is a parameter used by the
;; rest of this language.
(define current-state (make-parameter (new-state)))

(define (top-interaction . stx)
  (displayln "Toplevel:")
  (displayln stx))

(define (my-app . stx)
  (displayln "App:")
  (displayln stx))
 
;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (current-state (new-state))
   body ...))
;;   (parameterize ([current-state (new-state)])
;;     body ...)))
;; We don't want this because we want our REPL to recycle the old state.
 
(define-syntax-rule (greater-than)
  (increment-ptr (current-state)))
 
(define-syntax-rule (less-than)
  (decrement-ptr (current-state)))
 
(define-syntax-rule (plus)
  (increment-byte (current-state)))
 
(define-syntax-rule (minus)
  (decrement-byte (current-state)))
 
(define-syntax-rule (period)
  (write-byte-to-stdout (current-state)))
 
(define-syntax-rule (comma)
  (read-byte-from-stdin (current-state)))
 
(define-syntax-rule (brackets body ...)
  (loop (current-state) body ...))

;; Displaying results
(define-syntax-rule (show)
  (show-state (current-state)))
  