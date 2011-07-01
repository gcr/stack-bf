#lang racket/base
(require "reader.rkt")
(require (only-in "../language.rkt" show))

(provide (rename-out [get-lang-info get-info])
         configure)

;; Language info
(define (get-lang-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#((planet gcr/bf/lang/configure-runtime) configure #f))]
      [else
       default])))

;; For the REPL
(define read-interaction
  (λ(some-symbol port)
    ;(displayln "READING")
    (let ([x (my-read-syntax some-symbol port)])
      ;(displayln x)
      (when (eof-object? x)
        (show))
      x)))

(define (configure default)
  (current-read-interaction read-interaction))
     