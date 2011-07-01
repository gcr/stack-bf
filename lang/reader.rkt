#lang s-exp syntax/module-reader 
#:language '(planet gcr/bf/language)
#:read my-read
#:read-syntax my-read-syntax
#:language-info '#((planet gcr/bf/lang/configure-runtime) get-info #f)
 
(require "../parser.rkt")
 
(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))
 
(define (my-read-syntax src in)
  (parse-expr src in))

(provide my-read my-read-syntax)
