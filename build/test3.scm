#!r6rs

#| === BEGIN PRELUDE === |#
(import (rnrs))
(define (noteq a b) (not (equal? a b)))
(define (fnpipe x f) (f x))
(define print display)
(define print_int display)
(define print_float display)
#| === END PRELUDE === |#

(define (main) (begin (let ((incr (lambda (x) (+ x 1)))) (let ((decr (lambda (x) (- x 1)))) (let ((id (compose incr decr))) (fnpipe (id 123) print_int) 'unit)))))
(define (compose f g) (lambda (x) (f (g x))))
(main)
