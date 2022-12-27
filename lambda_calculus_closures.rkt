#lang racket
(require test-engine/racket-tests)

(define (expr? e)
  (match e
    [(? number? n) #t]
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    ))

(define (interp expr env)
  (match expr
    [(? number? n) n]
    [(? symbol? x) (hash-ref env x)]
    [`(lambda (,x) ,e) `(closure ,expr ,env)]

    [`(,e0 ,e1)
     (let ([clo-to-apply (interp e0 env)])
       (match clo-to-apply
         [`(closure (lambda (,x) ,e-body) ,env+) (interp e-body (hash-set env+ x (interp e1 env)))]
         [_ (error "expected to apply a closure")]))]))

(check-expect (expr? '7) #t)
(check-expect (interp '8 (hash)) 8)
(check-expect (expr? 'x) #t)
(check-expect (interp 'x (hash-set (hash) 'x 10)) 10)
(check-expect (interp 'x (hash 'x 11)) 11)
;(interp '(lambda (x) x) (hash 'x 12))
;(interp '(lambda (x) x) (hash-set (hash) 'x 10))
;(interp '((lambda (x) x) 10) (hash))
;(interp '((lambda (x) 6) 10) (hash))
;(interp '((lambda (x) y) 10) (hash 'y 12))