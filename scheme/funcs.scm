(define foo (lambda () (display "foo!\n")))
(define bar (lambda () (display "bar!\n")))
(if #f (foo) (bar))  ; This will display "bar!\n"

(define (nd . args)
  (cond
   ((null? args) #t)
   ((null? (car args)) #f)
   (else (apply nd (cdr args)))))

(display (nd 1 1))

(define (squaresum a b)
    (+    (* a a)
        (* b b)
    )
)
(define x 2)
(define y 5)
(define z 1)
(define (sqsumof2 a b c)
  (cond ((and (> y x) (> z x))
         (squaresum y z))
        ((and (> x y) (> z y))
         (squaresum x z))
        (else
         (squaresum x y))))
(sqsumof2 2 3 4)


(define (sqrt x)

 (define (good-enough? guess last-guess)
 (< (percent-error last-guess guess) 0.0000001))

 (define (improve guess)
 (average guess (/ x guess)))

 (define (sqrt-iter guess last-guess)
 (if (good-enough? guess last-guess)
 guess
 (sqrt-iter (improve guess) guess)))

 (sqrt-iter (/ x 2.0) 0))

(define (average x y)
 (/ (+ x y) 2))

(define (percent-error approx exact)
 (/ (abs (- approx exact)) (abs exact)))

 
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(factorial 5)
