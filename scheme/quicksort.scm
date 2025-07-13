(define (last xs)
  (if (null? (cdr xs)) (car xs) (last (cdr xs))))

(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc)))))
(define (fake-shuffle xs)
  (let* ((half (quotient (length xs) 2))
         (ys (take xs half))
         (zs (drop xs half)))
    (interleave ys zs)))

(define (take xs n)
  (if (or (zero? n) (null? xs)) '()
      (cons (car xs) (take (cdr xs) (- n 1)))))

(define (drop xs n)
  (if (or (zero? n) (null? xs)) xs
      (drop (cdr xs) (- n 1))))

(define (interleave xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        (else (cons (car xs) (interleave ys (cdr xs))))))
(define (qsort lst)
  (define (qsort-tail lst acc)
    (cond
      ((null? lst) acc)
      (else
       (let* ((pivot (car lst))
              (rest (cdr lst))
              (smaller (filter (lambda (x) (<= x pivot)) rest))
              (larger  (filter (lambda (x) (> x pivot)) rest)))
         (qsort-tail smaller (cons pivot (qsort-tail larger acc)))))))
  (qsort-tail lst '()))

(define sorted (qsort '(3 1 4 1 5 9 2 6 5)))
(display sorted) ; (1 1 2 3 4 5 5 6 9)
(newline)

(define big (iota 10000))
(display (car (qsort big))) ; 0
(newline)
(display (last (qsort big))) ; 9999
(newline)
