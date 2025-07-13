;; Function that returns animal-specific behavior
(define (make-animal type)
  (case type
    ((cat) (lambda (action)
             (case action
               ((speak) "Meow!")
               ((type) 'cat))))
    ((dog) (lambda (action)
             (case action
               ((speak) "Woof!")
               ((type) 'dog))))
    (else (error "Unknown animal type"))))

;; Generic lets-hear function
(define (lets-hear animal)
  (display (animal 'speak))
  (newline))

;; Usage
(define my-cat (make-animal 'cat))
(define my-dog (make-animal 'dog))

(lets-hear my-cat)  ; prints "Meow!"
(lets-hear my-dog)  ; prints "Woof!"