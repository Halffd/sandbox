;; ============================================
;; BAYESIAN STATISTICS IN SCHEME (GUILE-COMPATIBLE)
;; ============================================

(use-modules (srfi srfi-1)
             (ice-9 format))

;; Basic list utilities
(define (sum lst)
  (if (null? lst) 0 (+ (car lst) (sum (cdr lst)))))

(define (mean lst)
  (/ (sum lst) (length lst)))

(define (variance lst)
  (let ((mu (mean lst)))
    (mean (map (lambda (x) (expt (- x mu) 2)) lst))))

(define (std-dev lst)
  (sqrt (variance lst)))

;; Utility functions first (dependencies)
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (gamma-function x)
  ;; Simplified gamma function using Stirling's approximation
  (cond 
    ((< x 1) (/ (gamma-function (+ x 1)) x))
    ((= x 1) 1)
    (else (* (sqrt (/ (* 2 3.14159) x)) 
             (expt (/ x 2.71828) x)))))

(define (beta-function a b)
  (/ (* (gamma-function a) (gamma-function b)) 
     (gamma-function (+ a b))))

(define (binomial-coefficient n k)
  (if (or (< k 0) (> k n)) 0
      (/ (factorial n) (* (factorial k) (factorial (- n k))))))

(define (quantile samples p)
  (let ((sorted (sort samples <))
        (n (length samples)))
    (list-ref sorted (inexact->exact (floor (* p n))))))

(define (my-iota n)
  (let loop ((i 0) (acc '()))
    (if (>= i n) (reverse acc) (loop (+ i 1) (cons i acc)))))

(define (my-take lst n)
  (if (or (null? lst) (= n 0)) '() 
      (cons (car lst) (my-take (cdr lst) (- n 1)))))

(define (remove-index lst i)
  (let loop ((lst lst) (j 0) (acc '()))
    (cond ((null? lst) (reverse acc))
          ((= j i) (loop (cdr lst) (+ j 1) acc))
          (else (loop (cdr lst) (+ j 1) (cons (car lst) acc))))))

;; Random number generation using Guile's random
(define (random-uniform)
  (/ (random 1000000) 1000000.0))

(define (random-normal mu sigma)
  ;; Box-Muller transform
  (let* ((u1 (random-uniform))
         (u2 (random-uniform))
         (z0 (* (sqrt (* -2 (log u1))) 
                (cos (* 2 3.14159 u2)))))
    (+ mu (* sigma z0))))

(define (random-gamma alpha)
  ;; Simplified gamma sampler for alpha > 1
  (if (< alpha 1)
      (* (random-gamma (+ alpha 1)) (expt (random-uniform) (/ 1 alpha)))
      ;; Simple accept-reject for alpha >= 1
      (let loop ()
        (let* ((u (random-uniform))
               (v (random-uniform))
               (x (- alpha 1))
               (y (* x (/ u (- 1 u)))))
          (if (< (* v v v u) (exp (- y x (log u))))
              (+ x y)
              (loop))))))

(define (random-beta alpha beta)
  (let ((x (random-gamma alpha))
        (y (random-gamma beta)))
    (/ x (+ x y))))

;; Prior distributions
(define (normal-prior mu sigma)
  (lambda (x) 
    (/ (exp (- (/ (expt (- x mu) 2) (* 2 sigma sigma))))
       (* sigma (sqrt (* 2 3.14159))))))

(define (beta-prior alpha beta)
  (lambda (x) 
    (if (and (>= x 0) (<= x 1))
        (/ (* (expt x (- alpha 1)) (expt (- 1 x) (- beta 1)))
           (beta-function alpha beta))
        0)))

(define (gamma-prior alpha beta)
  (lambda (x)
    (if (>= x 0)
        (/ (* (expt beta alpha) (expt x (- alpha 1)) (exp (- (* beta x))))
           (gamma-function alpha))
        0)))

;; Likelihood functions
(define (normal-likelihood data mu sigma)
  (let ((n (length data))
        (sum-sq (sum (map (lambda (x) (expt (- x mu) 2)) data))))
    (/ (exp (- (/ sum-sq (* 2 sigma sigma))))
       (expt (* sigma (sqrt (* 2 3.14159))) n))))

(define (binomial-likelihood n k theta)
  (* (binomial-coefficient n k) 
     (expt theta k) 
     (expt (- 1 theta) (- n k))))

(define (poisson-likelihood k lambda)
  (/ (* (expt lambda k) (exp (- lambda)))
     (factorial k)))

;; Posterior computation
(define (posterior prior likelihood)
  (lambda (theta) (* (prior theta) (likelihood theta))))

;; Metropolis-Hastings sampler
(define (metropolis-hastings log-posterior proposal-fn initial n-samples)
  (let loop ((current initial) 
             (samples '()) 
             (accepted 0) 
             (n 0))
    (if (>= n n-samples)
        (list (reverse samples) (/ accepted n-samples))
        (let* ((candidate (proposal-fn current))
               (log-alpha (min 0 (- (log-posterior candidate) 
                                   (log-posterior current))))
               (accept? (< (log (random-uniform)) log-alpha)))
          (loop (if accept? candidate current)
                (cons (if accept? candidate current) samples)
                (if accept? (+ accepted 1) accepted)
                (+ n 1))))))

;; Random walk proposal
(define (random-walk-proposal sigma)
  (lambda (x) (+ x (random-normal 0 sigma))))

;; Conjugate updates
(define (beta-binomial-update alpha beta n k)
  (list (+ alpha k) (+ beta n (- k))))

(define (normal-normal-update mu0 sigma0 data sigma)
  (let* ((n (length data))
         (data-mean (mean data))
         (precision0 (/ 1 (* sigma0 sigma0)))
         (precision-data (/ n (* sigma sigma)))
         (new-precision (+ precision0 precision-data))
         (new-mu (/ (+ (* precision0 mu0) 
                      (* precision-data data-mean))
                   new-precision))
         (new-sigma (sqrt (/ 1 new-precision))))
    (list new-mu new-sigma)))

(define (gamma-poisson-update alpha beta data)
  (let ((n (length data))
        (sum-data (sum data)))
    (list (+ alpha sum-data) (+ beta n))))

;; Autocorrelation function
(define (autocorrelation samples)
  (let* ((n (length samples))
         (mu (mean samples))
         (var (variance samples))
         (max-lag (min 50 (floor (/ n 4)))))
    (map (lambda (lag)
           (if (>= (+ lag 1) n) 0
               (/ (mean (map (lambda (i)
                            (* (- (list-ref samples i) mu)
                               (- (list-ref samples (+ i lag)) mu)))
                          (my-iota (- n lag))))
                  var)))
         (my-iota max-lag))))

;; Effective sample size
(define (effective-sample-size samples)
  (let* ((n (length samples))
         (autocorrs (autocorrelation samples))
         (tau (* 2 (sum (my-take autocorrs (min 10 (length autocorrs)))))))
    (/ n (+ 1 (* 2 tau)))))

;; Examples with corrected format calls
(define (coin-example)
  (let* ((data '(1 0 1 1 0 1 0 1 1 1))
         (n (length data))
         (k (sum data))
         (alpha-prior 1)
         (beta-prior 1)
         (updated-params (beta-binomial-update alpha-prior beta-prior n k))
         (samples (map (lambda (x) (random-beta (car updated-params) 
                                               (cadr updated-params)))
                      (my-iota 1000))))
    (display "Coin flip example (Beta-Binomial):\n")
    (format #t "Prior: Beta(~a, ~a)~%" alpha-prior beta-prior)
    (format #t "Data: ~a heads out of ~a flips~%" k n)
    (format #t "Posterior: Beta(~a, ~a)~%" 
            (car updated-params) (cadr updated-params))
    (format #t "Posterior mean: ~a~%" (mean samples))
    (format #t "95% credible interval: [~a, ~a]~%"
            (quantile samples 0.025) (quantile samples 0.975))))

(define (normal-example)
  (let* ((data '(1.2 0.8 1.5 0.9 1.1 1.3 0.7 1.4))
         (sigma 0.5)
         (mu0 1.0)
         (sigma0 1.0)
         (updated-params (normal-normal-update mu0 sigma0 data sigma))
         (samples (map (lambda (x) (random-normal (car updated-params) 
                                                 (cadr updated-params)))
                      (my-iota 1000))))
    (display "Normal mean estimation:\n")
    (format #t "Prior: N(~a, ~a²)~%" mu0 sigma0)
    (format #t "Data: ~a observations~%" (length data))
    (format #t "Posterior: N(~a, ~a²)~%" 
            (car updated-params) (cadr updated-params))
    (format #t "Posterior mean: ~a~%" (mean samples))))

(define (mcmc-example)
  (let* ((data '(2 1 3 0 1 2 1 0 1 2))
         (log-posterior (lambda (lambda-param)
                         (if (> lambda-param 0)
                             (+ (log ((gamma-prior 2 1) lambda-param))
                                (sum (map (lambda (x) 
                                           (log (poisson-likelihood x lambda-param)))
                                         data)))
                             -1000000)))
         (proposal (random-walk-proposal 0.5))
         (result (metropolis-hastings log-posterior proposal 1.0 1000))
         (samples (car result))
         (acceptance-rate (cadr result)))
    (display "MCMC example (Poisson-Gamma):\n")
    (format #t "Acceptance rate: ~a~%" acceptance-rate)
    (format #t "Posterior mean: ~a~%" (mean samples))
    (format #t "Effective sample size: ~a~%" 
            (effective-sample-size samples))))

;; Run examples
(display "=== BAYESIAN STATISTICS IN SCHEME ===\n\n")
(coin-example)
(newline)
(normal-example)
(newline)
(mcmc-example)
(display "\n=== DONE ===\n")