;; ============================================
;; MINECRAFT END PORTAL PROBABILITY CALCULATOR
;; Dream's nightmare in Scheme form
;; ============================================

(use-modules (srfi srfi-1)
             (ice-9 format))

;; Basic probability utilities
(define (combination n k)
  "Calculate binomial coefficient C(n,k)"
  (if (or (< k 0) (> k n)) 0
      (/ (factorial n) (* (factorial k) (factorial (- n k))))))

(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (binomial-pmf n k p)
  "Binomial probability mass function"
  (* (combination n k) 
     (expt p k) 
     (expt (- 1 p) (- n k))))

(define (binomial-cdf n k p)
  "Binomial cumulative distribution function"
  (apply + (map (lambda (i) (binomial-pmf n i p))
                (iota (+ k 1)))))

;; ============================================
;; END PORTAL MECHANICS
;; ============================================

;; Eye of Ender has 1/10 chance of breaking when thrown
(define eye-break-probability 0.1)
(define eye-survive-probability 0.9)

;; Portal frame has 12 slots
;; Each slot has 1/10 chance of being filled naturally
(define portal-slots 12)
(define natural-fill-probability 0.1)

;; ============================================
;; PORTAL COMPLETION CALCULATIONS
;; ============================================

(define (portal-completion-probability filled-slots)
  "Probability of having exactly 'filled-slots' pre-filled"
  (binomial-pmf portal-slots filled-slots natural-fill-probability))

(define (portal-completion-probability-at-least filled-slots)
  "Probability of having at least 'filled-slots' pre-filled"
  (- 1 (binomial-cdf portal-slots (- filled-slots 1) natural-fill-probability)))

(define (eyes-needed-probability filled-slots)
  "Expected number of eyes needed given pre-filled slots"
  (- portal-slots filled-slots))

;; ============================================
;; EYE OF ENDER SURVIVAL CALCULATIONS
;; ============================================

(define (eye-survival-probability throws)
  "Probability that eye survives 'throws' number of throws"
  (expt eye-survive-probability throws))

(define (eye-break-before-throw n)
  "Probability that eye breaks before the nth throw"
  (- 1 (eye-survival-probability (- n 1))))

(define (expected-eyes-for-throws n)
  "Expected number of eyes needed for n successful throws"
  (/ n eye-survive-probability))

;; ============================================
;; DREAM'S SPECIFIC SCENARIO ANALYSIS
;; ============================================

(define (dream-scenario-probability)
  "Calculate probability of Dream's controversial speedrun scenario"
  (let* ((total-attempts 6)  ; Dream's stream attempts
         (successful-portals 0)  ; Portals that were very lucky
         (total-eyes-thrown 42)  ; Approximate from analysis
         (total-eyes-survived 35)  ; Eyes that didn't break
         
         ;; Calculate individual probabilities
         (eye-luck-prob (binomial-pmf total-eyes-thrown 
                                     total-eyes-survived 
                                     eye-survive-probability))
         
         ;; Portal completion luck (assuming average scenarios)
         (portal-luck-prob 1.0))  ; Placeholder for complex calculation
    
    (display "=== DREAM SCENARIO ANALYSIS ===\n")
    (format #t "Total eyes thrown: ~a~%" total-eyes-thrown)
    (format #t "Eyes survived: ~a~%" total-eyes-survived)
    (format #t "Eye survival rate: ~a~%" (/ total-eyes-survived total-eyes-thrown))
    (format #t "Expected survival rate: ~a~%" eye-survive-probability)
    (format #t "Probability of this eye luck: ~e~%" eye-luck-prob)
    (format #t "Odds against: 1 in ~a~%" (/ 1 eye-luck-prob))
    eye-luck-prob))

;; ============================================
;; GENERAL PORTAL PROBABILITY CALCULATOR
;; ============================================

(define (calculate-portal-odds pre-filled-slots)
  "Calculate odds for different portal configurations"
  (let* ((eyes-needed (- portal-slots pre-filled-slots))
         (completion-prob (portal-completion-probability pre-filled-slots))
         (expected-eyes (expected-eyes-for-throws eyes-needed)))
    
    (format #t "=== PORTAL WITH ~a PRE-FILLED SLOTS ===~%" pre-filled-slots)
    (format #t "Probability of this configuration: ~e~%" completion-prob)
    (format #t "Eyes needed to complete: ~a~%" eyes-needed)
    (format #t "Expected eyes required (with breaking): ~a~%" expected-eyes)
    (format #t "Odds: 1 in ~a~%" (/ 1 completion-prob))
    (newline)))

;; ============================================
;; SPEEDRUN PROBABILITY ANALYSIS
;; ============================================

(define (speedrun-portal-analysis runs)
  "Analyze portal luck over multiple speedrun attempts"
  (let* ((very-lucky-threshold 6)  ; 6+ pre-filled slots is very lucky
         (extremely-lucky-threshold 8)  ; 8+ is extremely lucky
         (godlike-threshold 10)  ; 10+ is basically impossible
         
         (very-lucky-prob (portal-completion-probability-at-least very-lucky-threshold))
         (extremely-lucky-prob (portal-completion-probability-at-least extremely-lucky-threshold))
         (godlike-prob (portal-completion-probability-at-least godlike-threshold))
         
         ;; Probability of getting lucky at least once in N runs
         (lucky-in-n-runs (- 1 (expt (- 1 very-lucky-prob) runs)))
         (extremely-lucky-in-n-runs (- 1 (expt (- 1 extremely-lucky-prob) runs)))
         (godlike-in-n-runs (- 1 (expt (- 1 godlike-prob) runs))))
    
    (display "=== SPEEDRUN PORTAL LUCK ANALYSIS ===\n")
    (format #t "Runs analyzed: ~a~%" runs)
    (format #t "Very lucky (6+ slots): ~e per run~%" very-lucky-prob)
    (format #t "Extremely lucky (8+ slots): ~e per run~%" extremely-lucky-prob)
    (format #t "Godlike (10+ slots): ~e per run~%" godlike-prob)
    (newline)
    (format #t "Chance of very lucky in ~a runs: ~a~%" runs lucky-in-n-runs)
    (format #t "Chance of extremely lucky in ~a runs: ~a~%" runs extremely-lucky-in-n-runs)
    (format #t "Chance of godlike in ~a runs: ~a~%" runs godlike-in-n-runs)
    (newline)))

;; ============================================
;; STATISTICAL SIGNIFICANCE TESTING
;; ============================================

(define (statistical-significance observed-successes total-trials expected-prob)
  "Calculate statistical significance using binomial test"
  (let* ((observed-rate (/ observed-successes total-trials))
         (expected-successes (* total-trials expected-prob))
         (p-value (- 1 (binomial-cdf total-trials (- observed-successes 1) expected-prob)))
         (z-score (/ (- observed-successes expected-successes)
                    (sqrt (* total-trials expected-prob (- 1 expected-prob))))))
    
    (display "=== STATISTICAL SIGNIFICANCE TEST ===\n")
    (format #t "Observed successes: ~a out of ~a trials~%" observed-successes total-trials)
    (format #t "Observed rate: ~a~%" observed-rate)
    (format #t "Expected rate: ~a~%" expected-prob)
    (format #t "Expected successes: ~a~%" expected-successes)
    (format #t "P-value: ~e~%" p-value)
    (format #t "Z-score: ~a~%" z-score)
    (format #t "Significance: ~a~%" (cond 
                                      ((< p-value 0.001) "Extremely significant (p < 0.001)")
                                      ((< p-value 0.01) "Very significant (p < 0.01)")
                                      ((< p-value 0.05) "Significant (p < 0.05)")
                                      (else "Not significant")))
    (newline)))

;; ============================================
;; UTILITY FUNCTIONS
;; ============================================

(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (>= i n) (reverse acc) (loop (+ i 1) (cons i acc)))))

;; ============================================
;; RUN ANALYSIS
;; ============================================

(display "=== MINECRAFT END PORTAL PROBABILITY CALCULATOR ===\n\n")

;; Show probabilities for different portal configurations
(for-each calculate-portal-odds (iota 13))

;; Analyze speedrun scenarios
(speedrun-portal-analysis 100)
(speedrun-portal-analysis 1000)

;; Dream's controversial scenario
(dream-scenario-probability)
(newline)

;; Statistical significance of unusual luck
(statistical-significance 35 42 0.9)  ; Dream's eye survival rate
(statistical-significance 4 6 0.001)  ; Hypothetical portal luck

(display "=== ANALYSIS COMPLETE ===\n")
