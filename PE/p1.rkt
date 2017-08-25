#lang racket

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.

;; Find the sum of all the multiples of 3 or 5 below 1000.


;; fold seems pretty fast
(define (multiples x y mn mx)
  (for/list ([i (in-range mn mx)]
             #:when (or (= 0 (modulo i x)) (= 0 (modulo i y))))
    i))

(define (genAnswer x y mn mx) ((curry foldl + mn) (multiples x y mn mx)))
(define answer (genAnswer 3 5 0 1000))

;; Clever answer from the forums
;; We can sum a arbritrary numeric progress with (n/2)(a + l), where n is the
;; number of terms, a is the first term and l is the last term
(define (sumArbSeries n a l) (* (/ n 2) (+ a l)))

;; Then we can find the nth term like so, where d is the common difference. the
;; common difference is the number that separates the number is the series ex:
;; 3, 6, 9 has a d = 3, and x is the last number in the seq, 1000 in this prb
;; the math equation is, unsurprising a + (n - 1) * d, which gives
;; n = (x - a)/ d + 1 in general
(define (nth x a d) (/ (- x a) (+ d 1)))

;; for this problem we can simplify even further because d == a, which gives a +
;; (n - 1) * a = x / d = n. Now using nth we can find the last term by equation
;; last = a + (n - 1)* d = d*(int(x/d)), plugging this into sum we get Sum =
;; (x/2d)(d + d(int(x/d))), which simplifies to sum = d *
;; int(x/d)*(1+int(x/d))/2 so now we just need to sum the 3,6,9 series, the 5,10,15
;; series, and substract the 3*5 series: 15,30,45. The problem is explicit that
;; this is numbers < 1000, so we use x = 999
(define (sumSeries d x)
  (foldl * 1 (list d (exact-floor (/ x d)) (/ (+ 1 (exact-floor (/ x d))) 2))))

(define 3series (sumSeries 3 999))

(define 5series (sumSeries 5 999))

(define 15series (sumSeries 15 999))

(define goodAnswer (- (+ 3series 5series) 15series))

;; arbritary solution to this problem, assumes a = d
(define (arbAnswer f1 f2 mx)
  (let* ([_sumSeries (lambda (x) (sumSeries x (- mx 1)))])
    (- (+ (_sumSeries f1) (_sumSeries f2)) (_sumSeries (* f1 f2)))))
