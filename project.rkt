#lang racket

(require openssl/sha1)
(require math/number-theory)
(require math/base)

;(sha1 (open-input-bytes #""))

(define (ggen h)
  (let ((g (modular-expt h quot p)))
    (if (equal? g 1)
        (ggen (+ h 1))
        g)))

(define (next-p p po)
(let ((np (+ p 1)))
  (if (prime? np)
      np
      (next-p (+ p po) po))))

(define L 512)
(define N 150)

(define q 794789827360301020706490998086570468747482899)

(define p 49276969296338663283802441881367369062343939739)

(define quot (/ (- p 1) q))

(define g 4611686018427387904)

(define (key-gen)
  (define sec-key (random-natural q))
  (define pub-key (modular-expt g sec-key p))
  (cons sec-key pub-key)
  
  )



(define (dig-sign m x)

  (define k 0)
  (define (helper)
    (begin
      (set! k (random-natural q))
      (display k) (display "\n")
      (define r (modulo (modular-expt g k p) q))
      (if (equal? r 0)
          (helper)
          (let ((s (modulo (* (modular-inverse k q) (+ (* x r) (string->number (string-append "#x" (sha1 m))) ) ) q)))
            (if (equal? s 0)
                (helper)
                (cons r s)))
          )
      )
    )
        
  (helper)
  )

(define (verify m sign y)
  (define r (car sign))
  (define s (cdr sign))
  (if (not (and (< r q) (< s q)))
      #f
      (let* ((w (modular-inverse s q))
             (u1 (modulo (* w (string->number (string-append "#x" (sha1 m)))) q))
             (u2 (modulo (* r w) q))
             (v (modulo (* (modular-expt g u1 p) (modular-expt y u2 p) ) q)))
        (display v)(display "\n") (display r)
        (equal? v r))
      
      )
  )

  

(define kpair (key-gen))
(equal? 1 (modular-expt (cdr kpair) q p))
(define sign (dig-sign (open-input-bytes #"abc") (car kpair)) )

(verify (open-input-bytes #"abc") sign (cdr kpair))
