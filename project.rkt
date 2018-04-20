#lang racket

(require openssl/sha1)
(require math/number-theory)
(require math/base)
(require racket/bytes)

(require racket/random)

(define sha sha1)
(define hlength 20) 


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
      
      (define r (modulo (modular-expt g k p) q))
      (if (equal? r 0)
          (helper)
          (let ((s (modulo (* (modular-inverse k q) (+ (* x r) (string->number (string-append "#x" (sha m))) ) ) q)))
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
             (u1 (modulo (* w (string->number (string-append "#x" (sha m)))) q))
             (u2 (modulo (* r w) q))
             (v (modulo (modulo (* (modular-expt g u1 p) (modular-expt y u2 p) ) p) q)))
        
        (equal? v r))
      
      )
  )

  

(define kpair (key-gen))

(define sign (dig-sign (open-input-bytes #"abc") (car kpair)) )

;(verify (open-input-bytes #"abc") sign (cdr kpair))

(struct block (hash ltrans nonce) #:transparent)
(define (int-to-bytes x)
(define (int-to-lbytes int)
  (if (= int 0)
      '()
      (cons (remainder int 256) (int-to-lbytes (quotient int 256)))))

  (list->bytes (int-to-lbytes x))
)
  

(define (concat l)
  (cond [(integer? l) (int-to-bytes l)]
        [(string? l) (int-to-bytes (string->number (string-append "#x" l)))]
        [(list? l) (if (null? l) #"" (bytes-append (concat (car l)) (concat (cdr l)) ))]
        [else (error "Type not handled")] ))

(define (check bytestr n)
    (if (> n 8)
        (and (equal? (bytes-ref bytestr 0) 0) (check (subbytes bytestr 1) (- n 8)) )
        (< (bytes-ref bytestr 0) (expt 2 (- 8 n)))))

(define (mine hash ltrans nos)
  (define blockbyte (concat (list hash ltrans)) )
  
  (define iter 0)
  (define (helper)
    (begin
      (define nonce (crypto-random-bytes hlength))
      (define trial (concat (sha (open-input-bytes (bytes-append nonce blockbyte)))) )
      (if (check trial nos)
          (cons nonce iter trial)
          (begin (set! iter (+ iter 1) ) (helper)))
      )
    )
  (helper)
  )


(define (verify-nonce blck nos)
  (check (concat (sha (open-input-bytes (bytes-append (block-nonce blck)
                                                      (concat (list (block-hash blck)
                                                                    (block-ltrans blck)))))))
          nos))

;(struct block (hash ltans nonce) #:transparent)
      
      
  



