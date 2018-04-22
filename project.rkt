#lang racket
(require openssl/sha1)
(require math/number-theory)
(require math/base)
(require racket/bytes)
(provide (all-defined-out))

(require racket/random)

(define sha sha1)
(define hlength 20)

(struct trans (id pub inparray outarray digsig) #:transparent )

(define (concat l)
  (cond [(integer? l) (int-to-bytes l)]
        [(pair? l) (bytes-append (car l) (cdr l) )]
        [(string? l) (int-to-bytes (string->number (string-append "#x" l)))]
        [(list? l) (if (null? l) #"" (bytes-append (concat (car l)) (concat (cdr l)) ))]
        [(trans? l) (bytes-append (concat (trans-id l)) (concat (trans-pub l)) (concat  (trans-inparray l)) (concat (trans-outarray l)) (concat (trans-digsig l)))]
        [else (error "Type not handled")] ))

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
          (let ((s (modulo (* (modular-inverse k q) (+ (* x r) (string->number (string-append "#x" (sha (open-input-bytes m)))) ) ) q)))
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
             (u1 (modulo (* w (string->number (string-append "#x" (sha (open-input-bytes m))))) q))
             (u2 (modulo (* r w) q))
             (v (modulo (modulo (* (modular-expt g u1 p) (modular-expt y u2 p) ) p) q)))
        
        (equal? v r))
      
      )
  )

  

(define kpair (key-gen))

(define sign (dig-sign #"abc" (car kpair)) )

;(verify (open-input-bytes #"abc") sign (cdr kpair))

(struct block (hash ltrans nonce) #:transparent)
(define (int-to-bytes x)
  (define (int-to-lbytes int)
    (if (= int 0)
        '()
        (cons (remainder int 256) (int-to-lbytes (quotient int 256)))))
  (list->bytes (int-to-lbytes x))
)
  



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
          (list nonce iter trial)
          (begin (set! iter (+ iter 1) ) (helper)))
      )
    )
  (helper)
  )


(define (mine-for cp block nos)
  (define hash (block-hash block))
  (define ltrans (block-ltrans block))
  (define blockbyte (concat (list hash ltrans)) ) 
  (define iter 0)
  (define (helper)
    (if (< iter cp)
        (let ()
          (begin
            (define nonce (crypto-random-bytes hlength))
            (define trial (concat (sha (open-input-bytes (bytes-append nonce blockbyte)))) )
            (if (check trial nos)
                (block hash ltrans nonce)
                (begin (set! iter (+ iter 1) ) (helper)))
          ))
        #f))
  (helper)
  )

(define (verify-nonce blck nos)
  (check (concat (sha (open-input-bytes (bytes-append (block-nonce blck)
                                                      (concat (list (block-hash blck)
                                                                    (block-ltrans blck)))))))
          nos))


(define (check-trans? trans blckchain)
  (define sum (foldr + 0 (map (λ (x) (car x)) (trans-outarray trans))) )
  (define ids (trans-inparray trans))
  (define leastid (argmin (λ x (car x)) ids))
  (define (helper blckch)
    (if (null? blckch)
        (equal? sum 0)
        (if (< (trans-id (car (block-ltrans (car blckch)))) leastid)
            (equal? sum 0)  
            (let ()
              (begin
                (define tlist (block-ltrans (car blckch)))
                (define tpresent?
                  (memf (λ (x) (memf (λ (y) (member y ids) ) (trans-inparray x) ) )
                        tlist))                
                (if (or (< sum 0) tpresent?)
                    #f
                    (begin
                       (for-each (λ (x)
                                  (let ()
                                    (begin
                                      (define isinput (filter (λ (y) (equal? (trans-pub trans) (cdr y)) ) (trans-outarray x) ))
                                      (for-each (λ (y) (set! sum (- sum (car y)))) isinput)
                                      )))
                                tlist)
                       (helper (cdr blckch))
                      ))       
                ))
            ))    
  )
  (and (helper blckchain) (verify (concat (list (trans-pub trans) (trans-inparray trans) (trans-outarray trans)))
                                  (trans-digsig trans) (trans-pub trans) ))
  )

(define (access list i)
  (if (<= i 0)
      list
      (access (cdr list (- i 1)))))

(define (access-first list i)
  (if (<= i 0)
      '()
      (cons (car list) (access (cdr list (- i 1))))))




