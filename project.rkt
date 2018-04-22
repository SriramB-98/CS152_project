#lang racket

(require openssl/sha1)
(require math/number-theory)
(require math/base)
(require racket/bytes)

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

(define sign (dig-sign "abc" (car kpair)) )

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
        (let () (begin
          (define nonce (crypto-random-bytes hlength))
          (define trial (concat (sha (open-input-bytes (bytes-append nonce blockbyte)))) )
          (if (check trial nos)
              nonce
              (begin (set! iter (+ iter 1) ) (helper)))
          ))
        #f))
  (helper)
  )



(define (mine-on-list lminer time)
  (define nos 6)
  (define continue? #t)
  (define (helper mlist)
    (if (null? mlist)
        (helper lminer)
        (let ([mineval (mine-for (miner-cp x) (miner-block x) nos)])
          (if mineval
              (begin (do stuff) #t)
              (helper (cdr mlist))
              )
          )))
  (helper lminer)
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
  (if (= i 0)
      list
      (access (cdr list (- i 1)))))

(define (access-first list i)
  (if (= i 0)
      '()
      (cons (car list) (access (cdr list (- i 1))))))


(define (accept-new-payment blckchain trans)
  (define btoskip 3)
  (define req-blockchain (access blckchain btoskip))

  (define (helper chain)
    (if (null? chain)
        #f
        (if (member trans (block-ltrans (car chain)) )
            (set! btoskip 1);; insert your code
            (helper (cdr chain))
            )           
        )
    )
  (if (< (length blckchain) btoskip) #f (helper req-blockchain))
  )

(define (make-payment myid rcvid amt)
  ;(id pub inparray outarray digsig)
  (define total 0)
  (define inparray '())
  (define (take-ids)
    (if (< total amt)
        (begin (set! total (+ total (cdr (car readypayments))))
               (set! inparray (cons (car (car readypayments)) inparray))
               (set! readypayments (cdr readypayments))
               (take-ids)
               )
        total)
    )
  
  (define outarray (list (cons amt rcvid) (cons (- total amt) myid)))

  (define signature (dig-sign (concat (list pubid inparray outarray)) privkey) ) 
  (begin (take-ids) (trans '() pubid inparray outarray signature) )
  )

(define (Extension? blckchain message)
  (define bool (and (verify-nonce message nos) (equal? (block-hash message) (sha (open-input-bytes (bytes-append (block-nonce (car blckchain))
                                                                                                                 (concat (list (block-hash (car blckchain))
                                                                                                                               (block-ltrans (car blckchain))))))))) )
  (define newchain (cons message blckchain))
  (define (helper)
    (if (null? (block-ltrans (car newchain) )) (begin (set! forks (cons message forks)) #t)
        (let () (begin
                  (define top (car (block-ltrans (car newchain) )))
                  (set! newchain (cons (struct-copy block (car newchain) [ltrans (cdr (block-ltrans (car newchain) ))]) (cdr newchain)) ) 
                  (and (check-trans? top blckchain) (helper) )
                  ))
        )
    )
  (begin
    (set! newchain (cons (struct-copy block (car newchain) [ltrans (cdr (block-ltrans (car newchain) ))]) (cdr newchain)) )
    (and bool (helper)))
  )

(define (Extension_Fork? message)
  ;(define newforks (map (λ (x) (append x local-Blockchain)) forks))
  (define bool #t)
  (define (helper forklist)
    (if (null? forklist) (begin (set! bool #f) '())
        (if (Extension? (append (car forklist) local-Blockchain) message)
            (cons (cons message (car forklist)) (cdr forklist) )
            (cons (car forklist) (helper (cdr forklist))))))
  (begin (set! forks (helper forks)) bool )
  )
  
(define (safe-fork-available?)
  (define maxfork (argmax (λ (x) (length x)) forks))

  (if (equal? (length maxfork) 4)
      (begin
        ;;call extract-from-fork
        (set! forks '())
        (set! localblockchain (append maxfork localBlockchain)) 
        )
      #f)
  )


  ;;((tid elementnumber amt 1/0 : comfirmed with)  )
(define (extract-from-fork forks)
  (define btoskip 3)
  ;(define conf (access forks 3) )
  (define (filtertrans transaction)
    (index-where (trans-outarray transaction) (λ (x) (equal? (cdr x) pubid)) )
    )
  (define (iter-over-list ltrans val)
    (if (null? ltrans) '()
        (if (filtertrans (car ltrans))
            (let ((elnum (filtertrans (car ltrans)))
                  (oarr (trans-outarray (car ltrans)))
                  ( ))
              (cons (list (trans-id (car ltrans))
                          elnum
                          (car (list-ref oarr elnum))
                          val) (iter-over-list (cdr ltrans))))
            (iter-over-list (cdr ltrans)) ) 
        ))
  
  (define (helper listf val)
    (if (null? listf) '()
        (append (iter-over-list (block-ltrans (car listf)) val) (helper (cdr listf) val) ))
    )

  (append (helper (access forks btoskip) 1) (helper (access-first forks btoskip ) 0 ) 
 )

(if (equal? (length list-of-transactons) 10)
    
 )

;;(id pub inparray outarray digsig)

;; Extension? localblcchain message // done

;; safe fork available

;; can block be added to given blockchain? 

;; accept new payments / make new payments //done