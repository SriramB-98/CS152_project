#lang racket
;(require "project.rkt")
(require data/heap)
(require racket/class)
(provide (all-defined-out))

(struct event (process time) #:transparent)
(define no_of_zeros 6)
(define minerlist (list ))
(define block-reward 10)
(define Max_forklength 2)
(define keytoname (make-hash))

(define-syntax-rule (for-loop [sym init check change] steps ...)
  (let loop ([sym init]
             [value #f])
    (if check
        (let ([new-value (let () steps ...)])
          (loop change new-value))
        value)))


(require openssl/sha1)
(require math/number-theory)
(require math/base)
(require racket/bytes)


(require racket/random)
(define sha sha1)
(define hlength 20)

(struct trans (id pub inparray outarray digsig) #:transparent )

(define (concat l)
  (begin
    ;(newline)
    ;(display l)
    ;(display "This is l !!")
    ;(newline)
    (cond
      [(equal? l "") #""]
      [(integer? l) (int-to-bytes l)]
      [(string? l) (int-to-bytes (string->number (string-append "#x" l)))]
      [(list? l) (if (null? l) #"" (bytes-append (concat (car l)) (concat (cdr l)) ))]
      [(pair? l) (bytes-append (concat (car l)) (concat (cdr l)) )]
      [(trans? l) (bytes-append (concat (trans-id l)) (concat (trans-pub l)) (concat  (trans-inparray l)) (concat (trans-outarray l)) (concat (trans-digsig l)))]
      [else (begin
              ;(display l)
              (error "Type not handled"))] ))
  )

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
(define sign (dig-sign #"abc" (car kpair)))

;(verify (open-input-bytes #"abc") sign (cdr kpair))

(struct block (hash ltrans nonce) #:transparent)
(define (int-to-bytes x)
  (define (int-to-lbytes int)
    (if (= int 0)
        (list)
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
          (block hash ltrans nonce)
          (begin (set! iter (+ iter 1) ) (helper)))
      )
    )
  (helper)
  )


(define (mine-for cp block22 nos)
  (define hash (block-hash block22))
  (define ltrans (block-ltrans block22))
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
  (define ids-2 (map (λ (x) (car x)) ids)) 
  ;(define leastid (argmin (λ (x) (car x)) ids))
  (define (helper blckch)
    (if (null? blckch)
        (begin
          ;(display sum)
          ;(display "Sum !!")
          ;(newline)
          ;(display trans)
          ;(newline)
          ;(newline)
          (equal? sum 0))
          
  ;      (if (< (trans-id (car (block-ltrans (car blckch)))) leastid)
   ;         (equal? sum 0)  
            (let ()
              (begin
                (define tlist (block-ltrans (car blckch)))
                (define tpresent?
                  (memf (λ (x) (memf (λ (y) (member y ids) ) (trans-inparray x) ) )
                        tlist))
                ;(display "entering !!")
                ;(newline)
                (if (or (< sum 0) tpresent?)
                    #f
                    (begin
                      ;(display sum)
                      ;(newline)
                      ;(display "Sum !!")
                      ;(newline)
                       (for-each (λ (x)
                                  (let ()
                                    (begin
                                      (define isinput (filter (λ (y) (equal? (trans-pub trans) (cdr y)) ) (trans-outarray x) ))
                                      ;(display isinput)
                                      (for-each (λ (y) (set! sum (- sum (car y)))) isinput)
                                      )))
                                (filter (λ (x) (member (trans-id x) ids-2)) tlist))
                       (helper (cdr blckch))
                      ))       
                ))
            )    
  )
  (begin
    ;(display "CHeck-Trans")
    ;(display (verify (concat (list (trans-id trans) (trans-pub trans) (trans-inparray trans) (trans-outarray trans)))
                                 ; (trans-digsig trans) (trans-pub trans) ))
    ;(newline)
    (and (helper blckchain) (verify (concat (list (trans-id trans) (trans-pub trans) (trans-inparray trans) (trans-outarray trans)))
                                  (trans-digsig trans) (trans-pub trans) ))
  ))

(define (access list i)
  (if (<= i 0)
      list
      (access (cdr list) (- i 1))))

(define (access-first list2 i)
  (if (<= i 0)
      (list)
      (cons (car list2) (access (cdr list2) (- i 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global_events%
  (class object%
    (init-field (events-list (make-heap (lambda (x y) (< (event-time x) (event-time y)))))) 
    (init-field (global_timer 0))
    
    (super-new)

    (define/public (insert_event process1 time1)
      (begin
        (define event-new (event process1 time1))
        (heap-add! events-list event-new)))
        
    (define/public (process)
      (cond
        [(= (heap-count events-list) 0) (let* [(temp-minerlist (filter (lambda (x) (send x make-block (get-field pending-trans x) global_timer)) minerlist))]
                                          (begin
                                            ;(display minerlist)
                                            ;(newline)
                                            ;(display temp-minerlist)
                                            ;(display " 2 ")
                                            ;(newline)
                                            (set! global_timer (+ 1 global_timer))  
                                            (mine-on-list 1 no_of_zeros temp-minerlist global_timer)
                                            (if (null? temp-minerlist)
                                                (begin
                                                  (newline)
                                                  (display "Fin Cayo")
                                                )
                                                (process))))]
        [else (let* [(event2 (heap-min events-list))
                     ]
                (begin
                  (heap-remove-min! events-list)
                  (if (< global_timer (event-time event2))
                      (let* [(temp-minerlist (filter (lambda (x) (send x make-block (get-field pending-trans x) global_timer)) minerlist))] (begin
                                ;(display minerlist)
                                ;(newline)
                                ;(display temp-minerlist)
                                ;(display " 2 ")
                                (newline)
                                ;(define temp-minerlist )
                                (mine-on-list (- (event-time event2) global_timer) no_of_zeros temp-minerlist global_timer)
                                (set! global_timer (event-time event2))
                                ((event-process event2))
                                (process)))
                      (begin
                        ((event-process event2))
                        (process)))))]))))

(define globaleventslist (make-object global_events%))


(define node%
  (class object%
    (init-field (nodeidentifier ""))
    (init-field (nodename 0))
    (hash-set! keytoname nodename nodeidentifier)
    (init (initial-connections '()))
    (define connections initial-connections)
    (init-field (private-key 1))
    (init-field (localtime 0))
    (init (initial_Blockchain (list )))
    ;Block chain initializer
    (define local_Blockchain initial_Blockchain)
    ;Initial accepted Blockchain (for starting the simulation)
    (init (init-total (list )))
    (define total-accepted-payments init-total)    
    ;List of Transactions in which payment is received and the Transaction is confirmed.
    (init (init-received (list )))
    (define received-payments-to-be-used init-received)
    ;A List of pairs. Each element of the list is pair whose first element is again a pair
    ;and second element is the amount received by this node.
    ;First element is a pair whose First element in turn is the transaction ID
    ;and Second element is the output array index of the same transaction in which given
    ;node receives money
    ;Transactions referenced here have been confirmed but not used again.
    (define pending-payments-to-confirm (list ))
    ;List of Transactions in which payments are received but not yet confirmed by the blockchain.
    (define pending-payments-to-be-made (list ))
    ;List of lists, each element list consists of a nodename and amount
    ;indicating payment of "amount" has yet to be done to the given "nodename" 
    (define forks (list ))
    ;Simply a list of future Blockchain extensions the node has heard.
    (super-new)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (add-new-connection node1 link-delay1)
      (set! connections (cons (list node1 link-delay1) connections)))
    (define/public (delete-connection node1)
      (set! connections (filter (lambda (x) (not (= (car x) node1))) connections)))

    (define/public (edit-connection node1 link_delay_new)
      (set! connections (map (lambda (x)
                               (if (equal? (car x) node1)
                                   (list node1 link_delay_new)
                                   (list (car x) (cadr x))))
                             connections)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (Display-payment-status bool nodename1 amount2 nodename2 time2)
      (if bool
          (begin
            (newline)
            (display "Payment made: ")
            (display amount2)
            (display " By ")
            (display (hash-ref keytoname nodename1))
            (display " To ")
            (display (hash-ref keytoname nodename2))
            (display " Time: ")
            (display time2)
            (newline))
          (begin
           (newline)
           (display "Attempt to pay failed: ")
           (display amount2)
           (display " By ")
           (display (hash-ref keytoname nodename1))
           (display " To ")
           (display (hash-ref keytoname nodename2))
           (display " Time: ")
           (display time2)
           (newline))))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (broadcast message time)
      (define (send-msg conn msg time)
        (cond [(null? conn) (display "Message Broadcasted: time:")
                            ;(newline)
                            (display (get-field global_timer globaleventslist))
                            (newline)
                            (newline)]
              [(list? conn) (begin
                              ;(display "Broadcasting...")
                              (define (f) (send (car (car conn)) receive msg (+ (cadr (car conn)) time)))
                              (send globaleventslist insert_event f (+ (cadr (car conn)) time))
                              ;(display (heap->vector (get-field events-list globaleventslist))) 
                              ;(newline)
                              (newline)
                              (newline)       
                              (send-msg (cdr conn) msg time))]
              [else (begin
                     (display "Connections of invalid type")
                    (newline))]))
      (send-msg connections message time))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (receive message time)
      (begin
        (display "A Message is received by : ")
        (newline)
        (display nodeidentifier)
        (newline)
        (cond [(block? message) (cond [(Extension? local_Blockchain message)
                                       (begin
                                         (display "New Fork added for node : ")
                                         (newline)
                                         (display nodeidentifier)
                                         (newline)
                                         ;(display (sha (open-input-bytes (bytes-append (block-nonce (car local_Blockchain))
                                          ;                                                                       (concat (list (block-hash (car local_Blockchain))
                                           ;                                                                                    (block-ltrans (car local_Blockchain))))))))
                                         ;(newline)
                                         ;(display (block-hash message))
                                         ;(newline)
                                         ;(newline)
                                         #t)]
                                      [(Extension_Fork? message) (begin
                                                                   ;(set! Forks (Update_Forks Forks message))
                                                                   (display "Checking if fork is extended for node : ")
                                                                   (newline)
                                                                   (display nodeidentifier)
                                                                   (newline)
                                                                   (if
                                                                    (Safe_Fork_Available? time)
                                                                    (begin
                                                                      (display "Fork is now used to update the blockchain for node : ")
                                                                      (newline)
                                                                      (display nodeidentifier)
                                                                      (newline)
                                                                      (display "My Blockchain has been updated!!")
                                                                      (newline)
                                                                      (newline)
                                                                      (Accept-New-Payment time)
                                                                      (Make-Old-Payments time)
                                                                      )
                                                                    (void)))]
                                      [else (display "Block rejected by: ")
                                            (display nodeidentifier)
                                            (newline)
                                            (display "Time: ")
                                            (display time)
                                            (newline)])]
              [else (begin
                      (display "Received a non block message Time: ")
                      (display time)
                      (newline)
                      (newline))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (Make-payment new nodename2 amount2 time2)
      (define inparray (list ))
      (define total 0)
      (define (take-ids)
        (if (< total amount2)
            (begin (set! total (+ total (cdr (car received-payments-to-be-used))))
                   (set! inparray (cons (car (car received-payments-to-be-used)) inparray))
                   (set! received-payments-to-be-used (cdr received-payments-to-be-used))
                   (take-ids))
             total))      
      (cond [(null? received-payments-to-be-used) (if (= new 1)
                                                      (begin
                                                        (Display-payment-status #f nodename amount2 nodename2 time2)
                                                        (set! pending-payments-to-be-made
                                                              (cons (list nodename2 amount2) pending-payments-to-be-made))
                                                        #f
                                                        )
                                                      (begin
                                                        (Display-payment-status #f nodename amount2 nodename2 time2)
                                                        #f))]
            [(< (foldr + 0 (map (lambda (x) (cdr x)) received-payments-to-be-used)) amount2) (if (= new 1)
                                                                      (begin
                                                                       (Display-payment-status #f nodename amount2 nodename2 time2) 
                                                                       (set! pending-payments-to-be-made
                                                                             (cons (list nodename2 amount2) pending-payments-to-be-made))
                                                                       #f
                                                                       )
                                                                     (begin
                                                                       (Display-payment-status #f nodename amount2 nodename2 time2)
                                                                       #f))]
            [else (begin
                    (take-ids)
                    (let* [(outarray (list (cons amount2 nodename2) (cons (- total amount2) nodename)))
                           (transid (string->number (string-append (~v (get-field global_timer globaleventslist) ) (~v nodename))))
                           (signature (dig-sign (concat (list transid nodename inparray outarray)) private-key))                      
                           (transaction_done (trans transid nodename inparray outarray signature))]                  
                     (begin
                       (Display-payment-status #t nodename amount2 nodename2 time2)
                       (newline)
                       (broadcast transaction_done time2)
                       #t
                       )))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (attempt_transaction nodename2 amount2 time)
    (Make-payment 1 nodename2 amount2 time))

  (define/public (Make-Old-Payments time)
     (set! pending-payments-to-be-made (filter (lambda (x) (let* [(nodename2 (car x))
                                                                  (amount2 (cadr x))]
                                                             (not (Make-payment 0 nodename2 amount2 time)))) pending-payments-to-be-made)))   
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (accept-new-payment blckchain trans11 time2)
    (define btoskip 0)
    (define (check)
      (if (= (cdr (car (trans-outarray trans11))) nodename)
          0
          (if (= (cdr (cadr (trans-outarray trans11))) nodename)
              1
              (begin
                (display "Wroong Transaction in redeem list!!!")
                #f))))
    (define (helper chain)
      (if (null? chain)
          #f
          (if (member trans11 (block-ltrans (car chain)))
              (let*  [(new_trans_used (cons (cons (trans-id trans11) (check))
                                            (if (= (check) 0)
                                                (car (car (trans-outarray trans11)))
                                                (if (= (check) 1)
                                                    (car (cadr (trans-outarray trans11)))
                                                    (void)))))]
                (begin
                  (set! received-payments-to-be-used (cons new_trans_used received-payments-to-be-used))
                  (set! total-accepted-payments (cons trans11 total-accepted-payments))
                  (display "Payment of : ")
                  (display (cdr (car received-payments-to-be-used)))
                  (display "  bitcoins accepted by node ")
                  (newline)
                  (display nodeidentifier)
                  (display " Time: ")
                  (display time2)
                  (newline)
                  #t))
              (helper (cdr chain))
              )           
          )
      )
    (if (< (length blckchain) btoskip)
        #f
        (let* [(req-blockchain (access blckchain btoskip))] 
          (helper req-blockchain)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define/public (Accept-New-Payment time2)
     (set! pending-payments-to-confirm (filter (lambda (x) (accept-new-payment local_Blockchain x time2))
                                                pending-payments-to-confirm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/public (Extension_Fork? message)
  ;(define newforks (map (λ (x) (append x local-Blockchain)) forks))
  (define bool #t)
  (define (helper forklist)
    (if (null? forklist) (begin (set! bool #f) (list ))
        (if (Extension? (append (car forklist) local_Blockchain) message)
            (cons (cons message (car forklist)) (cdr forklist) )
            (cons (car forklist) (helper (cdr forklist))))))
  (begin (set! forks (helper forks)) bool )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define/public (Safe_Fork_Available? time2)
  (define maxfork (argmax (λ (x) (length x)) forks))
  (if (equal? (length maxfork) Max_forklength)
        (let* [(trans-temp (extract-from-fork-new maxfork))
               (trans-temp2 (extract-from-fork maxfork))
               (new_accepted_trans (car trans-temp))
               (new_accepted_trans2 (car trans-temp2))
               (new_pending_confirmations (cdr trans-temp))]
          (begin
            (map (lambda (x) (begin
                               (display "Payment of : ")
                               (display (caddr x))
                               (display " bitcoins accepted by node ")
                               (display nodeidentifier)
                               (display " Time: ")
                               (display time2)
                               (newline)
                               (void))) new_accepted_trans2)
          
            (set! total-accepted-payments (append new_accepted_trans total-accepted-payments))
            (set! received-payments-to-be-used (append (map (lambda (x) (cons (cons
                                                                               (car x)
                                                                               (cadr x))
                                                                              (caddr x)))
                                                            new_accepted_trans2)
                                                       received-payments-to-be-used))
            (set! pending-payments-to-confirm  (append pending-payments-to-confirm
                                                       new_pending_confirmations))
            (set! forks (list ))
            (set! local_Blockchain (append maxfork local_Blockchain))) 
        )
      #f)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (Extension? blckchain message)
  (define bool (and (verify-nonce message no_of_zeros) (equal? (block-hash message) (sha (open-input-bytes (bytes-append (block-nonce (car blckchain))
                                                                                                                 (concat (list (block-hash (car blckchain))
                                                                                                                               (block-ltrans (car blckchain))))))))) )
  (define newchain (cons message blckchain))
  (define (helper)
    (if (null? (block-ltrans (car newchain) )) (begin (set! forks (cons (list message) forks)) #t)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;((tid elementnumber amt 1/0 : comfirmed with)  )
(define (extract-from-fork fork)
  (define btoskip 0)
  ;(define conf (access forks 3) )
  (define (filtertrans transaction)
    (index-where (trans-outarray transaction) (λ (x) (equal? (cdr x) nodename )) ) ;;nodename is public id
    )
  (define (iter-over-list ltrans val)
    (if (null? ltrans) (list )
        (if (filtertrans (car ltrans))
            (let ((elnum (filtertrans (car ltrans)))
                  (oarr (trans-outarray (car ltrans)))
                  )
              (cons (list (trans-id (car ltrans))
                          elnum
                          (car (list-ref oarr elnum))
                          val) (iter-over-list (cdr ltrans) val)))
            (iter-over-list (cdr ltrans) val) ) 
        ))
  
  (define (helper listf val)
    (if (null? listf) (list )
        (append (iter-over-list (block-ltrans (car listf)) val) (helper (cdr listf) val) ))
    )

  (cons (helper (access fork btoskip) 1) (helper (access-first fork btoskip ) 0 ) )
 )

(define (extract-from-fork-new fork)
  (define btoskip 0)
  ;(define conf (access forks 3) )
  (define (filtertrans transaction)
    (index-where (trans-outarray transaction) (λ (x) (equal? (cdr x) nodename )) ) ;;nodename is public id
    )
  (define (iter-over-list ltrans val)
    (if (null? ltrans) (list )
        (if (filtertrans (car ltrans))
            (cons (car ltrans) (iter-over-list (cdr ltrans) val))
            (iter-over-list (cdr ltrans) val)) 
        ))
  
  (define (helper listf val)
    (if (null? listf) (list )
        (append (iter-over-list (block-ltrans (car listf)) val) (helper (cdr listf) val) ))
    )

  (cons (helper (access fork btoskip) 1) (helper (access-first fork btoskip ) 0 ) )
 )    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
))


   (define miner%
      (class object%
        (init-field (nodeidentifier ""))
        (init-field (pending-trans (list )))
        (init-field (nodename 0))
        (hash-set! keytoname nodename nodeidentifier)
        (init (initial-connections (list )))
        (define connections initial-connections)
        (init-field (private-key 0))
        (init-field (localtime 0))
        (init-field (local_Blockchain (list )))
        ;Initial accepted Blockchain (for starting the simulation)
        ;(init (initial_Blockchain (list )))
        ;Block chain initializer
        (init-field (computing-power 1))
        ;(init (intialized-power 1))
        ;For defining the given miner's computing power
        (init-field (block_to_be_mined (block 0 (list ) 0)))
        ;(init (initial_mining_block (block 0 (list ) 0)))
        ;initalizer for mining block
        (init-field (to_mine 0))

        (super-new)

        (define/public (add-new-connection node1 link-delay1)
          (set! connections (cons (list node1 link-delay1) connections)))
     
        (define/public (delete-connection node1)
          (set! connections (filter (lambda (x) (not (= (car x) node1))) connections)))

        (define/public (edit-connection node1 link_delay_new)
          (set! connections (map (lambda (x)
                                   (if (equal? (car x) node1)
                                       (list node1 link_delay_new)
                                       (list (car x) (cadr x))))
                                 connections)))

     (define/public (Extension_new? blckchain message)
       (define bool (and (verify-nonce message no_of_zeros) (equal? (block-hash message) (sha (open-input-bytes (bytes-append (block-nonce (car blckchain))
                                                                                                                      (concat (list (block-hash (car blckchain))
                                                                                                                                    (block-ltrans (car blckchain))))))))) )
       (define newchain (cons message blckchain))
       
       (define (helper)
         (if (null? (block-ltrans (car newchain) ))
             #t
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (define/public (make-block ltrans time)
       (define hash (sha (open-input-bytes (bytes-append (block-nonce (car local_Blockchain))
                                                    (concat (list (block-hash (car local_Blockchain))
                                                                  (block-ltrans (car local_Blockchain))))))))
;       (define i (trans-id (car (block-ltrans (car local-blockchain)))))
;       (define (add-id-to-trans list-trans)
;         (reverse (map (λ (x) (begin
;                                (set! i (+ i 1))
;                                (struct-copy trans x [id i] )) (reverse list-trans)))))
       (if (< (length ltrans) 10)
           (begin
             ;(display (length ltrans))
             (newline)
             #f    )       
           (let* [(trans_id_here (string->number (string-append (~v (get-field global_timer globaleventslist) ) (~v nodename))))
                  (new_ltrans (cons (trans trans_id_here
                                           nodename
                                           (list )
                                           (list (cons block-reward nodename))
                                           "AA")
                                    (access ltrans (- (length ltrans) 10))))]
             (begin
               (display "Made Block To Be Mined Successfully")
               (newline)
               (display "Time : ")
               (display time)
               (newline)
               (set! block_to_be_mined (block hash new_ltrans (list )))
               (set! to_mine 1)
               #t))) 
       )

     (define/public (remove-trans block)       
       (define new-pending-trans
         (filter-not (λ (x) (member x (block-ltrans block))) pending-trans)
         )
       (if (Extension_new? local_Blockchain block)
           (begin
             (set! pending-trans new-pending-trans)
             (set! local_Blockchain (cons block local_Blockchain))
             #t)
           #f)
       )


     (define/public (add-trans? trans)
       (define newchain (cons (block (list ) pending-trans (list )) local_Blockchain))
       (check-trans? trans newchain)
       )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (define/public (receive message time)
          (cond [(block? message) (remove-trans message)]
                [(trans? message) (if (add-trans? message)
                                      (begin
                                       (display "New transaction discovered by miner : ")
                                       (display nodeidentifier)
                                       (newline)
                                       (display "Time: ")
                                       (display time)
                                       (newline)
                                       (set! pending-trans (cons message pending-trans))
                                       )
                                      (begin
                                        ;(display "Message:")
                                        ;(newline)
                                        ;(display message)
                                        ;(newline)
                                        (display "Not so valid message")
                                        (newline)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (define/public (broadcast message time)
          (define (send-msg conn msg time)
            (cond [(null? conn) (begin
                                  (newline)
                                  (display "Message Broadcasted: time: ")
                                  (display time)
                                  (newline)
                                  (display "Broadcast by Miner")
                                  (newline)
                                  (newline))]
                  [(list? conn) (begin
                                  (display "Miner sending message to ")
                                  (newline)
                                  (display (hash-ref keytoname (get-field nodename (car (car conn)))))
                                  (newline)
                                  (display " Time: ")
                                  (display time)
                                  (newline)
                                  (newline)
                                  (display "expected time to reach the other end : ")
                                  (display (+ (cadr (car conn)) time))
                                  (newline)
                                  (newline)
                                  (define (f) (send (car (car conn)) receive msg (+ (cadr (car conn)) time)))
                                  (send globaleventslist insert_event f (+ (cadr (car conn)) time))
                                  ;(display f)
                                  ;(newline)
                                  ;(display globaleventslist)
                                  ;(newline)
                                  ;(newline)
                                  (send-msg (cdr conn) msg time))]
                  [else (begin
                          (display "Connections of invalid type")
                          (newline))]))
          (send-msg connections message time))
        
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mine-on-list time nos lminer time2)
  (define continue? #t)
  (define i time)
  (define (helper mlist)
    (if (equal? i 0) #f
        (if (null? mlist)
            (begin (set! i (- i 1)) (helper lminer))
            (let* ((mineval (mine-for (get-field computing-power (car mlist)) (get-field block_to_be_mined (car mlist)) nos)))
              (begin
                ;(display "This is mineval !!")
                ;(newline)
                ;(display mineval)
                ;(newline)
                (if (block? mineval)
                  (begin
                    (display "Found my Nonce!!!  Miner: ")
                    (display (get-field nodeidentifier (car mlist)))
                    (newline)
                    (display " Time: ")
                    (display (+ (- time i) 1 time2))
                    ;(display 
                    (newline)
                    (send (car mlist) remove-trans mineval)
                    (send (car mlist) broadcast mineval (+ (- time i) 1 time2))
                    #t)
                  (begin
                    ;(display "Not Yet !!")
                    ;(newline)
                    (helper (cdr mlist)))
                  ))
              ))))
  (helper lminer)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define key1 (key-gen))
(define key2 (key-gen))
(define key3 (key-gen))
(define key4 (key-gen))
(define pubkey1 (cdr key1))
(define pubkey2 (cdr key2))
(define pubkey3 (cdr key3))
(define pubkey4 (cdr key4))
(define scrkey1 (car key1))
(define scrkey2 (car key2))
(define scrkey3 (car key3))
(define scrkey4 (car key4))

(define init-block-1 (block "AA" (list ;(id pub inparray outarray digsig)
                              (trans 1
                                     pubkey2
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 1 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 2
                                     pubkey2
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 2 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 3
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 3 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 4
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 4 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 5
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 5 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 6
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 6 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 7
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 7 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 8
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 8 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))

                            
                              (trans 9
                                     (cdr key3)
                                     (list)
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 9 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 10
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 10 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))

                              )
                          ""))

(define init-block-2 (block "AA" (list ;(id pub inparray outarray digsig)
                              (trans 11
                                     pubkey2
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 11 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 12
                                     pubkey2
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 12 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 13
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 13 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 14
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 14 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 15
                                     (cdr key2)
                                     (list )
                                     (list (cons 10 pubkey2))
                                     (dig-sign (concat (list 15 pubkey2 (list ) (list (cons 10 pubkey2)))) scrkey2))
                              (trans 16
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 16 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 17
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 17 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 18
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 18 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))

                            
                              (trans 19
                                     (cdr key3)
                                     (list)
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 19 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))
                              (trans 20
                                     (cdr key3)
                                     (list )
                                     (list (cons 10 pubkey3))
                                     (dig-sign (concat (list 20 pubkey3 (list ) (list (cons 10 pubkey3)))) scrkey3))

                              )
                          ""))

(define newblock-1 (mine "AA" (block-ltrans init-block-1) no_of_zeros))
(define newblock-2 (mine (block-hash newblock-1) (block-ltrans init-block-2) no_of_zeros))

(define minerA (make-object miner% "Nilay" (list ) pubkey1 (list ) scrkey1 0 (list newblock-2 newblock-1) 50))
(define minerB (make-object miner% "Sriram" (list ) pubkey4 (list ) scrkey4 0 (list newblock-2 newblock-1) 60))

(define initial-total-21 (filter (λ (x) (= (trans-pub x) pubkey2)) (block-ltrans newblock-1)))
(define initial-total-31 (filter (λ (x) (= (trans-pub x) pubkey3)) (block-ltrans newblock-1)))
(define initial-total-22 (filter (λ (x) (= (trans-pub x) pubkey2)) (block-ltrans newblock-2)))
(define initial-total-32 (filter (λ (x) (= (trans-pub x) pubkey3)) (block-ltrans newblock-2)))

(define initial-received-21 (map (lambda (x) (cons (cons (trans-id x) 0) (car (car (trans-outarray x))))) (filter (λ (x) (= (trans-pub x) pubkey2)) (block-ltrans newblock-1))))
(define initial-received-31 (map (lambda (x) (cons (cons (trans-id x) 0) (car (car (trans-outarray x))))) (filter (λ (x) (= (trans-pub x) pubkey3)) (block-ltrans newblock-1))))
(define initial-received-22 (map (lambda (x) (cons (cons (trans-id x) 0) (car (car (trans-outarray x))))) (filter (λ (x) (= (trans-pub x) pubkey2)) (block-ltrans newblock-2))))
(define initial-received-32 (map (lambda (x) (cons (cons (trans-id x) 0) (car (car (trans-outarray x))))) (filter (λ (x) (= (trans-pub x) pubkey3)) (block-ltrans newblock-2))))

(define nodeB (make-object node% "Alice" pubkey2 (list ) scrkey2 0 (list newblock-2 newblock-1) (append initial-total-22 initial-total-21) (append initial-received-22 initial-received-21)))
(define nodeC (make-object node% "Bob" pubkey3 (list ) scrkey3 0 (list newblock-2 newblock-1) (append initial-total-32 initial-total-31) (append initial-received-32 initial-received-31)))
(set! minerlist (list minerA minerB))

(define (f1) (send nodeB attempt_transaction pubkey3 9 12))
(define (f2) (send nodeC attempt_transaction pubkey2 1 15))
(define (f3) (send nodeB attempt_transaction pubkey3 5 18))
(define (f4) (send nodeC attempt_transaction pubkey2 4 21))
(define (f5) (send nodeB attempt_transaction pubkey3 1 24))
(define (f6) (send nodeC attempt_transaction pubkey2 1 27))
(define (f7) (send nodeB attempt_transaction pubkey3 6 33))
(define (f8) (send nodeC attempt_transaction pubkey2 9 37))
(define (f9) (send nodeB attempt_transaction pubkey3 8 40))
(define (f10) (send nodeC attempt_transaction pubkey2 3 44))
(define (f11) (send nodeC attempt_transaction pubkey2 2 49))
(define (f12) (send nodeC attempt_transaction pubkey2 10 55))
(define (f13) (send nodeB attempt_transaction pubkey3 9 60))
(define (f14) (send nodeC attempt_transaction pubkey2 1 65))
(define (f15) (send nodeB attempt_transaction pubkey3 5 70))
(define (f16) (send nodeC attempt_transaction pubkey2 4 71))
(define (f17) (send nodeB attempt_transaction pubkey3 1 73))
(define (f18) (send nodeC attempt_transaction pubkey2 1 89))
(define (f19) (send nodeB attempt_transaction pubkey3 6 97))
(define (f20) (send nodeC attempt_transaction pubkey2 9 98))
(define (f21) (send nodeB attempt_transaction pubkey3 8 99))
(define (f22) (send nodeC attempt_transaction pubkey2 3 144))
(define (f23) (send nodeC attempt_transaction pubkey2 2 149))
(define (f24) (send nodeC attempt_transaction pubkey2 10 155))

(send globaleventslist insert_event f1 12)
(send globaleventslist insert_event f2 15)
(send globaleventslist insert_event f3 18)
(send globaleventslist insert_event f4 21)
(send globaleventslist insert_event f5 24)
(send globaleventslist insert_event f6 27)
(send globaleventslist insert_event f7 33)
(send globaleventslist insert_event f8 37)
(send globaleventslist insert_event f9 40)
(send globaleventslist insert_event f10 44)
(send globaleventslist insert_event f11 49)
(send globaleventslist insert_event f12 55)
(send globaleventslist insert_event f13 60)
(send globaleventslist insert_event f14 65)
(send globaleventslist insert_event f15 70)
(send globaleventslist insert_event f16 71)
(send globaleventslist insert_event f17 73)
(send globaleventslist insert_event f18 89)
(send globaleventslist insert_event f19 97)
(send globaleventslist insert_event f20 98)
(send globaleventslist insert_event f21 99)
(send globaleventslist insert_event f22 144)
(send globaleventslist insert_event f23 149)
(send globaleventslist insert_event f24 155)

(send minerA add-new-connection nodeB 1)
(send minerA add-new-connection nodeC 1)
(send minerA add-new-connection minerB 1)
(send minerB add-new-connection nodeB 1)
(send minerB add-new-connection nodeC 1)
(send minerB add-new-connection minerA 1) 
(send nodeB add-new-connection minerA 1)
(send nodeB add-new-connection nodeC 1)
(send nodeB add-new-connection minerB 1)
(send nodeC add-new-connection minerA 1)
(send nodeC add-new-connection nodeB 1)
(send nodeC add-new-connection minerB 1)

(send globaleventslist process)