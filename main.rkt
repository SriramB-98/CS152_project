#lang racket
(require "cryptofunctions.rkt")
(require data/heap)
(require racket/class)
(provide (all-defined-out))

(struct event (process time) #:transparent)
(define no_of_zeros 6)
(define block-reward 10)
(define Max_forklength 2)
(define keytoname (make-hash))

(define global_events%
  (class object%
    (init-field (minerlist '()))
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
                      (let* [(temp-minerlist (filter (lambda (x) (send x make-block (get-field pending-trans x) global_timer)) minerlist))]
                        (begin                
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
    (define key (key-gen))
    (init-field (nodename (cdr key)))
    (init-field (private-key (car key)))
    (init-field (nodeidentifier ""))
    (hash-set! keytoname nodename nodeidentifier)
    (init (initial-connections '()))
    (define connections initial-connections)
    (init-field (localtime 0))
    (init (initial_Blockchain (list )))
    ;Block chain initializer
    (init-field (local_Blockchain initial_Blockchain))
    ;Initial accepted Blockchain (for starting the simulation)
    (init (init-total (list )))
    (init-field (total-accepted-payments init-total))    
    ;List of Transactions in which payment is received and the Transaction is confirmed.
    (init (init-received (list )))
    (init-field (received-payments-to-be-used init-received))
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
            (display " Bitcoins ")
            (display " By ")
            (display (hash-ref keytoname nodename1))
            (display " To ")
            (display (hash-ref keytoname nodename2))
            (display " Time : ")
            (display time2)
            )
          (begin
           (newline)
           (display "Attempt to pay failed: ")
           (display amount2)
           (display " By ")
           (display (hash-ref keytoname nodename1))
           (display " To ")
           (display (hash-ref keytoname nodename2))
           (display " Time : ")
           (display time2)
           )))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (broadcast message time)
      (define (send-msg conn msg time)
        (cond [(null? conn) (begin
                              (newline)
                              (display "Message Broadcasted at time : ")
                              (display (get-field global_timer globaleventslist)))]                       
                            
              [(list? conn) (begin
                              (define (f) (send (car (car conn)) receive msg (+ (cadr (car conn)) time)))
                              (send globaleventslist insert_event f (+ (cadr (car conn)) time))       
                              (send-msg (cdr conn) msg time))]
              [else (begin
                      (newline)
                      (display "Connections of invalid type")
                      )]))
      (send-msg connections message time))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (receive message time)
      (begin
        (newline)
        (display "A Message is received by : ")
        (display nodeidentifier)
        
        (cond [(block? message) (cond [(Extension? local_Blockchain message)
                                       (begin
                                         (newline)
                                         (display "New Fork added for node : ")
                                         (newline)
                                         (display nodeidentifier)
                                         #t)]
                                      [(Extension_Fork? message) (begin
                                                                   (newline)
                                                                   (display "Checking if fork is extended for node : ")
                                                                   (newline)
                                                                   (display nodeidentifier)
                                                                   (if
                                                                    (Safe_Fork_Available? time)
                                                                    (begin
                                                                      (newline)
                                                                      (display "Fork is now used to update the blockchain for node : ")
                                                                      (newline)
                                                                      (display nodeidentifier)
                                                                      (newline)
                                                                      (display "My Blockchain has been updated!!")
                                                                      (Accept-New-Payment time)
                                                                      (Make-Old-Payments time)
                                                                      )
                                                                    (void)))]
                                      [else (begin
                                              (newline)
                                              (display "Block rejected by: ")
                                              (display nodeidentifier)
                                              (display " Time : ")
                                              (display time)
                                              )])]
              [else (begin
                      (newline)
                      (display "Received a non block message Time : ")
                      (display time)
                      )])))
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
                       (newline)
                       (Display-payment-status #t nodename amount2 nodename2 time2)
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
                  (display " Time : ")
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
                               (display " Time : ")
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
        (define key (key-gen))
        (init-field (nodename (cdr key)))
        (init-field (private-key (car key)))
        (init-field (nodeidentifier ""))
        (init-field (pending-trans (list )))
        (hash-set! keytoname nodename nodeidentifier)
        (init (initial-connections (list )))
        (define connections initial-connections)
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
               (newline)
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
                                        (newline)
                                        (display "New transaction discovered by miner : ")
                                        (display nodeidentifier)
                                        (display " Time : ")
                                        (display time)
                                        (newline)
                                        (set! pending-trans (cons message pending-trans))
                                        )
                                      (begin
                                        (newline)
                                        (display "Not so valid message")
                                        ))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (define/public (broadcast message time)
          (define (send-msg conn msg time)
            (cond [(null? conn) (begin
                                  (newline)
                                  (display "Message Broadcasted at time : ")
                                  (display time)
                                  (newline)
                                  (display "Broadcast by Miner")
                                  )]
                  [(list? conn) (begin
                                  (newline)
                                  (display "Miner sending message to ")
                                  (display (hash-ref keytoname (get-field nodename (car (car conn)))))
                                  (display " Time : ")
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
                    (display " Time : ")
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
(define (init-listtrans node amount units startid)
  (if (< amount units)
      (list (trans startid (get-field nodename node) '() (list (cons amount (get-field nodename node))) "AA" ))
      (cons (trans startid (get-field nodename node) '() (list (cons units (get-field nodename node))) "AA" ) (init-listtrans node (- amount units) units startid))))

(define (total-init-trans nlist) ;;((node amt units) ...)
  (define id 1)
  (define (init-listtrans node amount units)
    (if (< amount units)
        (if (> amount 0)
            (list (trans id (get-field nodename node) '() (list (cons amount (get-field nodename node))) "AA" ))
            '())
        (cons (trans id (get-field nodename node) '() (list (cons units (get-field nodename node))) "AA" )
              (begin (set! id (+ 1 id)) (init-listtrans node (- amount units) units)))))
  (define (helper node-list)
    (if (null? node-list)
        '()
        (append (init-listtrans (caar node-list) (cadar node-list) (caddar node-list))
                (helper (cdr node-list)) )
        ))
  (helper nlist))

(define (chainify trans-list block-length)
  (define bhash "AA")

  (define finlist (append (build-list (- block-length (remainder (length trans-list) block-length) )
                                      (λ (x) (trans 0 0 '() '() "A" ) ) )
                          trans-list))
  
  (define (helper tlist blockchain)
    (if (null? tlist)
        blockchain
        (let ((firsttrans (access-first tlist block-length))
              (rest (access tlist block-length)))
          (begin
            (define fbm (mine bhash firsttrans no_of_zeros))

            ;(display firsttrans)
            (set! bhash (sha (open-input-bytes (bytes-append (block-nonce fbm)
                                                             (concat (list (block-hash fbm)
                                                                           (block-ltrans fbm)))))))
            (helper rest (cons fbm blockchain))
            ))
    )
    )
  (helper finlist '())
  )

(define (make-init-blockchain nlist bsize)
  (chainify (total-init-trans nlist) bsize))

(define (make-initial-total node blckchain)
  (define consolidated (foldr (λ (x y) (append (block-ltrans x) y) ) '() blckchain) )
  (filter (λ (x) (= (trans-pub x) (get-field nodename node) )) consolidated)
  )

(define (make-initial-received node blckchain)
  (define consolidated (foldr (λ (x y) (append (block-ltrans x) y) ) '() blckchain) )
  (map (lambda (x) (cons (cons (trans-id x) 0) (car (car (trans-outarray x))))) (filter (λ (x) (= (trans-pub x) (get-field nodename node))) consolidated))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;