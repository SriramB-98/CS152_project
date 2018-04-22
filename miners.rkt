#lang racket
(require "project.rkt")
(require data/heap)
(require racket/class)

(struct event (process time) #:transparent)

(define no_of_zeros 6)

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
        [(= (heap-count events-list) 0) (begin
                                          (set! global_timer (+ 1 global_timer))
                                          (continue-mining 1 no_of_zeros minerlist)
                                          (process))]
        [else (begin
               (define event2 (heap-min events-list))
               (heap-remove-min! events-list)
               (if (< global_timer (event-time event2))
                   (begin
                     (continue-mining (- (event-time event2) global_timer) no_of_zeros minerlist)
                     ((event-process event2))
                     (process))
                   (begin
                     (event-process event2)
                     (process))))]))))

(define globaleventslist (make-object global_events%))


(define node%
  (class object%
    (init-field nodename)
    (init initial-connections)
    (define connections initial-connections)
    (define private-key)
    (init-field localtime 0)
    
    (define local_Blockchain initial_Blockchain)
    ;Initial accepted Blockchain (for starting the simulation)
    (define total-accepted-payments '())
    ;List of Transactions in which payment is received and the Transaction is confirmed.
    (define received-payments-to-be-used '())
    ;A List of pairs. Each element of the list is pair whose first element is again a pair
    ;and second element is the amount received by this node.
    ;First element is a pair whose First element in turn is the transaction ID
    ;and Second element is the output array index of the same transaction in which given
    ;node receives money
    ;Transactions referenced here have been confirmed but not used again.
    (define pending-payments-to-confirm '())
    ;List of Transactions in which payments are received but not yet confirmed by the blockchain.
    (define pending-payments-to-be-made '())
    ;List of lists, each element list consists of a nodename and amount
    ;indicating payment of "amount" has yet to be done to the given "nodename" 
    (define Forks '())
    ;Simply a list of future Blockchain extensions the node has heard.
    (init initial_Blockchain)
    ;Block chain initializer
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
    
    (define/public (Display-payment-status bool nodename1 amount2 nodename2 time2)
      (if bool
          ( (display "Payment made: ")
            (display amount2)
            (display " By ")
            (display nodename1)
            (display " To ")
            (display nodename2)
            (display " Time: ")
            (display time2)
            (display "/n"))
          ((display "Attempt to pay failed: ")
           (display amount2)
           (display " By ")
           (display nodename1)
           (display " To ")
           (display nodename2)
           (display " Time: ")
           (display time2)
           (display "/n"))))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (broadcast message time)
      (define (send-msg conn msg time)
        (cond [(null? conn) (display "Message Broadcasted: time:")
                            (display (get-field global-timer globaleventslist))]
              [(list? conn) (begin
                              (define (f) (send (car (car conn)) receive msg (+ (cadr (car conn)) time)))
                              (send globaleventslist insert_event f (+ (cadr (car conn)) time))
                              (send-msg (cdr conn) msg))]
              [else ((display "Connections of invalid type")
                    (display "/n"))]))
      (send-msg connections message time))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (receive message time)
      (cond [(Block? message) (cond [(Extension? local_Blockchain message) (set! Forks (Add_new_fork Forks message))]
                                    [(Extension_Fork? Fork message) (begin
                                                                     ;(set! Forks (Update_Forks Forks message))
                                                                     (if
                                                                      (Safe_Fork_Available?)
                                                                      (begin
                                                                        (Accept-New-Payments time)
                                                                        (Make-Old-Payments time)
                                                                        )
                                                                      (void)))]
                                    [else (display "Block rejected by: ")
                                          (display nodename)
                                          (display "/n")])]
            [else (void)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (Make-payment new nodename2 amount2 time2)
      (define inparray '())
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
                                                              (cons '(nodename2 amount2) pending-payments-to-be-made))
                                                        #f
                                                        )
                                                      (begin
                                                        (Display-payment-status #f nodename amount2 nodename2 time2)
                                                        #f))]
            [(< (foldr + 0 (map (lambda (x) (cdr x)) received-payments-to-be-used)) amount2) (if (= new 1)
                                                                      (begin
                                                                       (Display-payment-status #f nodename amount2 nodename2 time2) 
                                                                       (set! pending-payments-to-be-made
                                                                             (cons '(nodename2 amount2) pending-payments-to-be-made))
                                                                       #f
                                                                       )
                                                                     (begin
                                                                       (Display-payment-status #f nodename amount2 nodename2 time2)
                                                                       #f))]
            [else (begin
                    (take-ids)
                    (let* [(outarray (list (cons amount2 nodename2) (cons (- total amount2) nodename)))
                           (signature (dig-sign (concat (list nodename inparray outarray)) privkey))                      
                           (transaction_done (trans '() nodename inparray outarray signature))]                  
                     (begin
                       (Display-payment-status #t nodename amount2 nodename2 time2)
                       (broadcast transaction_done time2)
                       #t
                       )))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (attempt_transaction nodename2 amount2 time)
    (Make-Payment 1 nodename2 amount2 time))

  (define/public (Make-Old-Payments time)
     (set! pending-payments-to-be-made (filter (lambda (x) (let* [(nodename2 (car x))
                                                                  (amount2 (cadr x))]
                                                             (not (Make-payment 0 nodename2 amount2 time)))) pending-payments-to-be-made)))   
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (accept-new-payment blckchain trans11 time2)
    (define btoskip 3)
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
                  (display "accepted by node ")
                  (display nodename)
                  (display " Time: ")
                  (display time2)
                  (display "/n")
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



))



    (define miner%
      (class object%
        (init pubkey)
        (define publickey pubkey)

        (define pending-transactions-block '()))
        
        (super-new)

        (define/public (get-publickey)
        (publickey))

        (define/override (receive message time)
          (cond [(Block? message) (cond [(Extension? local_Blockchain message) (set! Forks (Add_new_fork Forks message))]
                                        [(Extension_Fork? message) (begin
                                                                        ;(set! Forks (Update_Forks Forks message))
                                                                          (if
                                                                           (Safe_Fork_Available? Forks)
                                                                           (begin
                                                                             (set! local_Blockchain (Update local_Blockchain))
                                                                             (Accept-New-Payments time)
                                                                             (Make-New-Payments time)
                                                                             )
                                                                           (void)))]
                                        [else (display "Block rejected by: ")
                                              (display nodename)])]
                [(trans? message) (if (belongs-pending-list? message)
                                      (void)
                                      (set! received-payments-to-be-used (cons message received-payments-to-be-used)))]
                                   ))
    )
    