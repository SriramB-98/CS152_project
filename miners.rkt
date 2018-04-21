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
                     (continue-mining (- (event-time event2) no_of_zeros global_timer))
                     ((event-process event2)))
                   ((event-process event2))))]))))

(define globaleventslist (make-object global_events%))


(define node%
  (class object%
    (init-field nodename)
    (init initial-connections)
    (define connections initial-connections)
    (define private-key)
    (init-field localtime 0)
    
    (define local_Blockchain initial_Blockchain)
    
    (define total-accepted-payments '())
    (define received-payments-to-be-used '())
    (define pending-payments-to-confirm '())
    (define pending-payments-to-be-made '())

    (define Forks (make-vector 3))    
    (init initial_Blockchain)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (broadcast message time)
      (define (send-msg conn msg time)
        (cond [(null? conn) (display "Message Broadcasted: time:")
                            (display (send globaleventslist global-timer))]
              [(list? conn) (begin
                              (define (f) (send (car (car conn)) receive msg (+ (cadr (car conn)) time)))
                              (send globaleventslist insert_event f (+ (cadr (car conn)) time))
                              (send-msg (cdr conn) msg))]
              [else (display "Connections of invalid type")]))
      (send-msg connections message time))

    (define/public (receive message time)
      (cond [(Block? message) (cond [(Extension? local_Blockchain message) (set! Forks (Add_new_fork Forks message))]
                                    [(Extension_Fork? Fork message) (begin
                                                                     (set! Forks (Update_Forks Forks message))
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
            [else (void)]))

  (define/public (attempt_transaction nodename2 amount2 time)
    (let* [(trans_new (Make-Payment nodename2 amount2))]
      (cond [(trans? trans_new) (broadcast trans_new time)]
            [else (set! pending-payments-to-be-made (cons (nodename2 amount time) pending-payments-to-be-made))])))                                  

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
                                        [(Extension_Fork? Fork message) (begin
                                                                          (set! Forks (Update_Forks Forks message))
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
    ))
    