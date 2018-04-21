#lang racket
(require "project.rkt")
(require pfds/heap/binomial)

(define struct event (process time))

(define global_events%
  (class object%
    (define events-list (heap (lambda (x y) (< (event-time x) (event-time y))))) 
    (init-field global_timer)
    (define  





(define node%
  (class object%
    (init-field nodename)
    (init initial-connections)
    (define connections initial-connections)
    (define private-key)
    (define received_messages '())
    (init-field localtime 0)
    (define local_Blockchain initial_Blockchain)
    (define Forks (make-vector 10)) 
    (init initial_Blockchain)
    (define temp_Blockchain)
    
    
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

    (define/public (broadcast message time)
      (define (send-msg conn msg time)
        (cond [(null? conn) (display "Message Broadcasted")]
              [(list? conn) (begin
                              (send (car (car conn)) receive (cadr (car conn)) msg time)
                              (send-msg (cdr conn) msg))]
              [else (display "Connections of invalid type")]))
      (send-msg connections message time))

    (define/public (receive message)
      (cond [(Block? message) (cond [(Extension? local_Blockchain message) (Add_new_fork Forks message)]
                                    [(Extension_Fork? message) (begin
                                                                 (Update_Forks Forks message)
                                                                 (if (Safe_Fork_Available? Forks)
                                                                     (Update Block_chain)
                                                                     (void)))]
                                    [else (display "Block rejected by: ")
                                          (display nodename)])]
            [else (void)]))
        
      

    
      
    





(define miner%
  (class object%
    (init pubkey)
    (define publickey pubkey)
    (super-new)
    (define/public (get-publickey)
      (publickey))
    ))
    