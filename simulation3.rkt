#lang racket
(require "main.rkt")

(define nodeB (new node% [nodeidentifier "Alice"]))
(define nodeC (new node% [nodeidentifier "Bob"]))
(define nodelst (list nodeB nodeC))

(define initial_sim_blockchain (make-init-blockchain (list (list nodeB 200 10) (list nodeC 200 10)) 10))

(for-each (λ (x) (set-field! local_Blockchain x initial_sim_blockchain)) nodelst)

(for-each (lambda (x) (set-field! total-accepted-payments x (make-initial-total x initial_sim_blockchain))) nodelst)  
(for-each (lambda (x) (set-field! received-payments-to-be-used x (make-initial-received x initial_sim_blockchain))) nodelst)

(define minerA (new miner% [nodeidentifier "Nilay"] [computing-power 10]))
(define minerB (new miner% [nodeidentifier "Sriram"] [computing-power 10]))

(define minerC (new miner% [nodeidentifier "Adwait"] [computing-power 60]))
(define minerD (new miner% [nodeidentifier "Nitish"] [computing-power 60]))

(for-each (λ (x) (set-field! local_Blockchain x initial_sim_blockchain)) (list minerA minerB minerC minerD))

(set-field! minerlist globaleventslist (list minerA minerB minerC minerD))

(send minerA add-new-connection nodeB 1)
(send minerA add-new-connection nodeC 1)
(send minerA add-new-connection minerB 1)

(send minerB add-new-connection nodeB 1)
(send minerB add-new-connection nodeC 1)
(send minerB add-new-connection minerA 1)

(send nodeB add-new-connection nodeC 1)
(send nodeB add-new-connection minerA 1)
(send nodeB add-new-connection minerB 1)
(send nodeB add-new-connection minerC 1)
(send nodeB add-new-connection minerD 1)

(send nodeC add-new-connection nodeB 1)
(send nodeC add-new-connection minerA 1)
(send nodeC add-new-connection minerB 1)
(send nodeC add-new-connection minerC 1)
(send nodeC add-new-connection minerD 1)

(send minerC add-new-connection nodeB 65)
(send minerC add-new-connection nodeC 61)
(send minerC add-new-connection minerD 1)

(send minerD add-new-connection nodeB 56)
(send minerD add-new-connection nodeC 78)
(send minerD add-new-connection minerC 1)

(define simulation-list (list
                         (list nodeB nodeC 9 12)
                         (list nodeC nodeB 1 15)
                         (list nodeB nodeC 5 18)
                         (list nodeC nodeB 4 21)
                         (list nodeB nodeC 1 24)
                         (list nodeC nodeB 1 27)
                         (list nodeB nodeC 6 33)
                         (list nodeC nodeB 9 37)
                         (list nodeB nodeC 8 40)
                         (list nodeC nodeB 3 44)
                         (list nodeC nodeB 2 49)
                         (list nodeC nodeB 10 55)
                         (list nodeB nodeC 9 60)
                         (list nodeC nodeB 1 65)
                         (list nodeB nodeC 5 70)
                         (list nodeC nodeB 4 71)
                         (list nodeB nodeC 1 73)
                         (list nodeC nodeB 1 89)
                         (list nodeB nodeC 6 97)
                         (list nodeC nodeB 9 98)
                         (list nodeB nodeC 8 99)
                         (list nodeC nodeB 3 144)
                         (list nodeC nodeB 2 149)
                         (list nodeC nodeB 10 155)))
                         
(for-each (λ (x) (send globaleventslist insert_event (λ () (send (car x) attempt_transaction (get-field nodename (cadr x)) (caddr x) (cadddr x))) (cadddr x))) simulation-list) 
                     
(send globaleventslist process)