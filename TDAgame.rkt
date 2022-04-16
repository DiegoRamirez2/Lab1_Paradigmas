#lang racket
(require "TDAcardsSet.rkt")
(define game (lambda (playersNum cardSet gameMode rndFn)
               (list (list playersNum) cardSet gameMode rndFn (list null))))

(define stackMode (lambda (cardSet)
                    (append (list (car cardSet)) (list (cadr cardSet)))))

(define player (lambda (userName)
                     (list userName null 0 #f)))
(define register (lambda (user gameSet)
                   (if (= (- (length (car gameSet)) 1) (car (car gameSet)))
                       gameSet
                       (append (list (append (car gameSet) (list (player user))))
                           (cdr gameSet)))))