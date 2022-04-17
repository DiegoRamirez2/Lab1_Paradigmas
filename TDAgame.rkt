#lang racket
(require "TDAcardsSet.rkt")
(define game (lambda (playersNum cardSet gameMode rndFn)
               (list (list playersNum) cardSet gameMode rndFn (list null))))

(define stackMode (lambda (cardSet)
                    (append (list (car cardSet)) (list (cadr cardSet)))))

(define player (lambda (userName)
                     (list userName null 0 #f)))

(define firstPlayer (lambda (userName)
                     (list userName null 0 #t)))

                     
(define register (lambda (user gameSet)
                   (if (= (- (length (car gameSet)) 1) (car (car gameSet)))
                       gameSet
                       (if (= (length (car gameSet)) 1)
                           (append (list (append (car gameSet) (list (firstPlayer user))))
                           (cdr gameSet))
                           (append (list (append (car gameSet) (list (player user))))
                           (cdr gameSet))))))

(define whoseTurnsIsIt? (lambda (gameSet)
                             (whoseTurnsIsItAux? (cdr (car gameSet)))))

(define whoseTurnsIsItAux? (lambda (userList)
                          (if (car (cdddr userList))
                              (car (car userList))
                              (whoseTurnsIsItAux? (cdr userList)))))

                      