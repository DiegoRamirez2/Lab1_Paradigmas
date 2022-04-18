#lang racket
(require "TDAcardsSet.rkt")

;-----------------------------------CONSTRUCTORES------------------------------------------------------------

; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define game (lambda (playersNum cardSet gameMode rndFn)
               (list playersNum null gameMode rndFn (list "Jugando") cardSet)))

;(define stackMode (lambda (cardSet)
;                    (append (list (car cardSet)) (list (cadr cardSet)))))
; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define register (lambda (user gameSet)
                   (if (= (length (car (cdr gameSet))) (car gameSet))
                       gameSet
                       (append (append (list (car gameSet)) (list (append (car (cdr gameSet)) (list (player user))))
                       (cddr gameSet))))))

(define player (lambda (userName)
                     (list userName null)))
(define nextPlayer (lambda (gameSet)
                     (append (append (list (car gameSet)) (list  (append (list (car (car (cdr gameSet))) (reverse (cdr (car (cdr gameSet)))))))
                       (cddr gameSet)))))

;-----------------------------------FUNCIONES DE PERTENENCIA-------------------------------------------------
; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define whoseTurnsIsIt? (lambda (gameSet)
                          (car (car (cadr gameSet)))))

;-----------------------------------SELECTORES---------------------------------------------------------------

; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define stackMode (lambda (gameSet)
                     (if (equal? (list-ref gameSet 4) "Terminado")
                         null
                         (if (null? (list-ref gameSet 5))
                             null
                             (stackModeAux gameSet 0)))))

(define stackModeAux (lambda (gameSet acum)
                       (if (= acum 5)
                           (if (and (subsetOfcardGenerator? (car gameSet)) (= (length (car gameSet)) 2))
                               (append (list (cddr gameSet)) (list (car (car gameSet))))
                               (append (list (cddr (car gameSet))) (list (list (car (car gameSet)) (cadr (car gameSet))))))
                           (append (list (car gameSet)) (stackModeAux (cdr gameSet) (+ acum 1))))))
 




(define nullFn (lambda (gameSet)
                 (display (car (reverse (stackMode gameSet))))
                 (display "\n")
                 (stackMode gameSet)))

(define spotIt (lambda (element gameSet)
                 (if (< (length gameSet) 7)
                     gameSet
                     (if (equal? (set-intersect (car (car (reverse gameSet))) (cadr (car (reverse gameSet)))) (list element))
                         (nextPlayer (appendPoints (whoseTurnsIsIt? gameSet) gameSet))
                         (nextPlayer gameSet)))))
                         
(define appendPoints (lambda (user gameSet)
                       (if (equal? (car (car (cadr gameSet))) user)
                          (append  (append (list (car gameSet)) (appendPointsAux (cadr gameSet) (car (reverse gameSet))) (cddr (reverse (cdr (reverse gameSet))))))
                          (nextPlayer gameSet))))

(define appendPointsAux (lambda (listUsers pointCards)
                       (append (list (append (list (car (car listUsers))) (list (append (cadr (car listUsers)) pointCards)))) (list (cdr listUsers)))))
                                           
                           
; (nextPlayer (reverse (cdr (reverse (append (list (append (car gameSet)) (append (list (append (list user))
;                                                           (append (list (cadr (car (cadr gameSet)))) (car (reverse gameSet)))) (cdr (cadr gameSet))))) (list (cddr gameSet))))))                           
                     
                     
(define pass (lambda (gameSet)
               )
                 
                 
                  


                             
                     
(define game1 (game 4 (cardsSet (list 1 2 3 4 5 6 7) 3 -1 randomFn) stackMode randomFn))
(define game2 (register "Karim" (register "Mel" (register "Ale" (register "Diego" game1)))))
                    