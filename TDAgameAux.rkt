#lang racket
(require "TDAcardsSet.rkt")
;-----------------------------------CONSTRUCTORES------------------------------------------------------------

; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define player (lambda (userName)
                     (list userName null)))
(define nextPlayer (lambda (gameSet)
                      (append (append (list (car gameSet)) (list (append (cdr (cadr gameSet)) (list (car (cadr gameSet))))))  (cddr gameSet))))
(define nextPlayerAux (lambda (listUsers)
                        (append (cdr listUsers) (list (car listUsers))))) 


;-----------------------------------FUNCIONES DE PERTENENCIA-------------------------------------------------
(define exist? (lambda (lista nombre)
                 (if (> (length (existAux? lista nombre)) 0)
                     #t
                     #f)))

(define existAux? (lambda (lista nombre)
                 (filter (lambda (x) (equal? (car x) nombre)) lista)))
;-----------------------------------SELECTORES---------------------------------------------------------------

; Dominio:
; Recorrido:
; Descripción:
; Tipo de recursión:
(define stackModeAux (lambda (gameSet acum)
                       (if (= acum 5)
                           (if (and (subsetOfcardGenerator? (car gameSet)) (= (length (car gameSet)) 2))
                               (append (list (cddr gameSet)) (list (car (car gameSet))))
                               (append (list (cddr (car gameSet))) (list (list (car (car gameSet)) (cadr (car gameSet))))))
                           (append (list (car gameSet)) (stackModeAux (cdr gameSet) (+ acum 1))))))

(define appendPoints (lambda (user gameSet)
                       (if (equal? (car (car (cadr gameSet))) user)
                          (append  (append (list (car gameSet)) (list (appendPointsAux (cadr gameSet) (car (reverse gameSet)))) (cddr (reverse (cdr (reverse gameSet))))))
                          (nextPlayer gameSet))))

(define appendPointsAux (lambda (listUsers pointCards)
                       (append (list (append (list (car (car listUsers))) (list (append (cadr (car listUsers)) pointCards))))  (cdr listUsers))))
; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))