#lang racket
;Función que crea la primera carta con recursión de cola
(define firstCardAux (lambda (lista iterador)
                       (if (= (simbolsCard(length lista)) iterador)
                           null
                          (cons (list-ref lista iterador) (firstCardAux lista (+ iterador 1))))))

(define firstCard (lambda (lista_elemento)
                          (firstCardAux lista_elemento 0)))

;Función que crea el mazo con las N + 1 cartas
(define createNcard (lambda (lista_elemento)
                         (cons (firstCard lista_elemento) (createNcardAux lista_elemento 1))))

; Función que crea las cartas N primeras cartas, por ejemplo para los elementos (1 2 3 4 5 6 7 8 9 10 11 12 13)
; Debería entregar (1 5 6 7) (1 8 9 10) (1 11 12 13)
(define createNcardAux (lambda (lista_elemento iterador1)
                      (if (= (simbolsCard (length lista_elemento)) iterador1)
                          null
                          (cons (cons (list-ref lista_elemento 0) (createNcardAux2 lista_elemento iterador1 2)) (createNcardAux lista_elemento (+ iterador1 1))))))
; Función que crea las N cartas según fórmula
; Crea EJ: (5, 6 ,7) (8, 9 10) (11, 12, 13)
(define createNcardAux2 (lambda (lista_elemento iterador1 iterador2)
                       (if (< (simbolsCard (length lista_elemento)) iterador2)
                           null                                                                                        ; Esto esta bueno (iterador2)
                           (cons (list-ref lista_elemento (- (+ (* (- (simbolsCard (length lista_elemento)) 1) iterador1) iterador2) 1)) (createNcardAux2 lista_elemento iterador1 (+ iterador2 1))))))

; Función
(define SquareCardsAux (lambda (lista_elemento i)
                         (if (= (simbolsCard (length lista_elemento)) i)
                             null
                             (cons (SquareCardsAux2 lista_elemento i 1) (SquareCardsAux lista_elemento (+ i 1)))))) 

; Función que genera una carta perteneciente a la tercera parte del algoritmo dado
(define SquareCardsAux2 (lambda (lista_elemento i j)
                       (if (= (simbolsCard (length lista_elemento)) j)
                           null
                           (cons (cons (list-ref lista_elemento (+ j 1)) (SquareCardsAux3 lista_elemento i j 1)) (SquareCardsAux2 lista_elemento i (+ j 1))))))


; Formula para calcular el n constante: (- (simbolsCard (length lista_elemento)) 1)
; Función que genera cartas según la fórmula (+ (+ (+ n 2) (* n (- k 1))) (modulo (* (- i 1) (+ (- k 1) (- j 1))) n))))
(define SquareCardsAux3 (lambda (lista_elemento i j k)
                          (if (< (- (simbolsCard (length lista_elemento)) 1) k)
                              null
                              (cons (list-ref lista_elemento (-(calculoValor (- (simbolsCard (length lista_elemento))1) i j k) 1)) (SquareCardsAux3 lista_elemento i j (+ k 1))))))


(define calculoValor (lambda (n i j k)
                       (+ (+ n 2) (* n (- k 1)) (modulo (+ (* (- i 1) (- k 1)) (- j 1)) n))))
                       
                                        
(define append2 (lambda (lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (append2 (cdr lis1) lis2))))))

;calcular solución
(define simbolsCard (lambda (n)
                   (+ (/ (- (sqrt (- (* 4 n) 3)) 1) 2) 1)))

;(createNcard (list 1 "Hola" 3 "XD" 5 "Ramo" 7 8 9 24+3i 11 "culiao" 13))
;(createNcard (list 1 2 3 4 5 6 7 8 9 10 11 12 13))
(SquareCardsAux (list 1 2 3 4 5 6 7 8 9 10 11 12 13) 1)