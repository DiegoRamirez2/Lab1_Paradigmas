#lang racket
(define insertarAlFinal
     (lambda (elemento lista)
       (reverse
        (cons
         elemento
         (reverse (filter (lambda (x) (not (equal? elemento x))) lista))))))

(define largo
  (lambda (lista acum)
      (cond
        [(null? lista) acum]
        [(largo (cdr lista) (+ acum 1))])))
; Función que crea la primera carta
; Dominio element X int
(define firstCard
     (lambda (carta numero)
       (if (= (length carta) (+ numero 1))
         carta
          (firstCard (append2 carta (list (+ (length carta) 1))) numero))))

; Función auxiliar que crea la primera carta
; Función que construye N primeras cartas
(define ConstructorNcards
     (lambda (carta largoLista numero)
       (if (= (length carta) (+ numero 1))
              carta
              ((append2 carta (+ (* numero largoLista) (+ (length carta) 1)))
               (ConstructorNcards carta largoLista numero)))))

; Función que contruye una carta con un valor dado
(define newLista (lambda (n)
            (cons n null)))
; Función que contruye mazo con las N primeras cartas
(define Ncards
     (lambda (numero listaCartas)
     (define L1 1)
     (if (= numero 0)
         listaCartas
         ((append2 listaCartas (ConstructorNcards (list L1) (length listaCartas) (- (length listaCartas) 1)))
          (Ncards (- numero 1) listaCartas)))))
     
; Función que contruye N^2 cartas faltantes
;(define nSquaredCards (lambda 
  

(define append2 (lambda (lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (append2 (cdr lis1) lis2))))))
; Función de prueba


(define LenListAux
  (lambda (lista acum)
  (if (null? lista)
      acum
      (LenListAux (cdr lista) (+ 1 acum)))))

(define lenList (lambda (lista1)
  LenListAux lista1 0))


                        

(define L2 (list null))
(Ncards 3 (firstCard L2 3))