#lang racket
(require "TDAcardsSetAux.rkt")
;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

; Dominio: Una lista con elementos de tipo string, dos números enteros y una función random propia
; Recorrido: Set de cartas (cardsSet) del juego Dobble
; Descripción: Crea un tipo de dato cardsSet que representa el mazo de cartas del juego Dobble
; Tipo de Recursión: Recursión de Cola
(define cardsSet (lambda (Elements numE maxC rndFn)
                  (if (> (simbolsTotal (- numE 1)) (length Elements))
                      null
                      (if (< (simbolsTotal (- numE 1)) maxC)
                          null
                          (if (<= maxC 0)
                               (append (rndFn (totalCards (subLista Elements (simbolsTotal (- numE 1)))) (+ 2 numE)) (list Elements))
                               (append (subLista (rndFn (totalCards (subLista Elements (simbolsTotal (- numE 1)))) numE) maxC) (list Elements)))))))


; Dominio: Un elemento del tipo de dato CardsSet
; Recorrido: Un elemento del tipo de dato CardsSet
; Descripción: Función que recibe un mazo de cartas, y retorna, en caso de que no sea válido, las cartas faltantes para que el caso sea válido
; Recursión: Recursión de cola de funciones internas
(define missingCards (lambda (setCard)
                       (if (equal? (cardsSet (car (reverse setCard)) (length (nthCard setCard 0)) (findTotalCards (nthCard setCard 0)) randomFn) '())
                           null
                           (set-subtract (cdr (reverse (cardsSet (car (reverse setCard)) (length (nthCard setCard 0)) (findTotalCards (nthCard setCard 0)) randomFn))) setCard))))

;-----------------------------------PERTENENCIA---------------------------------------------------------------

; Dominio: Un set de cartas (cardsSet)
; Recorrido: Un boolean que corresponde a true en caso de que sea válido el conjunto, falso en caso contrario
; Descripción: Función que permite verificar si el conjunto de cartas en el conjunto corresponden a un conjunto válido 
; Recursión: Recursión de Cola interna  
(define dobble? (lambda (setCard)
                  (if (null? setCard)
                      #f
                      (if (and (= (simbolsCard (length (car (reverse setCard)))) (length (car setCard))) (subsetOfcardGenerator? setCard))
                      (dobbleAux? (cdr (reverse setCard)))
                      (dobbleAux? setCard)))))
                                       
(define subsetOfcardGenerator? (lambda (setCard)
                                 (subset? setCard (cardsSet (car (reverse setCard)) (length (car setCard)) -1 randomFn))))
;-----------------------------------SELECTORES-----------------------------------------------------------------

; Dominio: Un elemento del tipo de dato cardsSet
; Recorrido: Un número entero (int)
; Descripción: Función que determina la cantidad de cartas que hay en el set creado
; Tipo de recursión: Recursión de Cola
(define numCards (lambda (lista)
                   (numCardsAux lista 0)))

; Dominio: Una lista con listas de tipo set de cartas y un número entero (cardsSet X int)
; Recorrido: Una lista del tipo carta (card)
; Descripcion: Función que obtiene la carta ubicada en la n-ésima posición (partiendo desde 0 hasta (totalCartas-1))
; Tipo de recursion: Recursión de Cola
(define nthCard (lambda (lista nth)
                   (nthCardAux lista nth 0)))

; Dominio: Una lista de tipo carta y un número entero (card X int)
; Recorrido: Un número entero (int)
; Descripción: Función que calcula el total de cartas que se deben producir para construir un conjunto válido
; Tipo de recursión: No se utiliza recursión
(define findTotalCards (lambda (carta)
                         (+ (* (- (numCards carta) 1) (- (numCards carta) 1)) (- (numCards carta) 1) 1)))

; Dominio: Una lista de tipo carta (card)
; Recorrido: Un número entero (int)
; Descripción: Función que calcula el total de elementos necesarios para poder construir un conjunto válido
; Tipo de recursión: No se utiliza recursión
(define requiredElements (lambda (carta)
                           (+ (* (- (length carta) 1) (- (length carta) 1)) (- (length carta) 1) 1)))

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Dominio: Una lista con listas de tipo set de cartas (cardsSet)
; Recorrido: Una cadena de texto (string)
; Descripción: Función que crea un string que contiene todas las cartas y sus correspondientes símbolos
; Tipo de recursión: Recursión de Cola
(define cardsSet->string (lambda (lista)
                          (if (null? lista)
                              null
                              (recorreLista lista 1))))


; Dominio: Una lista con sublistas que contienen elementos (list X list)
; Recorrido: Una lista con sublistas re-posicionadas (list X list)
; Descripción: Función que re-posiciona los elementos, primero los de índice par y luego los de índice impar dado un número de veces
; Recursión: Recursión de Cola
(define randomFn (lambda (lista numero)
                   (if (= numero 0)
                       lista
                        (randomFn (suffleList lista) (- numero 1)))))



; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))