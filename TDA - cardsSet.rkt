#lang racket
(define L1 (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "50" "51" "52" "53" "54" "55" "56" "57"))
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

; Función auxiliar que crea la primera carta
(define firstCard (lambda (lista_elemento)
                          (firstCardAux lista_elemento 0)))
(define firstCardAux (lambda (lista iterador)
                       (if (= (simbolsCard(length lista)) iterador)
                           null
                          (cons (list-ref lista iterador) (firstCardAux lista (+ iterador 1))))))
(define createNcard (lambda (lista_elemento)
                         (cons (firstCard lista_elemento) (createNcardAux lista_elemento 1))))

; Función auxiliar que crea las N siguientes cartas
(define createNcardAux (lambda (lista_elemento iterador1)
                      (if (= (simbolsCard (length lista_elemento)) iterador1)
                          null
                          (cons (cons (list-ref lista_elemento 0) (createNcardAux2 lista_elemento iterador1 2)) (createNcardAux lista_elemento (+ iterador1 1))))))
(define createNcardAux2 (lambda (lista_elemento iterador1 iterador2)
                       (if (< (simbolsCard (length lista_elemento)) iterador2)
                           null                                                                                        
                           (cons (list-ref lista_elemento (- (+ (* (- (simbolsCard (length lista_elemento)) 1) iterador1) iterador2) 1)) (createNcardAux2 lista_elemento iterador1 (+ iterador2 1))))))

(define totalCards (lambda (lista_elemento)
                         (append (createNcard lista_elemento) (SquareCards lista_elemento 1))))

; Función auxiliar que crea las N^2 siguientes cartas
(define SquareCards (lambda (lista_elemento i)
                         (if (= (simbolsCard (length lista_elemento)) i)
                             null
                             (append2 (SquareCardsAux lista_elemento i 1) (SquareCards lista_elemento (+ i 1)))))) 
(define SquareCardsAux (lambda (lista_elemento i j)
                       (if (= (simbolsCard (length lista_elemento)) j)
                           null
                           (cons (cons (list-ref lista_elemento i) (SquareCardsAux2 lista_elemento i j 1)) (SquareCardsAux lista_elemento i (+ j 1))))))
(define SquareCardsAux2 (lambda (lista_elemento i j k)
                          (if (< (- (simbolsCard (length lista_elemento)) 1) k)
                              null
                              (cons (list-ref lista_elemento (-(calculoValor (- (simbolsCard (length lista_elemento))1) i j k) 1)) (SquareCardsAux2 lista_elemento i j (+ k 1))))))
(define calculoValor (lambda (n i j k)
                       (+ (+ n 2) (* n (- k 1)) (modulo (+ (* (- i 1) (- k 1)) (- j 1)) n))))

; Función que obtiene una SubLista de una lista
(define subLista (lambda (lista numE)
                   (subListaAux lista numE 0)))
(define subListaAux (lambda (lista numE acum)
                      (if (>= acum numE)
                          null
                          (cons (list-ref lista acum) (subListaAux lista numE (+ acum 1))))))

; Función que calcula los símbolos que hay por carta a través del largo de la lista de elementos
(define simbolsCard (lambda (n)
                   (+ (/ (- (sqrt (- (* 4 n) 3)) 1) 2) 1)))

; Función que calcula cuántos simbolos hay en total dado el número de elementos por carta
(define simbolsTotal (lambda (n)
                       (+ (+ (* n n) n) 1)))

; Función Append propia
(define append2 (lambda (lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (append2 (cdr lis1) lis2))))))

; Dominio: Un elemento del tipo de dato CardsSet
; Recorrido: Un elemento del tipo de dato CardsSet
; Descripción: Función que recibe un mazo de cartas, y retorna, en caso de que no sea válido, las cartas faltantes para que el caso sea válido
; Recursión:
(define missingCards (lambda (setCard)
                       (if (equal? (cardsSet (car (reverse setCard)) (length (nthCard setCard 0)) (findTotalCards (nthCard setCard 0)) randomFn) '())
                           null
                           (set-subtract (cdr (reverse (cardsSet (car (reverse setCard)) (length (nthCard setCard 0)) (findTotalCards (nthCard setCard 0)) randomFn))) setCard))))

;-----------------------------------PERTENENCIA---------------------------------------------------------------
; Dominio:
; Recorrido:
; Descripción:
; Recursión:
(define dobble? (lambda (setCard)
                  (if (null? setCard)
                      #f
                      (if (and (= (simbolsCard (length (car (reverse setCard)))) (length (car setCard))) (subset? setCard (cardsSet (car (reverse setCard)) (length (car setCard)) -1 randomFn)))
                      (dobbleAux? (cdr (reverse setCard)))
                      (dobbleAux? setCard)))))
                                       

(define dobbleAux? (lambda (setCard)
                  (if (not (sameN setCard))
                      #f
                     (if (seRepite? setCard)
                          #f
                          (if (oneCommonElement? setCard)
                              #t
                              #f)))))

(define seRepite? (lambda (setCard)
                    (if (null? setCard)
                        #f
                        (if (seRepiteAux? (car setCard))
                            #t
                            (seRepite? (cdr setCard))))))

(define seRepiteAux? (lambda (Card)
                       (if (null? Card)
                           #f
                           (if (> (length (filter (lambda (x) (equal? (car Card) x)) Card)) 1)
                               #t
                               (seRepiteAux? (cdr Card))))))

(define oneCommonElement? (lambda (setCard)
                             (if (null? (cdr setCard))
                                 #t
                                 (if (not (oneCommonElementAux? (car setCard) (cdr setCard)))
                                     #f
                                     (oneCommonElement? (cdr setCard))))))

(define oneCommonElementAux? (lambda (Card setCard)
                               (if (null? setCard)
                                   #t
                                   (if (> (length (set-intersect Card (car setCard))) 1)
                                       #f
                                       (oneCommonElementAux? Card (cdr setCard))))))
                                     
(define sameN (lambda (setCard)
                (sameNaux (car setCard) (cdr setCard))))

(define sameNaux (lambda (Card setCard)
                   (if (null? setCard)
                       #t
                       (if (not (= (length Card) (length (car setCard))))
                           #f
                           (sameNaux Card (cdr setCard))))))

;-----------------------------------SELECTORES-----------------------------------------------------------------

; Dominio: Un elemento del tipo de dato cardsSet
; Recorrido: Un número entero (int)
; Descripción: Función que determina la cantidad de cartas que hay en el set creado
; Tipo de recursión: Recursión de Cola
(define numCards (lambda (lista)
                   (numCardsAux lista 0)))
(define numCardsAux (lambda (lista acum)
                   (if (null? lista)
                       acum
                       (numCardsAux (cdr lista) (+ acum 1)))))

; Dominio: Una lista con listas de tipo set de cartas y un número entero (cardsSet X int)
; Recorrido: Una lista del tipo carta (card)
; Descripcion: Función que obtiene la carta ubicada en la n-ésima posición (partiendo desde 0 hasta (totalCartas-1))
; Tipo de recursion: Recursión de Cola
(define nthCard (lambda (lista nth)
                   (nthCardAux lista nth 0)))
(define nthCardAux (lambda (lista nth acum)
                   (if (= nth acum)
                       (car lista)
                       (nthCardAux (cdr lista) nth (+ acum 1)))))

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
(define recorreLista (lambda (lista acum)
                       (if (null? (cddr lista))
                            (string-append "Carta " (myNum->string acum) ": " (recorreAux (car lista)))
                           (string-append "Carta " (myNum->string acum) ": " (recorreAux (car lista))
                                          (recorreLista (cdr lista) (+ 1 acum))))))
(define recorreAux (lambda (lista)
                     (if (null? (cdr lista))
                          (string-append (myNum->string (car lista)) "\n")
                         (string-append (myNum->string (car lista)) ", " (recorreAux (cdr lista))))))

(define myNum->string (lambda (elemento)
                    (if (number? elemento)
                        (number->string elemento)
                        (elemento))))

; Dominio: Una lista con sublistas que contienen elementos (list X list)
; Recorrido: Una lista con sublistas re-posicionadas (list X list)
; Descripción: Función que re-posiciona los elementos, primero los de índice par y luego los de índice impar dado un número de veces
; Recursión: Recursión de Cola
(define randomFn (lambda (lista numero)
                   (if (= numero 0)
                       lista
                        (randomFn (suffleList lista) (- numero 1)))))

(define suffleList (lambda (lista)
                     (append (suffleListPar lista 0) (suffleListImpar lista 1))))
                    
(define suffleListPar (lambda (lista acum)
                        (if (>= acum (length lista))
                            null
                            (cons (list-ref lista acum) (suffleListPar lista (+ acum 2))))))

(define suffleListImpar (lambda (lista acum)
                        (if (>= acum (length lista))
                            null
                            (cons (list-ref lista acum) (suffleListImpar lista (+ acum 2))))))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))