#lang racket
(require "TDAcardsSet.rkt")
(require "TDAgameAux.rkt")
;-----------------------------------CONSTRUCTORES------------------------------------------------------------

; Dominio: Un número que representa el número de jugadores (int), un mazo de cartas (cardsSet), el modo de juego (gamemode) y una función random (fn)
; Recorrido: Un tablero (game) con un modo de juego especifico
; Descripción: Función a partir de la cual se crea e inicializa un tablero a partir de los parámetros de entrada
; Tipo de recursión: No se utiliza recursión
(define game (lambda (playersNum cardSet gameMode rndFn)
               (list playersNum null gameMode rndFn (list "Jugando") cardSet)))

; Dominio: Un nombre de usuario (string) y un tablero de juego (game)
; Recorrido: Un tablero de juego (game) con el usuario ya regsitrado
; Descripción: Función para registrar a un jugador en un juego, en caso de que no existiera antes
; Tipo de recursión: No utiliza recursión
(define register (lambda (user gameSet)
                   (if (= (length (car (cdr gameSet))) (car gameSet))
                       gameSet
                       (if (exist? (cadr gameSet) user)
                           gameSet
                           (append (append (list (car gameSet)) (list (append (car (cdr gameSet)) (list (player user))))
                       (cddr gameSet)))))))

;-----------------------------------FUNCIONES DE PERTENENCIA-------------------------------------------------
; Dominio: Un tablero de juego (game) con jugadores ya registrados
; Recorrido: Una línea de texto (string) que corresponde al jugador que le toca jugar
; Descripción: Función que retorna el usuario a quién le corresponde jugar en el turno
; Tipo de recursión: No se utiliza
(define whoseTurnsIsIt? (lambda (gameSet)
                          (if (null? (cadr gameSet))
                              null
                              (car (car (cadr gameSet))))))

;-----------------------------------SELECTORES---------------------------------------------------------------

; Dominio: Un tablero de juego (game)
; Recorrido: Un tablero de juego (game) con las cartas puestas en el área de juego
; Descripción: Función que permite retirar y voltear las dos cartas superiores del stack de cartas en el juego y las dispone en el área de juego
; Tipo de recursión: Recursión de cola 
(define stackMode (lambda (gameSet)
                     (if (equal? (list-ref gameSet 4) "Terminado")
                         null
                         (if (null? (list-ref gameSet 5))
                             null
                             (stackModeAux gameSet 0)))))


; Dominio: Un tablero de juego (game) y una acción a realizar en especifica (fn)
; Recorrido: Un tablero de juego (game) con la acción ya realizada, si es que cumple las reglas del juego
; Descripción: Función que permite realizar una jugada a partir de la acción especificadas por una función action (fn)
; Tipo de recursión: Recursión de cola dentro de sus subfunciones
(define play (lambda (gameSet action)
               (if (equal? (list-ref gameSet 4) '("Terminado"))
                   gameSet
                   (if (= (length (list-ref gameSet 5)) 0)
                       gameSet
                       (cond
                     [(equal? action null) (nullFn gameSet)]
                     [(equal? action pass) (pass gameSet)]
                     [(equal? action finish) (finish gameSet)]
                     [(equal? (car action) "spotIt") (spotItAux (cdr action) gameSet)]
                     [else gameSet])))))
; Función que hace el volteo inicial de cartas según la modalidad de juego activa y no se pasa el turno
(define nullFn (lambda (gameSet)
                 (if (equal? (list-ref gameSet 4) '("Terminado"))
                     gameSet
                     (nullFnAux gameSet))))

(define nullFnAux (lambda (gameSet)
                    (display (car (reverse ((list-ref gameSet 2) gameSet))))
                     (display "\n")
                     ((list-ref gameSet 2) gameSet)))

; Función currificada para realizar la comparación entre las cartas volteadas a partir del elemento indicado por el usuario. Luego de esta función se contempla cambio de turno
(define spotIt (lambda (element)
                 (list "spotIt" element)))

(define spotItAux (lambda (element gameSet)
                 (if (equal? (list-ref gameSet 4) '("Terminado"))
                     gameSet
                     (if (< (length gameSet) 7)
                     gameSet
                     (if (= (length (car (reverse gameSet))) 1)
                         gameSet
                         (if (equal? (set-intersect (car (car (reverse gameSet))) (cadr (car (reverse gameSet)))) element)
                         (nextPlayer (appendPoints (whoseTurnsIsIt? gameSet) gameSet))
                         (nextPlayer gameSet)))))))


; Función que permite pasar el turno, procurando volver las cartas a su sitio de acuerdo a la modalidad de juego
(define pass (lambda (gameSet)
               (if (equal? (list-ref gameSet 4) '("Terminado"))
                   gameSet
                   (if (< (length gameSet) 7)
                   gameSet
                   (nextPlayer (append (reverse (cddr (reverse gameSet))) (append (list (car (reverse gameSet))) (list (cadr (reverse gameSet))))))))))

; Función que da término a la partida cambiando el estado del juego a Terminado
(define finish (lambda (gameSet)
                 (if (equal? (car gameSet) '("Jugando"))
                     (append (list (list "Terminado")) (cdr gameSet))
                     (append (list (car gameSet)) (finish (cdr gameSet))))))

; Dominio: Un tablero de juego (game)
; Recorrido: Una línea de texto (string) que corresponde al estado del juego
; Descripción: Función que retorna el estado actual del juego
; Tipo de recursión: No utiliza recursión
(define status (lambda (gameSet)
                 (car (list-ref gameSet 4))))
                                                                
; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))