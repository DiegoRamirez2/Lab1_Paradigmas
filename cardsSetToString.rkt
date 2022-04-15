#lang racket

(define cardSet->string (lambda (lista)
                          (recorreLista lista 1)))
(define recorreLista (lambda (lista acum)
                       (if (null? (cdr lista))
                            (string-append "Tarjeta " (number->string acum) ": " (recorreAux (car lista)))
                           (string-append "Tarjeta " (number->string acum) ": " (recorreAux (car lista))
                                          (recorreLista (cdr lista) (+ 1 acum))))))
(define recorreAux (lambda (lista)
                     (if (null? (cdr lista))
                          (string-append (car lista) "\n")
                         (string-append (car lista) ", " (recorreAux (cdr lista))))))

(define stackMode (lambda (setCard)
                    (

(define stackModeAux (lambda (setCard usuario numero)
                    (appendPoints usuario (car setCard) (cadr setCard))))
                    

(define appendPoints (lambda (usuario numero carta1 carta2)
                       (append usuario (isCorrect? (list numero) carta1 carta2))))

(define isCorrect? (lambda (numero carta1 carta2)
                     (if (equal? (set-intersect carta1 carta2) numero)
                         (append (list carta1) (list carta2))
                         null)))
                     

(define whoseTurnsIsIt (lambda (userList)
