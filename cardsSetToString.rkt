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

