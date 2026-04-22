#lang racket




(struct exn:producto-invalido exn:fail
  (codigo)
  #:extra-constructor-name make-exn:producto-invalido)
(define (raise-producto-invalido codigo)
  (raise
   (make-exn:producto-invalido
    (format "La opción '~a' no corresponde a ningún producto disponible." codigo)
    (current-continuation-marks)
    codigo)))

;; Datos

(define productos
  '((1 "Gatorade"                                2000)
    (2 "Plushie Eric Ross"                       4000)
    (3 "Frac chocolate"                          1300)
    (4 "Score Water"                             2000)
    (5 "Tiramisú"                                2500)
    (6 "Amigurumi Paolini"                       4000)
    (7 "Polera Poser Meteora LP"                 7000)
    (8 "Jugo Soprole Durazno"                     800)))

;; Funciones auxiliares

(define (mostrar-productos)
  (displayln "\n=== Productos ===")
  (for-each
   (lambda (p)
     (printf "~a. ~a - $~a\n" (first p) (second p) (third p)))
   productos))
(define (buscar-producto opcion)
  (findf (lambda (p) (= (first p) opcion)) productos))

;; Flujo de compra

(define (comprar)
  (mostrar-productos)
  (display "Seleccione producto (0 para salir): ")
  (define opcion (read))
  (cond
    [(equal? opcion 0)
     (displayln "¡Hasta pronto!")]
    [(not (integer? opcion))
     (raise-producto-invalido opcion)]
    [else
     (define producto (buscar-producto opcion))
     (if producto
         (begin
           (display "\nObteniendo producto")
           (display ".") (sleep 1)
           (display ".") (sleep 1)
           (display ".") (sleep 1)
           (printf "\n✓ Dispensando: ~a ($~a)\n" (second producto) (third producto))
           (displayln "Retire su producto de la bandeja.")
           (menu))
         (raise-producto-invalido opcion))]))

;; Menú principal

(define (menu)
  (displayln "\n=== Máquina Expendedora UCN ===")
  (with-handlers
      ([exn:producto-invalido?
        (lambda (e)
          (printf "\n⚠ Producto inválido: ~a\n" (exn-message e))
          (printf "  Código ingresado: ~a\n" (exn:producto-invalido-codigo e))
          (menu))]
       [exn:fail?
        (lambda (e)
          (printf "\n⚠ Error inesperado: ~a\n" (exn-message e))
          (menu))])
    (comprar)))
(menu)
