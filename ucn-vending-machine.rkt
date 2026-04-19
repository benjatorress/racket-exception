#lang racket


(define productos
  '((1 "Gatorade" 2000)
    (2 "Plushie Eric Ross" 4000)
    (3 "Frac chocolate" 1300)
    (4 "Score Water" 2000)
    (5 "Tiramisú" 2500)
    (6 "Amigurumi Paolini" 4000)
    (7 "Polera Poser Meteora LP (Lenguajes de Programación)" 7000)
    (8 "Jugo Soprole Durazno" 800)))

(define (mostrar-productos)
  (displayln "\n=== Productos ===")
  (for-each
   (lambda (p)
     (printf "~a. ~a - $~a\n" (first p) (second p) (third p)))
   productos))

(define (buscar-producto opcion)
  (findf (lambda (p) (= (first p) opcion)) productos))

  
(define (comprar)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (string-append "⚠ Error: " (exn-message e)))
                     (menu))])
    
    (mostrar-productos)
    (display "Seleccione producto: ")
    (define opcion (read))
    
    (define producto (buscar-producto opcion))
    
    (when (not producto)
      (error "Producto no existe"))
    
    (define precio (third producto))
    (define nombre (second producto))

    (display "Obteniendo producto")
    (display ".")
    (sleep 1)
    (display ".")
    (sleep 1)
    (display ".")
    (sleep 1)
    
    (displayln "Producto trabado en cajuela")

    
    ))

(define (menu)
  (displayln "\n=== Máquina Expendedora UCN ===")
    
    (comprar)
  )
    
(menu)