#lang racket

;; =========================
;; Excepciones personalizadas
;; =========================
(struct exn:producto-no-existe exn:fail ())
(struct exn:pago-insuficiente exn:fail (faltante))
(struct exn:entrada-invalida exn:fail ())

;; =========================
;; Datos
;; =========================
(define productos
  '((1 "Gatorade" 2000)
    (2 "Plushie Eric Ross" 4000)
    (3 "Frac chocolate" 1300)
    (4 "Score Water" 2000)
    (5 "Tiramisú" 2500)
    (6 "Amigurumi Paolini" 4000)
    (7 "Polera Poser Meteora LP (Lenguajes de Programación)" 7000)
    (8 "Jugo Soprole Durazno" 800)))

;; =========================
;; Funciones
;; =========================
(define (mostrar-productos)
  (displayln "\n=== Productos ===")
  (for-each
   (lambda (p)
     (printf "~a. ~a - $~a\n" (first p) (second p) (third p)))
   productos))

(define (buscar-producto opcion)
  (findf (lambda (p) (= (first p) opcion)) productos))

;; =========================
;; Lógica de compra
;; =========================
(define (comprar)
  (with-handlers
      ([exn:entrada-invalida?
        (lambda (e)
          (displayln (string-append "❌ " (exn-message e)))
          (menu))]

       [exn:producto-no-existe?
        (lambda (e)
          (displayln (string-append "❌ " (exn-message e)))
          (menu))]

       [exn:pago-insuficiente?
        (lambda (e)
          (printf "❌ Pago insuficiente. Te faltan $~a\n"
                  (exn:pago-insuficiente-faltante e))
          (menu))])
    
    (mostrar-productos)
    
    (display "Seleccione producto: ")
    (define opcion (read))
    
    ;; Validar que sea número
    (unless (number? opcion)
      (raise
       (exn:entrada-invalida
        "Debe ingresar un número válido"
        (current-continuation-marks))))
    
    (define producto (buscar-producto opcion))
    
    ;; Validar existencia
    (when (not producto)
      (raise
       (exn:producto-no-existe
        "El producto no existe"
        (current-continuation-marks))))
    
    (define precio (third producto))
    (define nombre (second producto))
    
    (printf "Precio de ~a: $~a\n" nombre precio)
    
    (display "Ingrese dinero: ")
    (define dinero (read))
    
    ;; Validar que sea número
    (unless (number? dinero)
      (raise
       (exn:entrada-invalida
        "Debe ingresar un monto numérico válido"
        (current-continuation-marks))))
    
    ;; Validar dinero suficiente
    (when (< dinero precio)
      (raise
       (exn:pago-insuficiente
        "Dinero insuficiente"
        (current-continuation-marks)
        (- precio dinero))))
    
    ;; Simulación
    (display "Obteniendo producto")
    (for ([i 3])
      (display ".")
      (sleep 1))
    
    (displayln "\n✅ Producto entregado")
    (printf "Vuelto: $~a\n" (- dinero precio))
    
    (menu)))

;; =========================
;; Menú
;; =========================
(define (menu)
  (displayln "\n=== Máquina Expendedora UCN ===")
  (comprar))

(menu)