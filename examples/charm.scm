(define ro 1)   ; Outer radius
(define ri 0.7) ; Inner radius
(define t 0.2)  ; Extrusion thickness

; Initial 2D model
(define s (difference (circle '(0 0) ro)
                      (circle '(0 0) ri)))

; Extruded into 3D
(define e (extrude-z s (- t) t))

; And rotated a few times
(define model (union e (rotate-x e (/ pi 2))
                       (rotate-y e (/ pi 2))))

(ao-show model)
