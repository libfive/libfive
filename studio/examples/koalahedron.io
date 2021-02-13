#| koalahedron.io

   Catherine Wolfram's "Polyhedral Koalas" in Libfive

   Reference: https://demonstrations.wolfram.com/PolyhedralKoalas/
   Reference: https://gitlab.com/kavalogic-inc/inspekt3d/-/blob/master/examples/turtle.scm
              (definition of icosohedron shape for Libfive)

|#

(define +phi+ 1.6180339889)

(define (half-space-pts point1 point2 point3)
  "half-space-pts point1 point2 point3
  (specify points in CW order for RH outward facing normal)"
  (let* ((V1 (- point1 point2))
         (V2 (- point3 point2))
         (X (cross V1 V2))
         (l (norm X))
         (N (/ X l))
         (pos (/ (+ point1 point2 point3) 3)))
    (half-space N pos)))

(define* (truncated-icosohedron #:optional (P +phi+))
  (let* ((-P (- P))
         (2P (* 2 P))
         (-2P (- 2P))
         (-1-2P (- -1 2P))
         (1+2P (+ 1 2P))
         (2+P (+ 2 P))
         (-2-P (- -2 P))
         (3P (* 3 P))
         (-3P (- 3P))
         (vertices
          (list->vector
           (list
            #[ -P        2       -1-2P   ]
            #[ -1       -3P       0      ]
            #[  2+P     -2P       1      ]
            #[  0        1        3P     ]
            #[ -1       -2-P      2P     ]
            #[ -3P       0        1      ]
            #[  1       -2-P      2P     ]
            #[ -2P       1       -2-P    ]
            #[  1        2+P     -2P     ]
            #[  2        1+2P    -P      ]
            #[ -2-P     -2P       1      ]
            #[ -2-P      2P      -1      ]
            #[ -1       -2-P     -2P     ]
            #[ -P       -2       -1-2P   ]
            #[ -1-2P    -P        2      ]
            #[  1+2P     P        2      ]
            #[ -1        3P       0      ]
            #[  1+2P     P       -2      ]
            #[ -2        1+2P     P      ]
            #[ -1-2P    -P       -2      ]
            #[  3P       0        1      ]
            #[  P       -2        1+2P   ]
            #[  1+2P    -P       -2      ]
            #[  0       -1       -3P     ]
            #[  0       -1        3P     ]
            #[  1        2+P      2P     ]
            #[ -1-2P     P       -2      ]
            #[ -2        1+2P    -P      ]
            #[  1       -3P       0      ]
            #[  1+2P    -P        2      ]
            #[ -2P      -1       -2-P    ]
            #[  2P       1        2+P    ]
            #[  P        2        1+2P   ]
            #[  2+P      2P      -1      ]
            #[ -2-P     -2P      -1      ]
            #[  P       -2       -1-2P   ]
            #[  2       -1-2P     P      ]
            #[  3P       0       -1      ]
            #[ -2P       1        2+P    ]
            #[ -1-2P     P        2      ]
            #[ -P       -2        1+2P   ]
            #[  2+P     -2P      -1      ]
            #[ -1        2+P      2P     ]
            #[ -P        2        1+2P   ]
            #[  1        3P       0      ]
            #[ -2P      -1        2+P    ]
            #[  P        2       -1-2P   ]
            #[  1       -2-P     -2P     ]
            #[  2P       1       -2-P    ]
            #[  2P      -1        2+P    ]
            #[ -1        2+P     -2P     ]
            #[  2        1+2P     P      ]
            #[  2+P      2P       1      ]
            #[ -2       -1-2P    -P      ]
            #[  0        1       -3P     ]
            #[ -3P       0       -1      ]
            #[  2P      -1       -2-P    ]
            #[  2       -1-2P    -P      ]
            #[ -2       -1-2P     P      ]
            #[ -2-P      2P       1      ])))
         (pentagons
          '(#(12 47 35 23 13)  ; flipped
            #(18 59 11 27 16)
            #(19 30 7 26 55)
            #(6 4 40 24 21)    ; flipped
            #(42 25 32 3 43)   ; flipped
            #(44 9 33 52 51)
            #(8 50 0 54 46)    ; flipped
            #(56 22 37 17 48)  ; flipped
            #(49 31 15 20 29)
            #(5 39 38 45 14)
            #(34 10 58 1 53)   ; flipped
            #(57 28 36 2 41)))
         (hexagons
          '(#(10 14 45 40 4 58)
            #(11 26 7 0 50 27)
            #(42 43 38 39 59 18) ; flipped
            #(2 36 6 21 49 29)
            #(20 37 22 41 2 29)
            #(25 51 52 15 31 32)
            #(26 11 59 39 5 55)
            #(57 47 12 53 1 28)  ; flipped
            #(28 1 58 4 6 36)
            #(49 21 24 3 32 31)  ; flipped
            #(53 12 13 30 19 34) ; flipped
            #(20 15 52 33 17 37) ; flipped
            #(41 22 56 35 47 57)
            #(16 27 50 8 9 44)   ; flipped
            #(45 38 43 3 24 40)
            #(48 46 54 23 35 56)
            #(14 10 34 19 55 5)  ; flipped
            #(51 25 42 18 16 44)
            #(30 13 23 54 0 7)   ; flipped
            #(9 8 46 48 17 33))))

    ;; intersect all half-spaces from a triangle of each of the pentagons
    ;; and hexagons
    (apply intersection
           (append
            (map
             (lambda (pentagon)
               (half-space-pts (vector-ref vertices (vector-ref pentagon 0))
                               (vector-ref vertices (vector-ref pentagon 1))
                               (vector-ref vertices (vector-ref pentagon 2))))
             pentagons)
            (map
             (lambda (hexagon)
               (half-space-pts (vector-ref vertices (vector-ref hexagon 0))
                               (vector-ref vertices (vector-ref hexagon 1))
                               (vector-ref vertices (vector-ref hexagon 2))))
             hexagons)))))

(define* (koalahedron shape #:optional (center #[0 0 0]) (shape-radius 1))
  (let* ((body shape)
         (head-scale 0.7)
         (head (scale-xyz body [head-scale head-scale head-scale]))
         (leg (scale-xyz body [0.4 0.4 0.4]))
         (eye (scale-xyz body [0.1 0.1 0.1]))
         (nose (scale-xyz body [0.2 0.2 0.2]))
         (ear (scale-xyz body [0.4 0.4 0.4]))
         (body-off-radius (* 1.3 shape-radius))
             (head-off #[0 body-off-radius 0])
             (leg-off #[(* body-off-radius 7/8) 0 0])
             (eye-off (+ head-off #[0 0 (* body-off-radius head-scale 23/32)])))
    (move
     (union
      ;; body
      body
      ;; head
      (move head head-off)
      ;;   eyes
      (rotate-y (rotate-x (move eye eye-off) (/ pi -12) head-off)
                (/ pi 7) head-off)
      (rotate-y (rotate-x (move eye eye-off) (/ pi -12) head-off)
                (/ pi -7) head-off)
      ;;   nose
      (move nose
            (+ head-off #[0 0 (* body-off-radius head-scale 3/4)]))
      ;;   ears
      (rotate-z
       (move ear (+ head-off
                    #[0 (* body-off-radius head-scale 5/8) 0]))
       (/ pi 3) head-off)
      (rotate-z
       (move ear (+ head-off
                    #[0 (* body-off-radius head-scale 5/8) 0]))
       (/ pi -3) head-off)
      ;; legs
      (rotate-z (move leg (- leg-off)) (/ pi 6))
      (rotate-z (move leg (- leg-off)) (/ pi -6))
      (rotate-z (move leg leg-off) (/ pi 6))
      (rotate-z (move leg leg-off) (/ pi -6)))
     center)))

#| uncomment for spherical koala:

(set-bounds! [-2 -2 -2] [3 3 3])
(set-quality! 10)
(set-resolution! 60)

(koalahedron (sphere 0.5) [0 0 0] 0.5)

|#

(define +truncated-icosohedron-circumradius+ (sqrt (+ (* 9 +phi+) 10)))

(set-bounds! [-10 -10 -10] [12 12 12])
(set-quality! 10)
(set-resolution! 30)

(koalahedron (truncated-icosohedron) [0 0 0]
             +truncated-icosohedron-circumradius+)
