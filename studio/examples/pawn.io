#|
   Example 1 from:

   Vadim Shapiro (2007). Semi-analytic geometry with R-functions.
   Acta Numerica, 16, pp 239-303

   https://engrspatial.wiscweb.wisc.edu/wp-content/uploads/sites/715/2014/04/2007-1.pdf
|#

;;;;
;;;; Demonstrates use of R-functions
;;;;

;;; Some setup
(set-bounds! [-10 -10 -10] [10 10 10])
(set-quality! 10)
(set-resolution! 10)
;;;
(define  ⋅ *)
(define √ sqrt)
(define ▫ square)
;;;
(define-syntax-rule (≡ name body)
  (define name body))
(define-syntax-rule (λ△ params body ...)
  (lambda-shape params
    body ...))

;;; R-functions for CSG ops
;;;   binary arguments, α = 0
(define (∨ f₁ f₂)
  (+ f₁ f₂ (√ (+ (▫ f₁) (▫ f₂)))))
(define (∧ f₁ f₂)
  (- (+ f₁ f₂) (√ (+ (▫ f₁) (▫ f₂)))))
;;;   convenience functions for n-ary arguments
(define (∪ f₁ . rest)
  (if (null? rest)
      f₁
      (∨ f₁ (apply ∪ (car rest) (cdr rest)))))
(define (∩ f₁ . rest)
  (if (null? rest)
      f₁
      (∧ f₁ (apply ∩ (car rest) (cdr rest)))))

;;; Table 4.1(b)
(≡ Ω₁ ; solid of revolution
   (λ△ (x y z)
      (+ (- z)
         (⋅ 7/16 (▫ (- (√ (+ (▫ x) (▫ y))) 4))))))

(≡ Ω₂ ; cylinder
   (λ△ (x y z)
      (- 9 (▫ x) (▫ y))))

(≡ Ω₃ ; horizontal slab
   (λ△ (x y z)
      (⋅ z (- 7 z))))

(≡ Ω₄ ; sphere
   (λ△ (x y z)
      (- 1 (▫ x) (▫ y) (▫ (- 7 z)))))

(≡ Ω₅ ; ellipsoid
   (λ△ (x y z)
      (- 2 (▫ x) (▫ y) (⋅ 9 (▫ (- 6 z))))))

;;;
;;; A chess pawn, the thing we are modeling...
;;;
(≡ Ω (∪ (∩ Ω₁ Ω₂ Ω₃) Ω₄ Ω₅))

Ω
