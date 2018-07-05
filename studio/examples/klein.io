;; Klein bottle
;;
;; Defined in terms of the immersion from
;; http://mathworld.wolfram.com/KleinBottle.html
(define-shape (klein x y z)
  (let* ((r (+ (square x) (square y) (square z)))
         (a (+ r (*  2 y) -1))
         (b (+ r (* -2 y) -1)))
    (+ (* a (- (square b) (* 8 (square z))))
       (* 16 x z b))))

klein

(set-quality! 8)
(set-resolution! 10)
(set-bounds! [-10 -10 -10] [10 10 10])