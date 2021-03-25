;; Studio demo
;; ================
;;
;; Welcome to Studio, a GUI for the libfive CAD kernel.
;;
;; This file is written in Guile Scheme, but Studio supports both Scheme
;; and Python.  To change languages, use the Language menu.


;; Shapes are defined in this pane, and rendered
;; on the right.  You can start with shapes from
;; the standard library, e.g.
(box [-1 -1 -1] [1 1 0])


;; Any top-level expression that evaluates to a
;; shape will be rendered as a mesh.  You can
;; control bounds and resolution using these special commands:
(set-quality! 8)
(set-resolution! 10)
(set-bounds! [-10 -10 -10] [10 10 10])


;; Constructive solid geometry (CSG)
;; operations can be peformed using the libfive
;; standard library functions e.g. this difference
;; operation:
(difference (box [-4 -1 -1] [-2 1 0]) (box [-3 -1 -0.5] [-2 0.5 0]))


;; Shapes can also be defined as arbitrary distance
;; fields, expressed as functions of x, y, z.
;; Here's the distance-field expression for a sphere:
(lambda-shape (x y z)
  (- (sqrt (+ (* x x) (* y y) (* z z))) 1))


;; Of course, this is a full Scheme interpreter,
;; so you can also do arbitrary computations.
;;
;; The result of the last computation is shown in
;; the pane below.  Here, we'll find the first 10
;; numbers in the Fibonacci sequence:
(reverse
  (let fib ((fs '(1 1)))
   (if (= 10 (length fs)) fs
     (fib (cons (+ (car fs) (cadr fs)) fs))
)))


;; Errors are marked with a red underline and
;; an error message in the box.  Uncomment out
;; this line to see what that looks like:
;(+ 1 "not a number")

;; To see a full list of shapes in the standard library,
;; open the "Shape reference" from the help menu.

;; Additional examples can be found in the source
;; repository--locally or online:
;;
;;   <https://github.com/libfive/libfive/tree/master/studio/examples>
