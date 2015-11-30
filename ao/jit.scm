(define-module (ao jit))

(use-modules (srfi srfi-1))
(use-modules (ice-9 receive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (var? a)
    (and (list? a) (= 2 (length a)) (equal? 'var (car a))
                                    (symbol? (cadr a))))

(define (num? a)
    (and (list? a) (= 2 (length a)) (equal? 'num (car a))
                                    (number? (cadr a))))

(define (op? a)
    (and (list? a) (>= (length a) 3) (equal? 'op (car a))
                                     (symbol? (cadr a))))

(define (token? a) (or (var? a) (num? a) (op? a)))

;; Constructor functions for numeric constants, variables, and operations
(define (make-num a) (list 'num a))
(define (make-var a) (list 'var a))
(define (make-op sym . args)
        (append (list 'op sym) args))

;; Converts the given argument to a token
;; (only works for numbers)
(define (to-token a)
    (cond ((token? a) a)
          ((number? a) (make-num a))
          (else (error "to-token: Bad type" a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a binary tree that reduces the given list of arguments
(define (bin-reduce sym default args)
    (let ((len  (length args))
          (half (quotient (length args) 2)))
        (cond ((= 0 len) (if (null? default) (error "Too few arguments")
                                             (make-num default)))
              ((= 1 len) (to-token (car args)))
              (else (make-op sym (bin-reduce sym default (take args half))
                                 (bin-reduce sym default (drop args half)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a wrapper function for a given commutative input function
;;
;; The wrapper will call func if all arguments are numbers; otherwise,
;; it will return a parse tree
(define (make-commutative func sym default)
  (lambda (. args)
    (receive (nums other) (partition number? args)
      (if (pair? nums)
          (let ((accum (apply func nums)))
            (if (pair? other)
                (bin-reduce sym default (cons accum other))
                accum))
          (bin-reduce sym default other)))))

;; Store backups of our numeric functions
(define _+ +)
(define _* *)
(define _max max)
(define _min min)

;; Overload commutative functions
(define + (make-commutative _+ 'add 0))
(define * (make-commutative _* 'mul 1))
(define max (make-commutative _max 'max #nil))
(define min (make-commutative _min 'min #nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a wrapper function for a non-commutative function
;; (e.g. subtraction) which reduces (cdr args) with g
(define (make-non-commutative func g sym)
  (lambda (. args)
    (receive (nums other) (partition number? args)
    (if (= 0 (length other))
      (apply func nums)
      (make-op sym (to-token (car args))
                   (to-token (apply g (cdr args))))))))

;; Store backups of functions
(define _- -)
(define _/ /)

;; Overload non-commutative functions
(define - (make-non-commutative _- + 'sub))
(define / (make-non-commutative _/ * 'div))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a wrapper function for a unary function
(define (make-unary f sym)
  (lambda (a)
    (if (number? a) (f a) (make-op sym a))))

;; Store backups of functions
(define _sqrt sqrt)

;; Overload unary functions
(define sqrt (make-unary _sqrt 'sqrt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (jit f)
  (let ((args (car (procedure-minimum-arity f))))
    (cond ((= 2 args) (f (make-var 'x) (make-var 'y)))
          ((= 3 args) (f (make-var 'x) (make-var 'y) (make-var 'z)))
          (else (error "Invalid function arity" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export jit)
(export! + * min max - / sqrt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
