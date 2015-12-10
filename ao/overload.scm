(define-module (ao overload))

(use-modules (srfi srfi-1) (ice-9 receive))
(use-modules (ao jit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a binary tree that reduces the given list of arguments
(define (bin-reduce sym default args)
    (let* ((len  (length args))
           (half (quotient len 2)))
        (cond ((= 0 len) (if (null? default) (error "Too few arguments")
                                             (make-token default)))
              ((= 1 len) (make-token (car args)))
              (else (make-token sym
                        (bin-reduce sym default (take args half))
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

;; Helper macro to make a backup of the builtin procedure then
;; make the wrapper function and export it
(define-syntax-rule (wrap-commutative f sym default)
    (begin (define builtin f)
           (define f (make-commutative builtin sym default))
           (export! f)))

(wrap-commutative + 'add 0)
(wrap-commutative * 'mul 0)
(wrap-commutative min 'min #nil)
(wrap-commutative max 'max #nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handle subtraction, which behaves as negation with a single argument
(define _- -)
(define (sub . args)
    (receive (nums other) (partition number? args)
    (cond ((null-list? other) (apply _- nums))
          ((= 1 (length args)) (make-token 'neg (car args)))
          (else (make-token 'sub (car args)
                                 (apply + (cdr args)))))))

;; Handle division, which divides 1 / x for a single argument
;; (and accumulates cdr args with multiplication otherwise)
(define _/ /)
(define (div . args)
    (receive (nums other) (partition number? args)
    (cond ((null-list? other) (apply _/ nums))
          ((= 1 (length args)) (make-token 'div 1 (car args)))
          (else (make-token 'sub (car args) (apply * (cdr args)))))))

;; Helper macro to define and export the overloaded procedure
(define-syntax-rule (wrap-custom f g)
    (begin (define f g)
           (export! f)))

(wrap-custom - sub)
(wrap-custom / div)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a wrapper function for a unary function
(define (make-unary f sym)
  (lambda (a)
    (if (number? a) (f a) (make-token sym a))))

;; Helper macro for unary definition and export
(define-syntax-rule (wrap-unary f sym)
    (begin (define builtin f)
           (define f (make-unary builtin sym))
           (export! f)))

(wrap-unary sqrt 'sqrt)
