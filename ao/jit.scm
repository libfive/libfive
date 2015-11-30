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

;; Helper macro to make a backup of the builtin procedure then
;; make the wrapper function and export it
(define-syntax-rule (wrap-commutative f sym default)
    (begin (define _f f)
           (define f (make-commutative _f sym default))
           (export! f)))

(wrap-commutative + 'add 0)
(wrap-commutative * 'mul 0)
(wrap-commutative min 'min #nil)
(wrap-commutative max 'max #nil)

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

;; Helper macro to make a backup of the builtin procedure then
;; make the wrapper function and export it
(define-syntax-rule (wrap-non-commutative f g sym )
    (begin (define _f f)
           (define f (make-non-commutative _f g sym))
           (export! f)))

(wrap-non-commutative - + 'sub)
(wrap-non-commutative / * 'div)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a wrapper function for a unary function
(define (make-unary f sym)
  (lambda (a)
    (if (number? a) (f a) (make-op sym a))))

;; Helper macro for unary definition and export
(define-syntax-rule (wrap-unary f sym )
    (begin (define _f f)
           (define f (make-unary _f sym))
           (export! f)))

(wrap-unary sqrt 'sqrt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
  (let ((args (car (procedure-minimum-arity f))))
    (cond ((= 2 args) (f (make-var 'x) (make-var 'y)))
          ((= 3 args) (f (make-var 'x) (make-var 'y) (make-var 'z)))
          (else (error "Invalid function arity" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
