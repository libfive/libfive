(use-modules (ice-9 sandbox) (ice-9 textual-ports) (ao kernel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define integer-chars
  (map (lambda (i) (integer->char (+ i (char->integer #\0))))
       (iota 10)))

(define-public vars (make-hash-table))
(map (lambda (c)
  (eval `(read-hash-extend ,c
    (lambda (chr port)
      (unget-char port ,c)
      (let* ((var (make-var))
             (value (read port)))
        (hash-set! vars (tree-id var) (list '() var value))
        var)))
    (interaction-environment)))
  (cons #\- integer-chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-bindings mod)
  (cons mod (module-map (lambda (n . a) n) (resolve-interface mod))))

(define-public sandbox-bindings
  (append (list (cons '(ao kernel) ao-bindings)
          (get-bindings '(ao shapes))
          (get-bindings '(ao csg))
          (get-bindings '(ao transforms)))
    all-pure-bindings))

(define (tag-var-addresses! d addr prev)
  (cond
    ((list? d)
      (let loop ((d d) (i 0))
        (when (not (nil? d))
          (tag-var-addresses! (car d) (cons i addr) prev)
          (loop (cdr d) (1+ i)))))
    ((vector? d)
     (let loop ((i 0))
       (when (< i (vector-length d))
         (tag-var-addresses! (vector-ref d i) (cons i addr) prev)
         (loop (1+ i)))))
    ((and (tree? d) (var? d))
      (when (hash-ref prev addr) (format #t "Found matching var\n"))
      (set-car! (hash-ref vars (tree-id d)) addr))
))

(define-public (eval-sandboxed str)
  (let ((mod (make-sandbox-module sandbox-bindings))
        (in (open-input-string str))
        (failed #f)
        (prev-vars (make-hash-table)))
    (hash-map->list (lambda (k v) (hash-set! prev-vars (car v) (cadr v))) vars)
    (format #t "prev vars: ~A\n" (hash-map->list cons prev-vars))
    (hash-clear! vars)
    (let loop ((i 0))
      ;; Attempt to read the next clause, storing text location
      (let ((before (cons (port-line in) (port-column in)))
            (clause (catch #t (lambda () (read in))
                              (lambda (key . params)
                                      (set! failed #t)
                                      (list key params))))
            (after (cons (port-line in) (port-column in))))

        (cond
          ;; If we've failed, then record a failed tag
          (failed (list (append (list 'error before after) clause)))

          ;; If we're at the end of the stream, then sever module
          ;; and return the end of the list
          ((eof-object? clause) (eval-in-sandbox #nil #:module mod))

          ;; Otherwise, attempt to evaluate
          (else
            (tag-var-addresses! clause (list i) prev-vars)
            (let ((result
              (catch #t
                ;; Evaluation thunk
                (lambda () (cons 'valid
                  (eval-in-sandbox clause #:module mod #:sever-module? #f
                                          #:time-limit 10
                                          #:allocation-limit #e10e8)))
                ;; Error handling thunk
                (lambda (key . params)
                  (set! failed #t)
                  (list 'error before after key params))
                ;; Pre-unwind handler to capture stack
                (lambda (key . parameters)
                  (define p (standard-output-port))
                  (display-backtrace (make-stack #t) p 4)
                  (flush-output-port p))
                )))
              (if (not failed)
                  (cons result (loop (1+ i)))
                  (list result)))))))))
