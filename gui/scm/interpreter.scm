(use-modules (ice-9 sandbox) (ice-9 textual-ports) (ao kernel)
             (rnrs io ports) (system vm frame))

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
             (line (port-line port))
             (start (port-column port))
             (value (read port))
             (end (port-column port))
             (pos (list line start end)))
        (when (not (number? value))
          (error "Invalid variable literal" value))
        (hash-set! vars (tree-id var) (list '() var value pos))
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

(define (sandbox-backtrace stack)
  (define parent-frame
    (let recurse ((i 4))
      (define src (frame-source (stack-ref stack i)))
      (if (and src (equal? (cadr src) "ice-9/sandbox.scm"))
        i (recurse (1+ i)))))
  (define lowest-frame
    (let recurse ((i (1- parent-frame)))
      (format #t "frame ~A: src ~A\n" i (frame-source (stack-ref stack i)))
      (if (not (frame-source (stack-ref stack i)))
        (recurse (1- i)) i)))
  (format #t "frames: ~A to ~A\n" lowest-frame parent-frame))

(define (tag-var-addresses! d addr prev)
  (cond
    ((list? d)
      (let loop ((e d) (i 0))
        (when (not (nil? e))
          (let ((res (tag-var-addresses! (car e) (cons i addr) prev)))
            (when res
              (list-set! d i res)))
          (loop (cdr e) (1+ i))))
      #f)
    ((vector? d)
      (let loop ((i 0))
        (when (< i (vector-length d))
          (let ((res (tag-var-addresses! (vector-ref d i) (cons i addr) prev)))
            (when res
              (vector-set! d i res)))
          (loop (1+ i))))
       #f)
    ((and (tree? d) (var? d))
      (let ((id (tree-id d))
            (var (hash-ref prev addr)))
        (if var ; If there's a matching previous var, use it
          (let* ((prev (hash-ref vars id))
                 (value (caddr prev))
                 (pos (cadddr prev)))
            (hash-remove! vars id)
            (hash-set! vars (tree-id var) (list addr var value pos))
            var)
          (begin
            (set-car! (hash-ref vars id) addr)
            #f))))
    (else #f)
))

(define-public (eval-sandboxed str)
  (let ((mod (make-sandbox-module sandbox-bindings))
        (in (open-input-string str))
        (failed #f)
        (stack #f)
        (prev-vars (make-hash-table)))
    (hash-map->list (lambda (k v) (hash-set! prev-vars (car v) (cadr v))) vars)
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
          (failed (list (append (list 'error before after) clause (list ""))))

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
                  (list 'error before after key params stack))

                ;; Pre-unwind handler to capture stack
                (lambda (key . parameters)
                  (define s (make-stack #t))

                  (define str (call-with-output-string (lambda (p)
                    (display-backtrace s p 0 (- (stack-length s) 13))
                    (flush-output-port p))))

                  (sandbox-backtrace s)
                  (newline)
                  (display-backtrace s (current-output-port))
                  (newline)
                  (flush-output-port (current-output-port))
                  (set! stack str))

                )))
              (if (not failed)
                  (cons result (loop (1+ i)))
                  (list result)))))))))
