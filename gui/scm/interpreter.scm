(use-modules (ice-9 sandbox) (ao kernel))

(define my-bindings (append (list (cons '(ao kernel) ao-bindings)
    (cons '(ao shapes)
        (module-map (lambda (n . a) n) (resolve-interface '(ao shapes))))
    (cons '(ao csg)
        (module-map (lambda (n . a) n) (resolve-interface '(ao csg))))
    (cons '(ao transforms)
        (module-map (lambda (n . a) n) (resolve-interface '(ao transforms)))))
    all-pure-bindings))

(define (eval-sandboxed str)
  (let ((mod (make-sandbox-module my-bindings))
        (in (open-input-string str))
        (failed #f))
    (let loop ()
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
          ;; and return a dummy list
          ((eof-object? clause) (eval-in-sandbox #nil #:module mod))

          ;; Otherwise, attempt to evaluate
          (else
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
                  (list 'error before after key params)))))
              (if (not failed)
                  (cons result (loop))
                  (list result)))))))))
eval-sandboxed
