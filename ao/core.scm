(define-module (ao core))

(use-modules (system foreign))
(use-modules (ao jit) (ao lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ao-show- name f)
    (show-tree (or (current-filename) "<repl>") name (jit f)))

(define-syntax ao-show
    (syntax-rules ()
    " Show the given function in the 3D viewport
      Invoked as (ao-show VARIABLE) or (ao-show ID SHAPE) "
    ((_ shape)
         (if (pair? 'shape)
             (error
              "ao-show needs an identifier if its argument is not a variable")
         (ao-show- (symbol->string 'shape) shape)))
    ((_ id shape)
        (ao-show- (symbol->string 'id) shape))))
(export ao-show)

(define-public (ao-watch f)
    " Watch a particular file for changes "
    (let ((target (if (absolute-file-name? f) f
                    (string-append (getcwd) file-name-separator-string f))))
    (display "Watching ") (display target) (newline)
    (watch-file (dirname target) (basename target))))

(define-public (ao-clear)
    " Clear the Window's frames and redraw "
    (clear-frames))

(define-public (ao-edit file)
    " Watch and edit the given file "
    (system (string-append "touch " file))
    (ao-watch file)
    (if (getenv "TMUX")
        (if (equal? (system "which reattach-to-user-namespace") 0)
             (system (string-append "tmux split-window -b "
                                    "reattach-to-user-namespace $EDITOR " file))
             (system (string-append "tmux split-window -b $EDITOR " file)))
        (warn "Could not detect tmux session")))
