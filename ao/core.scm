(define-module (ao core))

(use-modules (system foreign))
(use-modules (ao jit) (ao lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ao-show- name f)
    (show_tree (or (current-filename) "<repl>") name (jit f)))

(define-syntax ao-show
    (syntax-rules ()
    "ao-show [id] variable
    Show the given function in the 3D viewport"
    ((_ shape)
         (if (pair? 'shape)
             (error
              "ao-show needs an identifier if its argument is not a variable")
         (ao-show- (symbol->string 'shape) shape)))
    ((_ id shape)
        (ao-show- (symbol->string 'id) shape))))
(export ao-show)

(define-public (ao-watch f)
    "ao-watch filename
    Watch a particular file for changes"
    (let ((target (if (absolute-file-name? f) f
                    (string-append (getcwd) file-name-separator-string f))))
    (display "Watching ") (display target) (newline)
    (watch_file (dirname target) (basename target))))

(define-public (ao-clear)
    "ao-clear
    Clear the window's frames"
    (clear_frames))

(define-public (ao-edit file)
    "ao-edit filename
    Watch and edit the given file "
    (system (string-append "touch " file))
    (ao-watch file)
    (if (getenv "TMUX")
        (if (equal? (system "which reattach-to-user-namespace") 0)
             (system (string-append "tmux split-window -b "
                                    "reattach-to-user-namespace $EDITOR " file))
             (system (string-append "tmux split-window -b $EDITOR " file)))
        (warn "Could not detect tmux session")))
