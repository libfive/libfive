(define-module (ao edit))

(use-modules (system foreign))
(use-modules (ao jit) (ao lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ao-show name f)
    " Show the given function in the 3D viewport "
    (show-tree (or (current-filename) "<repl>") name (jit f)))

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
    (ao-watch file)
    (if (getenv "TMUX")
        (system (string-append "tmux split-window -b $EDITOR " file))
        (warn "Could not detect tmux session")))
