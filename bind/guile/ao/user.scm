#|
    Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>

    This file is part of Ao.

    Ao is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    Ao is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Ao.  If not, see <http://www.gnu.org/licenses/>.
|#
(define-module (ao user))

(use-modules (ao bind) (ao jit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ao-show- file name f)
    (window-show-tree file name (jit f)))

(define-syntax ao-show
    (syntax-rules ()
    "ao-show [id] variable
    Show the given function in the 3D viewport"
    ((_ shape)
         (if (pair? 'shape)
             (ao-error 'ao-show-id
              "ao-show needs an identifier if its argument is not a variable")
         (ao-show- (or (current-filename) "<repl>") (symbol->string 'shape) shape)))
    ((_ id shape)
        (ao-show- (or (current-filename) "<repl>") id shape))))
(export ao-show)

(define-public (ao-watch f)
    "ao-watch filename
    Watch a particular file for changes"
    (let ((target (if (absolute-file-name? f) f
                    (string-append (getcwd) file-name-separator-string f))))
    (display "Watching ") (display target) (newline)
    (window-watch-file (dirname target) (basename target))))

(define-public (ao-clear)
    "ao-clear
    Clear the window's frames"
    (window-clear-frames))

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

