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
(define-module (ao sys user))

(use-modules (ao sys libao) (ao sys jit) (ao bounds) (ao sys util))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (export-bounded func shape file res args)
    "export-bounded func shape file res args
    Checks for bounds on the shape; otherwise uses args"
    (let ((bounds (get-bounds shape))
          (shape (jit shape)))
      (if bounds
        (func shape file (car bounds) (cadr bounds) res)
        (if (not (= 2 (length args)))
          (ao-error 'export-bounds
                    "Shape has no bounds; must provide them for export")
          (func shape file (car args) (cadr args) res)))))


(define-public (ao-export-heightmap shape file res . args)
    "ao-export-heightmap shape file res ['(x0 y0 z0) '(x1 y1 z1)]
    Renders a shape and saves it to an image.
    bounds are three-element lists; res is a resolution in voxels per unit."
    (export-bounded tree-export-heightmap shape file res args))

(define-public (ao-export-mesh shape file res . args)
    "ao-export-mesh shape file res ['(x0 y0 z0) '(x1 y1 z1)]
    Renders a shape and saves it to an mesh
    bounds are three-element lists; res is a resolution in voxels per unit."
    (export-bounded tree-export-mesh shape file res args))

(define-public (ao-export-slice shape file res z . args)
    "ao-export-slice shape file res z ['(x0 y0) '(x1 y1)]
    Renders a 2D slice of a model and saves it to an svg file
      res is a resolution in voxels per unit
      z is a single number
      bounds are two-element lists"
    (export-bounded (lambda (shape file a b res)
                      (tree-export-slice shape file a b z res))
      shape file res args))
