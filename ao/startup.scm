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
(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (ice-9 regex))
(use-modules (system repl common))
(use-modules (ao overload) (ao shapes) (ao core) (ao export))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ao-shapes)
    (let ((docs
        (module-map (lambda (sym var)
            (let ((v (variable-ref var)))
            (if (procedure? v) (procedure-documentation v))))
            (resolve-interface '(ao shapes)))))
    (map (lambda (d) (display d) (newline))
         (sort (filter string? docs) string<?)))
    #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print a startup message and hotpatch version string
(system "clear")
(display     "         \x1b[1m.8.
        .888.
       :88888.          ,o8888o.
      . `88888.      . 8888   `88.
     .8. `88888.    ,8 8888     `8b
    .8`8. `88888.   88 8888      `8b
   .8' `8. `88888.  88 8888      ,8P
  .8'   `8. `88888. `8 8888     ,8P
 .888888888. `88888. ` 8888   ,88'
.8'       `8. `88888.   `88888P'
       \x1b[0m(c) 2015 Matt Keeter

REPL is provided by ")

(set! *version* (regexp-substitute #f (string-match "This program" *version*)
                                      'pre "Guile" 'post))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patch the repl-welcome function so that it also modifies the prompt
;; (the 17 \x01 characters are to pad the prompt so it ends up at the same
;;  length as "scheme@(guile-user)> "; otherwise readline does bad things with
;;  parenthesis matching).
(define (ao-prompt repl)
    (format #f "\x1b[34mAo~A\x1b[36m>\x1b[0m \x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01"
            (let ((level (length (cond
                                  ((fluid-ref *repl-stack*) => cdr)
                                  (else '())))))
              (if (zero? level) "" (format #f " \x1b[31m[~a]" level)))))

(use-modules (ice-9 readline))
(activate-readline)

(repl-default-option-set! 'prompt ao-prompt)
