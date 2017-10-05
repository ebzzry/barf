(uiop:define-package :nix-lisp/misc
    (:use :cl
          :uiop
          :inferior-shell
          :cl-scripting
          :fare-utils
          :cl-launch/dispatch)
  (:export #:symlink))

(in-package :nix-lisp/misc)

(exporting-definitions
 (defun symlink (src)
   (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR")) :ensure-directory t)))
     (with-current-directory (binarch)
       (dolist (i (cl-launch/dispatch:all-entry-names))
         (run `(ln -sf ,src ,i)))))
   (success)))

(register-commands :nix-lisp/misc)
