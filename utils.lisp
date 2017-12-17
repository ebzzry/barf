(uiop:define-package :baf/utils
    (:use :cl
          :uiop
          :inferior-shell
          :cl-scripting
          :fare-utils
          :local-time
          :cl-launch/dispatch)
  (:export #:home
           #:err
           #:apply-args
           #:apply-args-1
           #:string-first
           #:psg-lines
           #:find-binary))

(in-package :baf/utils)

(defun home (path)
  (subpathname (user-homedir-pathname) path))

(defun err (message)
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  (apply function (append options args)))

(defun string-first (string)
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(defun find-binary (binary)
  (run/ss `(readlink -f ,(run/ss `(which ,binary)))))
