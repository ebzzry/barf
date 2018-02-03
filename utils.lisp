;;; utils.lisp

(uiop:define-package :baf/utils
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:fare-utils
          #:cl-launch/dispatch)
  (:export #:home
           #:err
           #:apply-args
           #:apply-args-1))

(in-package :baf/utils)

(defun home (path)
  "Return the home directory of the current user"
  (subpathname (user-homedir-pathname) path))

(defun err (message)
  "Exit with error message MESSAGE"
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  "Call apply on FUNCTION with the list of arguments on OPTIONS and ARGS"
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  "Call apply on FUNCTION with the list of arguments on OPTIONS and ARGS"
  (apply function (append options args)))
