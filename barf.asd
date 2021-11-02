#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :barf-system
  (:use #:cl #:asdf))

(in-package #:barf-system)

(defsystem :barf
  :name "barf"
  :version "0.0.1"
  :description "Common Lisp frontend to Nix commands"
  :license "CC0"
  :author "Rommel Martinez <ebzzry@ebzzry.io>"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
               (:version "fare-utils" "1.0.0.5")
               "barf/utils"
               "barf/barf"))
