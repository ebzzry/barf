(uiop:define-package
    :nix-lisp/nix
    (:use
     :cl
     :fare-utils
     :uiop
     :inferior-shell
     :cl-scripting
     :optima
     :optima.ppcre
     :cl-ppcre
     :local-time
     :cl-launch/dispatch
     :nix-lisp/utils)
  (:export #:nix
           #:main))

(in-package :nix-lisp/nix)

(defparameter +self+ (or (argv0) "nix"))
(defparameter +http-repository+ "https://github.com/NixOS/nixpkgs.git")
(defparameter +git-repository+ "git@github.com:NixOS/nixpkgs.git")

(defparameter +hostname+ nil)
(defun init-hostname ()
  (setf +hostname+ (hostname)))
(register-image-restore-hook 'init-hostname t)

(defparameter +base-dir+ nil)
(defun init-base-dir ()
  (setf +base-dir+ (subpathname (user-homedir-pathname) ".nix/")))
(register-image-restore-hook 'init-base-dir t)

(defun base-path (path)
  (subpathname +base-dir+ path))

(defparameter +nixpkgs+ (base-path "nixpkgs/"))
(defparameter +index+ (base-path "index/"))
(defparameter +default-nix+ (base-path "nixpkgs/default.nix"))

(defun index-path (name)
  (subpathname (base-path "index/") (format nil "~A.~A.xz" name +hostname+)))

(defparameter +index-channels+ (index-path "channels"))
(defparameter +index-upstream+ (index-path "upstream"))
(defparameter +index-installed+ (index-path "installed"))

(defun ensure-nixpkgs ()
  (ensure-directories-exist +base-dir+)
  (unless (file-exists-p +default-nix+)
    (with-current-directory (+base-dir+)
      (run/i `(git "clone" ,+http-repository+)))))

(defun ensure-index ()
  (ensure-directories-exist +index+)
  (run/i `(,(argv0) "index")))

(defun cdx (&rest args)
  (when (>= (length args) 1)
    (let ((directory (first args))
          (arguments (rest args)))
      (chdir directory)
      (when arguments (run/i arguments))
      (success))))

(defun display-usage ()
  (format t "Usage: ~A [COMMAND]... [OPTION]...

See https://github.com/ebzzry/nix-lisp for more information~%"
          +self+))

(exporting-definitions
 (defun nix (args)
   (cond ((null args) (display-usage))
         (t (let ((self (argv0))
                  (op (first args))
                  (a (rest args)))
              (match op
                ((ppcre "^(init)$")
                 (ensure-nixpkgs)
                 (ensure-index))
                ((ppcre "^(cd)$")
                 (apply #'cdx `(,+nixpkgs+ ,@a)))

                 ;; TODO
                ((ppcre "^(out-path|o-p)$")
                 (match (run/ss `(,self "query" "--out-path" ,(last a)))
                   ((ppcre ".*? (/.*)" path) (format t "~A~%" path))))

                ((ppcre "^(which|h)$")
                 (run `(command-not-found ,@a) :error-output t :on-error nil))
                ((ppcre "^(store)$")
                 (run/i `(nix-store ,@a)))
                ((ppcre "^(repl)$")
                 (run/i `(nix-repl ,@a)))
                ((ppcre "^(impure-shell|i-s)$")
                 (run/i `(nix-shell ,@a)))
                ((ppcre "^(pure-shell|p-s|shell)$")
                 (nix `("impure-shell" "--pure" ,@a)))
                ((ppcre "^(rebuild)$")
                 (run/i `(sudo nixos-rebuild ,@a)))
                ((ppcre "^(rebuild-switch|r-s)$")
                 (nix `("rebuild" "switch" ,@a)))
                ((ppcre "^(rebuild-switch-upgrade|r-s-u)$")
                 (nix `("rebuild-switch" "--upgrade" ,@a)))
                ((ppcre "^(instantiate)$")
                 (run/i `(nix-instantiate ,@a)))
                ((ppcre "^(eval)$")
                 (nix `("instantiate" "--eval" "--strict" "--show-trace" ,@a)))
                ((ppcre "^(grep)$")
                 (with-current-directory (+nixpkgs+)
                   (run/i `(find "." "-iname" "*.nix" "-exec" "grep" ,@a "{}" "\;"))))
                ((ppcre "^(find)$")
                 (run/i `(find ,+nixpkgs+ "-iname" ,@a)))
                ((ppcre "^(install-package|i-p)$")
                 (run/i `(sudo "nix-install-package" ,@a)))
                ((ppcre "^(install-package-uri|i-p-u)$")
                 (nix `("install-package" "--non-interactive" "--url" ,@a)))

                ;; TODO
                ((ppcre "^(references|r)$")
                 (nix `("store" "-q" "--references" ,(run/ss `(,self "out-path" ,(last a))))))
                ((ppcre "^(referrers|R)$")
                 (nix `("store" "-q" "--referrers" ,(run/ss `(,self "out-path" ,(last a))))))

                ((ppcre "^(query-root|q-r)$")
                 (run/i `(sudo "nix-env" "--query" ,@a)))
                ((ppcre "^(closure|c)$")
                 (nix `("store" "-qR" ,@a)))
                ((ppcre "^(set-flag|s-f)$")
                 (nix `("env" "--set-flag" ,@a)))
                ((ppcre "^(option|o)$")
                 (run/i `(nixos-option ,@a)))
                ((ppcre "^(garbage-collect|g-c)$")
                 (nix `("store" "--gc" ,@a)))
                ((ppcre "^(garbage-collect-delete|g-c-d)$")
                 (run/i `(sudo "nix-collect-garbage -d" ,@a)))

                ((ppcre "^(channel|ch)$")
                 (run/i `(nix-channel ,@a)))
                ((ppcre "^(channel-list|ch-l)$")
                 (nix `("channel" "--list" ,@a)))
                ((ppcre "^(channel-add|ch-a)$")
                 (nix `("channel" "--add" ,@a)))
                ((ppcre "^(channel-remove|ch-r)$")
                 (nix `("channel" "--remove" ,@a)))
                ((ppcre "^(channel-update|ch-u)$")
                 (nix `("channel" "--update" ,@a)))
                ;; TODO
                ((ppcre "^(channel-name|ch-n)$")
                 (match (run/ss `(,self "channel-list" ,@a))
                   ((ppcre "^(.*?) .*" name)
                    (format t "~A~%" name))))

                ((ppcre "^(root-channel|r-ch)$")
                 (run/i `(sudo "nix-channel" ,@a)))
                ((ppcre "^(root-channel-list|r-ch-l)$")
                 (nix `("root-channel" "--list" ,@a)))
                ((ppcre "^(root-channel-add|r-ch-a)$")
                 (nix `("root-channel" "--add" ,@a)))
                ((ppcre "^(root-channel-remove|r-ch-r)$")
                 (nix `("root-channel" "--remove" ,@a)))
                ((ppcre "^(root-channel-update|r-ch-u)$")
                 (nix `("root-channel" "--update" ,@a)))
                ;; TODO
                ((ppcre "^(root-channel-name|r-ch-n)$")
                 (match (run/ss `(,self "root-channel-list" ,@a))
                   ((ppcre "^(.*?) .*" name)
                    (format t "~A~%" name))))

                ;; channels
                ((ppcre "^(env|e)$")
                 (run/i `(nix-env ,@a)))
                ((ppcre "^(build|b)$")
                 (run/i `(nix-build ,@a)))
                ((ppcre "^(query|q)$")
                 (nix `("env" "--query" ,@a)))
                ((ppcre "^(upgrade|U)$")
                 (nix `("env" "--upgrade" ,@a)))
                ((ppcre "^(upgrade-always|U-a)$")
                 (nix `("upgrade" "--always" ,@a)))
                ;; TODO
                ((ppcre "^(install|i)$")
                 (nix `("env" "--install" "-A"
                                ,@(loop :for pkg :in a
                                     :collect (format nil "~A.~A" (run/ss `(,self "channel-name")) pkg)))))
                ((ppcre "^(Install|I)$")
                 (nix `("env" "--install" "-A" ,@a)))
                ((ppcre "^(query-available|q-a)$")
                 (nix `("query" "--available" "-P" ,@a)))
                ((ppcre "^(compare-versions|c-v)$")
                 (nix `("query" "--compare-versions" ,@a)))
                ((ppcre "^(compare-versions-less-than|c-v-l-t)$")
                 (run/i `(pipe (nix `("compare-versions" ,@a)) (grep "<"))))
                ((ppcre "^(compare-versions-equal|c-v-e)$")
                 (run/i `(pipe (nix `("compare-versions" ,@a)) (grep "="))))
                ((ppcre "^(compare-versions-greater-than|c-v-g-t)$")
                 (run/i `(pipe (nix `("compare-versions" ,@a)) (grep ">"))))
                ((ppcre "^(describe-available|d-a)$")
                 (nix `("query-available" "--description" ,@a)))
                ((ppcre "^(index-available|i-a)$")
                 (run `(pipe (nix ("query-available")) (xz "-c" (> ,+index-channels+)))))
                ((ppcre "^(search-available|search|s-a|s)$")
                 (run `(xzgrep "--color" "-i" ,@a ,+index-channels+) :error-output nil :on-error nil))
                ((ppcre "^(view-available|v-a)$")
                 (run/i `(xzless ,+index-channels+)))

                ;; upstream
                ((ppcre "^(upstream-env|u-e)$")
                 (nix `("env" "-f" ,+default-nix+ ,@a)))
                ((ppcre "^(upstream-build|u-b)$")
                 (nix `("build" "-I" ,(format nil "nixpkgs=~A" +nixpkgs+) ,@a)))
                ((ppcre "^(upstream-query|u-q)$")
                 (nix `("upstream-env" "--query" ,@a)))
                ((ppcre "^(upstream-upgrade|u-U)$")
                 (nix `("upstream-env" "--upgrade" ,@a)))
                ((ppcre "^(upstream-upgrade-always|u-U-a)$")
                 (nix `("upstream-upgrade" "--always" ,@a)))
                ((ppcre "^(upstream-install|u-i)$")
                 (nix `("upstream-env" "--install" "-A" ,@a)))
                ((ppcre "^(upstream-Install|u-I)$")
                 (nix `("upstream-env" "--install" "-A" ,@a)))
                ((ppcre "^(upstream-query-available|u-q-a)$")
                 (nix `("upstream-query" "--available" "-P" ,@a)))
                ((ppcre "^(upstream-compare-versions|u-c-v)$")
                 (nix `("upstream-query" "--compare-versions" ,@a)))
                ((ppcre "^(upstream-compare-versions-less-than|u-c-v-l-t)$")
                 (run/i `(pipe (nix ("upstream-compare-versions" ,@a)) (grep "<"))))
                ((ppcre "^(upstream-compare-versions-equal|u-c-v-e)$")
                 (run/i `(pipe (nix ("upstream-compare-versions" ,@a)) (grep "="))))
                ((ppcre "^(upstream-compare-versions-greater-than|u-c-v-g-t)$")
                 (run/i `(pipe (nix ("upstream-compare-versions" ,@a)) (grep ">"))))
                ((ppcre "^(upstream-describe-available|u-d-a)$")
                 (nix `("upstream-query-available" "--description" ,@a)))
                ((ppcre "^(upstream-index-available|u-i-a)$")
                 (run `(pipe (nix ("upstream-query-available")) (xz "-c" (> ,+index-upstream+)))))
                ((ppcre "^(upstream-search-available|u-search|u-s-a|u-s)$")
                 (run `(xzgrep "--color" "-i" ,@a ,+index-upstream+) :error-output nil :on-error nil))
                ((ppcre "^(upstream-view-available|u-v-a)$")
                 (run/i `(xzless ,+index-upstream+)))

                ;; installed
                ((ppcre "^(view-installed|v-i)$")
                 (run/i `(xzless ,+index-installed+)))
                ((ppcre "^(search-installed|s-i)$")
                 (run `(xzgrep "--color" "-i" ,@a ,+index-installed+)
                      :error-output nil :on-error nil))
                ((ppcre "^(index-installed|i-i)$")
                 (run `(pipe (nix ("query-installed")) (xz "-c" (> ,+index-installed+)))))
                ((ppcre "^(describe-installed|d-i)$")
                 (nix `("query-installed" "--description" ,@a)))
                ((ppcre "^(query-installed|q-i)$")
                 (nix `("query" "--installed" ,@a)))

                ;; common
                ((ppcre "^(uninstall|remove)$")
                 (nix `("env" "--uninstall" ,@a)))
                ((ppcre "^(build-index|index)$")
                 (loop :for command :in '("index-available" "upstream-index-available" "index-installed")
                    :do (nix `(,command))))
                ((ppcre "^(upstream-update|u-u)$")
                 (with-current-directory (+nixpkgs+)
                   (when (string= (run/ss `(git "rev-parse" "--abbrev-ref" "HEAD")) "master")
                     (run/i `(git "pull" "origin" "master")))))
                ((ppcre "^(full-update|f-u|complete-update)$")
                 (loop :for command :in '("channel-update" "root-channel-update" "upstream-update" "build-index")
                    :do (nix `(,command))))
                ((ppcre "^(full-upgrade|f-U|complete-upgrade)$")
                 (loop :for command :in '("full-update" "upstream-upgrade" "rebuild-switch-upgrade")
                    :do (nix `(,command))))
                ((ppcre "^(full-search|f-s)$")
                 (loop :for command :in '("search-available" "upstream-search-available")
                    :do (nix `(,command ,@a))))

                ;; miscellany

                ;; TODO
                ((ppcre "^(view-packages|v-p)$")
                 (nix `("query-available" "-A" ,(format nil "~A.~A" (run/ss `(,self "channel-name")) (first a)))))

                ((ppcre "^(make)$")
                 (nix `("pure-shell" "--run" "make" ,@a)))
                ((ppcre "^(nix-version)$")
                 (nix `("env" "--version")))
                ((ppcre "^(nixpkgs-version)$")
                 (nix `("instantiate" "--eval" "<nixpkgs>" "-A" "lib.nixpkgsVersion")))
                ((ppcre "^(nixos-version)$")
                 (run/i `(nixos-version)))
                ((ppcre "^(version)$")
                 (loop :for command :in '("nix" "nixpkgs" "nixos")
                    :do (nix `(,(format nil "~A-version" command)))))

                ;; prefetch
                ((ppcre "^(fetch-url)$")
                 (run/i `(nix-prefetch-url ,@a)))
                ((ppcre "^(fetch-file)$")
                 (nix `("fetch-url" ,(format nil "file://~A" (first a)))))
                ((ppcre "^(fetch-git)$")
                 (run/i `(nix-prefetch-git ,@a)))
                ((ppcre "^(fetch-zip)$")
                 (run/i `(nix-prefetch-zip ,@a)))
                ((ppcre "^(fetch-hg)$")
                 (run/i `(nix-prefetch-hg ,@a)))
                ((ppcre "^(fetch-svn)$")
                 (run/i `(nix-prefetch-svn ,@a)))
                ((ppcre "^(fetch-bzr)$")
                 (run/i `(nix-prefetch-bzr ,@a)))
                ((ppcre "^(fetch-cvs)$")
                 (run/i `(nix-prefetch-cvs ,@a)))
                (else (display-usage)))
              )))
   (success)))

(defun main (&rest args)
   (apply #'nix args)
   (success))

(register-commands :nix-lisp/nix)
