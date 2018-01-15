(uiop:define-package
    :baf/baf
    (:use
     :cl
     :fare-utils
     :uiop
     :inferior-shell
     :cl-scripting
     :optima
     :optima.ppcre
     :cl-ppcre
     :cl-launch/dispatch
     :baf/utils)
  (:export #:baf
           #:main))

(in-package :baf/baf)

(defparameter +self+ (or (argv0) "baf"))
(defparameter +version+ "0.0.15")
(defparameter +http-repository+ "https://github.com/NixOS/nixpkgs.git")
(defparameter +git-repository+ "git@github.com:NixOS/nixpkgs.git")

(defun base-dir () (subpathname (user-homedir-pathname) ".baf/"))
(defun base-path (path) (subpathname (base-dir) path))

(defun nixpkgs () (base-path "nixpkgs/"))
(defun index () (base-path "index/"))
(defun default-nix () (base-path "nixpkgs/default.nix"))

(defun index-path (name) (subpathname (base-path "index/") (format nil "~A.~A.gz" name (hostname))))
(defun index-channels () (index-path "channels"))
(defun index-upstream () (index-path "upstream"))
(defun index-installed () (index-path "installed"))

(defun run! (cmd)
  (run/interactive cmd :on-error nil))

(defun ensure-nixpkgs ()
  (and (directory-exists-p (nixpkgs))
       (delete-directory-tree (physicalize-pathname (nixpkgs)) :validate t))
  (ensure-directories-exist (base-dir))
  (unless (file-exists-p (default-nix))
    (with-current-directory ((base-dir))
      (run/i `(git "clone" ,+http-repository+)))))

(defun ensure-index ()
  (and (directory-exists-p (index))
       (delete-directory-tree (physicalize-pathname (index)) :validate t))
  (ensure-directories-exist (index))
  (run/i `(,(argv0) "index")))

(defun nixosp () (file-exists-p "/etc/nixos/configuration.nix"))

(defun cdx (&rest args)
  (when (>= (length args) 1)
    (let ((directory (first args))
          (arguments (rest args)))
      (chdir directory)
      (when arguments (run/i arguments))
      (success))))

(defun display-usage ()
  (format t "Usage: ~A [COMMAND]... [OPTION]...

See https://github.com/ebzzry/baf for more information~%"
          +self+))

(exporting-definitions
 (defun baf (args)
   (cond ((null args) (display-usage))
         (t (let ((self (argv0))
                  (op (first args))
                  (a (rest args)))
              (match op
                ((ppcre "^(init)$")
                 (ensure-nixpkgs)
                 (ensure-index))
                ((ppcre "^(cd)$")
                 (apply #'cdx `(,(nixpkgs) ,@a)))

                ((ppcre "^(out-path|o-p)$")
                 (match (run/ss `(,self "query" "--out-path" ,(last a)))
                   ((ppcre ".*? (/.*)" path) (format t "~A~%" path))))

                ((ppcre "^(which|h)$")
                 (run! `(command-not-found ,@a)))
                ((ppcre "^(store)$")
                 (run! `(nix-store ,@a)))
                ((ppcre "^(repl)$")
                 (run! `(nix-repl ,@a)))
                ((ppcre "^(impure-shell|i-s)$")
                 (run! `(nix-shell ,@a)))
                ((ppcre "^(pure-shell|p-s|shell)$")
                 (baf `("impure-shell" "--pure" ,@a)))
                ((ppcre "^(rebuild)$")
                 (run! `(sudo nixos-rebuild ,@a)))
                ((ppcre "^(rebuild-switch|r-s)$")
                 (baf `("rebuild" "switch" ,@a)))
                ((ppcre "^(rebuild-switch-upgrade|r-s-u)$")
                 (baf `("rebuild-switch" "--upgrade" ,@a)))
                ((ppcre "^(instantiate)$")
                 (run! `(nix-instantiate ,@a)))
                ((ppcre "^(eval)$")
                 (baf `("instantiate" "--eval" "--strict" "--show-trace" ,@a)))
                ((ppcre "^(grep)$")
                 (with-current-directory ((nixpkgs))
                   (run! `(find "." "-iname" "*.nix" "-exec" "grep" ,@a "{}" "\;"))))
                ((ppcre "^(find)$")
                 (run! `(find ,(nixpkgs) "-iname" ,@a)))
                ((ppcre "^(install-package|i-p)$")
                 (run! `(sudo "nix-install-package" ,@a)))
                ((ppcre "^(install-package-uri|i-p-u)$")
                 (baf `("install-package" "--non-interactive" "--url" ,@a)))

                ((ppcre "^(references|r)$")
                 (baf `("store" "-q" "--references" ,@a)))
                ((ppcre "^(referrers|R)$")
                 (baf `("store" "-q" "--referrers" ,@a)))

                ((ppcre "^(query-root|q-r)$")
                 (run! `(sudo "nix-env" "--query" ,@a)))
                ((ppcre "^(closure|c)$")
                 (baf `("store" "-qR" ,@a)))
                ((ppcre "^(set-flag|s-f)$")
                 (baf `("env" "--set-flag" ,@a)))
                ((ppcre "^(option|o)$")
                 (run! `(nixos-option ,@a)))
                ((ppcre "^(garbage-collect|g-c)$")
                 (baf `("store" "--gc" ,@a)))
                ((ppcre "^(garbage-collect-delete|g-c-d)$")
                 (run! `(sudo "nix-collect-garbage" "-d" ,@a)))

                ((ppcre "^(channel|ch)$")
                 (run! `(nix-channel ,@a)))
                ((ppcre "^(channel-list|ch-l)$")
                 (baf `("channel" "--list" ,@a)))
                ((ppcre "^(channel-add|ch-a)$")
                 (baf `("channel" "--add" ,@a)))
                ((ppcre "^(channel-remove|ch-r)$")
                 (baf `("channel" "--remove" ,@a)))
                ((ppcre "^(channel-update|ch-u)$")
                 (baf `("channel" "--update" ,@a)))
                ((ppcre "^(channel-name|ch-n)$")
                 (match (run/ss `(,self "channel-list" ,@a))
                   ((ppcre "^(.*?) .*" name)
                    (format t "~A~%" name))))

                ((ppcre "^(root-channel|r-ch)$")
                 (and (nixosp) (run! `(sudo "nix-channel" ,@a))))
                ((ppcre "^(root-channel-list|r-ch-l)$")
                 (baf `("root-channel" "--list" ,@a)))
                ((ppcre "^(root-channel-add|r-ch-a)$")
                 (baf `("root-channel" "--add" ,@a)))
                ((ppcre "^(root-channel-remove|r-ch-r)$")
                 (baf `("root-channel" "--remove" ,@a)))
                ((ppcre "^(root-channel-update|r-ch-u)$")
                 (baf `("root-channel" "--update" ,@a)))
                ((ppcre "^(root-channel-name|r-ch-n)$")
                 (match (run/ss `(,self "root-channel-list" ,@a))
                   ((ppcre "^(.*?) .*" name)
                    (format t "~A~%" name))))

                ;; channels
                ((ppcre "^(env|e)$")
                 (run! `(nix-env ,@a)))
                ((ppcre "^(build|b)$")
                 (run! `(nix-build ,@a)))
                ((ppcre "^(query|q)$")
                 (baf `("env" "--query" ,@a)))
                ((ppcre "^(upgrade|U)$")
                 (baf `("env" "--upgrade" ,@a)))
                ((ppcre "^(upgrade-always|U-a)$")
                 (baf `("upgrade" "--always" ,@a)))
                ((ppcre "^(install|i)$")
                 (baf `("env" "--install" "-A"
                                ,@(loop :for pkg :in a
                                     :collect (format nil "~A.~A" (run/ss `(,self "channel-name")) pkg)))))
                ((ppcre "^(Install|I)$")
                 (baf `("env" "--install" "-A" ,@a)))
                ((ppcre "^(query-available|q-a)$")
                 (baf `("query" "--available" "-P" ,@a)))
                ((ppcre "^(compare-versions|c-v)$")
                 (baf `("query" "--compare-versions" ,@a)))
                ((ppcre "^(compare-versions-less-than|c-v-l-t)$")
                 (run/i `(pipe (baf `("compare-versions" ,@a)) (grep "<"))))
                ((ppcre "^(compare-versions-equal|c-v-e)$")
                 (run/i `(pipe (baf `("compare-versions" ,@a)) (grep "="))))
                ((ppcre "^(compare-versions-greater-than|c-v-g-t)$")
                 (run/i `(pipe (baf `("compare-versions" ,@a)) (grep ">"))))
                ((ppcre "^(describe-available|d-a)$")
                 (baf `("query-available" "--description" ,@a)))
                ((ppcre "^(index-available|i-a)$")
                 (run/i `(pipe (baf ("query-available")) (gzip "-c" (> ,(index-channels))))))
                ((ppcre "^(search-available|search|s-a|s)$")
                 (run! `(zgrep "--color" "-i" ,@a ,(index-channels))))
                ((ppcre "^(view-available|v-a)$")
                 (run! `(zless ,(index-channels))))

                ;; upstream
                ((ppcre "^(upstream-env|u-e)$")
                 (baf `("env" "-f" ,(default-nix) ,@a)))
                ((ppcre "^(upstream-build|u-b)$")
                 (baf `("build" "-I" ,(format nil "nixpkgs=~A" (nixpkgs)) ,@a)))
                ((ppcre "^(upstream-query|u-q)$")
                 (baf `("upstream-env" "--query" ,@a)))
                ((ppcre "^(upstream-upgrade|u-U)$")
                 (baf `("upstream-env" "--upgrade" ,@a)))
                ((ppcre "^(upstream-upgrade-always|u-U-a)$")
                 (baf `("upstream-upgrade" "--always" ,@a)))
                ((ppcre "^(upstream-install|u-i)$")
                 (baf `("upstream-env" "--install" "-A" ,@a)))
                ((ppcre "^(upstream-Install|u-I)$")
                 (baf `("upstream-env" "--install" "-A" ,@a)))
                ((ppcre "^(upstream-query-available|u-q-a)$")
                 (baf `("upstream-query" "--available" "-P" ,@a)))
                ((ppcre "^(upstream-compare-versions|u-c-v)$")
                 (baf `("upstream-query" "--compare-versions" ,@a)))
                ((ppcre "^(upstream-compare-versions-less-than|u-c-v-l-t)$")
                 (run/i `(pipe (baf ("upstream-compare-versions" ,@a)) (grep "<"))))
                ((ppcre "^(upstream-compare-versions-equal|u-c-v-e)$")
                 (run/i `(pipe (baf ("upstream-compare-versions" ,@a)) (grep "="))))
                ((ppcre "^(upstream-compare-versions-greater-than|u-c-v-g-t)$")
                 (run/i `(pipe (baf ("upstream-compare-versions" ,@a)) (grep ">"))))
                ((ppcre "^(upstream-describe-available|u-d-a)$")
                 (baf `("upstream-query-available" "--description" ,@a)))
                ((ppcre "^(upstream-index-available|u-i-a)$")
                 (run/i `(pipe (baf ("upstream-query-available")) (gzip "-c" (> ,(index-upstream))))))
                ((ppcre "^(upstream-search-available|u-search|u-s-a|u-s)$")
                 (run! `(zgrep "--color" "-i" ,@a ,(index-upstream))))
                ((ppcre "^(upstream-view-available|u-v-a)$")
                 (run! `(zless ,(index-upstream))))

                ;; installed
                ((ppcre "^(view-installed|v-i)$")
                 (run! `(zless ,(index-installed))))
                ((ppcre "^(search-installed|s-i)$")
                 (run! `(zgrep "--color" "-i" ,@a ,(index-installed))))
                ((ppcre "^(index-installed|i-i)$")
                 (run/i `(pipe (baf ("query-installed")) (gzip "-c" (> ,(index-installed))))))
                ((ppcre "^(describe-installed|d-i)$")
                 (baf `("query-installed" "--description" ,@a)))
                ((ppcre "^(query-installed|q-i)$")
                 (baf `("query" "--installed" ,@a)))

                ;; common
                ((ppcre "^(uninstall|remove)$")
                 (baf `("env" "--uninstall" ,@a)))
                ((ppcre "^(build-index|index)$")
                 (loop :for command :in '("index-available" "upstream-index-available" "index-installed")
                    :do (baf `(,command))))
                ((ppcre "^(upstream-update|u-u)$")
                 (with-current-directory ((nixpkgs))
                   (when (string= (run/ss `(git "rev-parse" "--abbrev-ref" "HEAD")) "master")
                     (run! `(git "pull" "origin" "master")))))
                ((ppcre "^(full-update|f-u|complete-update)$")
                 (loop :for command :in '("channel-update" "root-channel-update" "upstream-update" "build-index")
                    :do (baf `(,command))))
                ((ppcre "^(full-upgrade|f-U|complete-upgrade)$")
                 (loop :for command :in '("full-update" "upstream-upgrade" "rebuild-switch-upgrade")
                    :do (baf `(,command))))
                ((ppcre "^(full-search|f-s)$")
                 (loop :for command :in '("search-available" "upstream-search-available")
                    :do (baf `(,command ,@a))))

                ;; miscellany

                ((ppcre "^(view-packages|v-p)$")
                 (baf `("query-available" "-A" ,(format nil "~A.~A" (run/ss `(,self "channel-name")) (first a)))))
                ((ppcre "^(upstream-view-packages|u-v-p)$")
                 (baf `("upstream-query-available" "-A" ,(first a))))

                ((ppcre "^(make)$")
                 (baf `("pure-shell" "--run" "make" ,@a)))
                ((ppcre "^(nix-version)$")
                 (baf `("env" "--version")))
                ((ppcre "^(nixpkgs-version)$")
                 (baf `("instantiate" "--eval" "<nixpkgs>" "-A" "lib.nixpkgsVersion")))
                ((ppcre "^(nixos-version)$")
                 (run! `(nixos-version)))
                ((ppcre "^(version)$")
                 (loop :for command :in '("nix" "nixpkgs" "nixos")
                    :do (baf `(,(format nil "~A-version" command)))))

                ;; prefetch
                ((ppcre "^(fetch-url)$")
                 (run! `(nix-prefetch-url ,@a)))
                ((ppcre "^(fetch-file)$")
                 (baf `("fetch-url" ,(format nil "file://~A" (first a)))))
                ((ppcre "^(fetch-git)$")
                 (run! `(nix-prefetch-git ,@a)))
                ((ppcre "^(fetch-zip)$")
                 (run! `(nix-prefetch-zip ,@a)))
                ((ppcre "^(fetch-hg)$")
                 (run! `(nix-prefetch-hg ,@a)))
                ((ppcre "^(fetch-svn)$")
                 (run! `(nix-prefetch-svn ,@a)))
                ((ppcre "^(fetch-bzr)$")
                 (run! `(nix-prefetch-bzr ,@a)))
                ((ppcre "^(fetch-cvs)$")
                 (run! `(nix-prefetch-cvs ,@a)))
                (else (display-usage)))
              )))
   (success)))

(defun main (&rest args)
   (apply #'baf args)
   (success))

(register-commands :baf/baf)
