;;;; barf.lisp

(uiop:define-package :barf/barf
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-ppcre
          #:cl-launch/dispatch
          #:barf/utils)
  (:export #:barf
           #:main))

(in-package :barf/barf)


;;;-------------------------------------------------------------------------------------------------
;;; Variables
;;;-------------------------------------------------------------------------------------------------

(defparameter +self+
  (or (argv0) "barf")
  "The name of this program.")

(defparameter +version+
  "1.0.0"
  "The version of this program.")

(defparameter +http-repository+
  "https://github.com/NixOS/nixpkgs.git"
  "The remote repository for Nixpkgs sources via HTTP.")

(defparameter +git-repository+
  "git@github.com:NixOS/nixpkgs.git"
  "The remote repository for Nixpkgs sources via Git.")


;;;-------------------------------------------------------------------------------------------------
;;; Functions
;;;-------------------------------------------------------------------------------------------------

(defun display-usage ()
  "Display program usage."
  (format t "Usage: ~A [COMMAND]... [OPTION]...

See https://github.com/ebzzry/barf for more information~%"
          +self+))

(defun base-dir ()
  "Return the base directory where to store barf data"
  (subpathname (user-homedir-pathname) ".barf/"))

(defun base-path (path)
  "Return a path relative to BASE-DIR."
  (subpathname (base-dir) path))

(defun nixpkgs ()
  "Return the local path of the Nixpkgs directory."
  (base-path "nixpkgs/"))

(defun index ()
  "Return the local path of the index."
  (base-path "index/"))

(defun default-nix ()
  "Return the local path of the top-level Nixpkgs file."
  (base-path "nixpkgs/default.nix"))

(defun find-machine-id ()
  "Return the path to the machine-id file."
  (loop :for path :in '("/etc/machine-id" "/var/lib/dbus/machine-id")
        :when (uiop:file-exists-p path)
        :return path))

(defun index-path (name)
  "Return the index file for the current host."
  (let* ((base (format nil "~A.~A" name (hostname)))
         (id-file (or (find-machine-id) ""))
         (path (if (uiop:file-exists-p id-file)
                   (format nil "~A.~A.gz" base (string-trim '(#\space #\tab #\newline)
                                                            (uiop:read-file-string id-file)))
                   (format nil "~A.gz" base))))
    (subpathname (base-path "index/") path)))

(defun profile-path (path)
  "Return a profile directory."
  (let ((path (subpathname (base-path "profiles/") path)))
    (ensure-directories-exist path)
    path))

(defun index-channels ()
  "Return the index file for the channels."
  (index-path "channels"))

(defun index-upstream ()
  "Return the index file for the upstream."
  (index-path "upstream"))

(defun index-installed ()
  "Return the index file for installed applications."
  (index-path "installed"))

(defun run! (cmd)
  "Run command CMD muffling its errors."
  (run/interactive cmd :on-error nil))

(defun ensure-nixpkgs ()
  "Fetch the Nixpkgs repository if it does not exist yet."
  (and (directory-exists-p (nixpkgs))
       (delete-directory-tree (physicalize-pathname (nixpkgs)) :validate t))
  (ensure-directories-exist (base-dir))
  (unless (file-exists-p (default-nix))
    (with-current-directory ((base-dir))
      (run/i `(git "clone" ,+http-repository+)))))

(defun ensure-index ()
  "Build the local index unconditionally."
  (and (directory-exists-p (index))
       (delete-directory-tree (physicalize-pathname (index)) :validate t))
  (ensure-directories-exist (index))
  (run/i `(,(argv0) "index")))

(defun nixosp ()
  "Return true if we are on NixOS."
  (file-exists-p "/etc/nixos/configuration.nix"))

(defun delete-tree (path)
  "Delete a directory tree."
  (let ((path (ensure-directory-pathname path)))
    (delete-directory-tree path :validate t)))

(defun remove-profile (profile)
  "Remove a profile path."
  (delete-tree (profile-path profile)))

(exporting-definitions
  (defun barf (args)
    "Top-level command for managing Nix facilities."
    (cond ((null args) (display-usage))
          (t (let ((self (argv0))
                   (op (first args))
                   (a (rest args)))
               (match op
                 ((ppcre "^(init)$")
                  (ensure-nixpkgs)
                  (ensure-index))
                 ((ppcre "^(out-path|o-p)$")
                  (match (run/ss `(,self "query" "--out-path" ,(last a)))
                    ((ppcre ".*? (/.*)" path) (format t "~A~%" path))))
                 ((ppcre "^(ls)$")
                  (let* ((item (first (last a)))
                         (position (position #\/ item))
                         (length (length item))
                         (package (subseq item 0 (or position length)))
                         (sub-directory (if position (subseq item position length) ""))
                         (location (match (run/ss `(,self "query" "--out-path" ,package))
                                     ((ppcre ".*? (/.*)" path) (format nil "~A" path)))))
                    (when location
                      (run! `(ls ,@(butlast a) ,(concatenate 'string location sub-directory))))))
                 ((ppcre "^(which|h)$")
                  (run! `(command-not-found ,@a)))
                 ((ppcre "^(store)$")
                  (run! `(nix-store ,@a)))
                 ((ppcre "^(repl)$")
                  (run! `(nix-repl ,@a)))
                 ((ppcre "^(impure-shell|i-s)$")
                  (run! `(nix-shell ,@a)))
                 ((ppcre "^(pure-shell|p-s|shell)$")
                  (barf `("impure-shell" "--pure" ,@a)))
                 ((ppcre "^(rebuild)$")
                  (run! `(sudo nixos-rebuild ,@a)))
                 ((ppcre "^(rebuild-switch|r-s)$")
                  (barf `("rebuild" "switch" ,@a)))
                 ((ppcre "^(rebuild-switch-upgrade|r-s-u)$")
                  (barf `("rebuild-switch" "--upgrade" ,@a)))
                 ((ppcre "^(instantiate)$")
                  (run! `(nix-instantiate ,@a)))
                 ((ppcre "^(eval)$")
                  (barf `("instantiate" "--eval" "--strict" "--show-trace" ,@a)))
                 ((ppcre "^(grep|g|rg)$")
                  (with-current-directory ((nixpkgs))
                    (run! `(fd "--extension" "nix" "--exec" "rg" ,@a "{}"))))
                 ((ppcre "^(find|fd|f)$")
                  (run! `(fd ,@a ,(nixpkgs))))
                 ((ppcre "^(install-package|i-p)$")
                  (run! `(sudo "nix-install-package" ,@a)))
                 ((ppcre "^(install-package-uri|i-p-u)$")
                  (barf `("install-package" "--non-interactive" "--url" ,@a)))

                 ((ppcre "^(references|r)$")
                  (barf `("store" "-q" "--references" ,@a)))
                 ((ppcre "^(referrers|R)$")
                  (barf `("store" "-q" "--referrers" ,@a)))

                 ((ppcre "^(query-root|q-r)$")
                  (run! `(sudo "nix-env" "--query" ,@a)))
                 ((ppcre "^(closure|c)$")
                  (barf `("store" "-qR" ,@a)))
                 ((ppcre "^(set-flag|s-f)$")
                  (barf `("env" "--set-flag" ,@a)))
                 ((ppcre "^(option|o)$")
                  (run! `(nixos-option ,@a)))
                 ((ppcre "^(garbage-collect|g-c)$")
                  (barf `("store" "--gc" ,@a)))
                 ((ppcre "^(garbage-collect-delete|g-c-d)$")
                  (run! `(sudo "nix-collect-garbage" "-d" ,@a)))

                 ((ppcre "^(channel|ch)$")
                  (run! `(nix-channel ,@a)))
                 ((ppcre "^(channel-list|ch-l)$")
                  (barf `("channel" "--list" ,@a)))
                 ((ppcre "^(channel-add|ch-a)$")
                  (barf `("channel" "--add" ,@a)))
                 ((ppcre "^(channel-remove|ch-r)$")
                  (barf `("channel" "--remove" ,@a)))
                 ((ppcre "^(channel-update|ch-u)$")
                  (barf `("channel" "--update" ,@a)))
                 ((ppcre "^(channel-name|ch-n)$")
                  (match (run/ss `(,self "channel-list" ,@a))
                    ((ppcre "^(.*?) .*" name)
                     (format t "~A~%" name))))

                 ((ppcre "^(root-channel|r-ch)$")
                  (and (nixosp) (run! `(sudo "nix-channel" ,@a))))
                 ((ppcre "^(root-channel-list|r-ch-l)$")
                  (barf `("root-channel" "--list" ,@a)))
                 ((ppcre "^(root-channel-add|r-ch-a)$")
                  (barf `("root-channel" "--add" ,@a)))
                 ((ppcre "^(root-channel-remove|r-ch-r)$")
                  (barf `("root-channel" "--remove" ,@a)))
                 ((ppcre "^(root-channel-update|r-ch-u)$")
                  (barf `("root-channel" "--update" ,@a)))
                 ((ppcre "^(root-channel-name|r-ch-n)$")
                  (match (run/ss `(,self "root-channel-list" ,@a))
                    ((ppcre "^(.*?) .*" name)
                     (format t "~A~%" name))))

                 ;; channels
                 ((ppcre "^(env)$")
                  (run! `(nix-env ,@a)))
                 ((ppcre "^(build|b)$")
                  (run! `(nix-build ,@a)))
                 ((ppcre "^(query|q)$")
                  (barf `("env" "--query" ,@a)))
                 ((ppcre "^(upgrade|U)$")
                  (barf `("env" "--upgrade" ,@a)))
                 ((ppcre "^(upgrade-always|U-a)$")
                  (barf `("upgrade" "--always" ,@a)))
                 ((ppcre "^(install|i)$")
                  (barf `("env" "--install" "-A"
                               ,@(loop :for name :in a
                                       :collect (format nil "~A.~A" (run/ss `(,self "channel-name")) name)))))
                 ((ppcre "^(Install|I)$")
                  (barf `("env" "--install" "-A" ,@a)))
                 ((ppcre "^(query-available|q-a)$")
                  (barf `("query" "--available" "-P" ,@a)))
                 ((ppcre "^(compare-versions|c-v)$")
                  (barf `("query" "--compare-versions" ,@a)))
                 ((ppcre "^(compare-versions-less-than|c-v-l-t)$")
                  (run! `(pipe (barf ("compare-versions" ,@a)) (rg "-N" "<")))
                  (success))
                 ((ppcre "^(compare-versions-equal|c-v-e)$")
                  (run! `(pipe (barf ("compare-versions" ,@a)) (rg "-N" "=")))
                  (success))
                 ((ppcre "^(compare-versions-greater-than|c-v-g-t)$")
                  (run! `(pipe (barf ("compare-versions" ,@a)) (rg "-N" ">")))
                  (success))
                 ((ppcre "^(describe-available|d-a)$")
                  (barf `("query-available" "--description" ,@a)))
                 ((ppcre "^(index-available|i-a)$")
                  (run! `(pipe (barf ("query-available")) (gzip "-c" (> ,(index-channels))))))
                 ((ppcre "^(search-available|search|s-a|s)$")
                  (loop :for name :in a :do (run! `(rg "-N" "--color=auto" "-z" "-i" ,name ,(index-channels)))))
                 ((ppcre "^(view-available|v-a)$")
                  (run! `(zless ,(index-channels))))
                 ((ppcre "^(profile|p)$")
                  (barf `("env" "-p" ,(profile-path (first a)) ,@(rest a))))

                 ;; upstream
                 ((ppcre "^(upstream-env|u-env)$")
                  (barf `("env" "-f" ,(default-nix) ,@a)))
                 ((ppcre "^(upstream-build|u-b)$")
                  (barf `("build" "-I" ,(format nil "nixpkgs=~A" (nixpkgs)) ,@a)))
                 ((ppcre "^(upstream-query|u-q)$")
                  (barf `("upstream-env" "--query" ,@a)))
                 ((ppcre "^(upstream-upgrade|u-U)$")
                  (barf `("upstream-env" "--upgrade" ,@a)))
                 ((ppcre "^(upstream-upgrade-always|u-U-a)$")
                  (barf `("upstream-upgrade" "--always" ,@a)))
                 ((ppcre "^(upstream-install|u-i)$")
                  (barf `("upstream-env" "--install" "-A" ,@a)))
                 ((ppcre "^(upstream-Install|u-I)$")
                  (barf `("upstream-env" "--install" "-A" ,@a)))
                 ((ppcre "^(upstream-query-available|u-q-a)$")
                  (barf `("upstream-query" "--available" "-P" ,@a)))
                 ((ppcre "^(upstream-compare-versions|u-c-v)$")
                  (barf `("upstream-query" "--compare-versions" ,@a)))
                 ((ppcre "^(upstream-compare-versions-less-than|u-c-v-l-t)$")
                  (run! `(pipe (barf ("upstream-compare-versions" ,@a)) (rg "-N" "<")))
                  (success))
                 ((ppcre "^(upstream-compare-versions-equal|u-c-v-e)$")
                  (run! `(pipe (barf ("upstream-compare-versions" ,@a)) (rg "-N" "=")))
                  (success))
                 ((ppcre "^(upstream-compare-versions-greater-than|u-c-v-g-t)$")
                  (run! `(pipe (barf ("upstream-compare-versions" ,@a)) (rg "-N" ">")))
                  (success))
                 ((ppcre "^(upstream-describe-available|u-d-a)$")
                  (barf `("upstream-query-available" "--description" ,@a)))
                 ((ppcre "^(upstream-index-available|u-i-a)$")
                  (run! `(pipe (barf ("upstream-query-available")) (gzip "-c" (> ,(index-upstream))))))
                 ((ppcre "^(upstream-search-available|u-search|u-s-a|u-s)$")
                  (loop :for name :in a :do (run! `(rg "-N" "--color=auto" "-z" "-i" ,name ,(index-upstream)))))
                 ((ppcre "^(upstream-view-available|u-v-a)$")
                  (run! `(zless ,(index-upstream))))
                 ((ppcre "^(upstream-profile|u-p)$")
                  (barf `("upstream-env" "-p" ,(profile-path (first a)) ,@(rest a))))

                 ;; installed
                 ((ppcre "^(query-installed|q-i)$")
                  (barf `("query" "--installed" ,@a)))
                 ((ppcre "^(query-installed-names|q-i-n)$")
                  (run! `(pipe (barf ("query-installed") ,@a) (sed "-e" "s/-[0-9].*//"))))
                 ((ppcre "^(search-installed|s-i)$")
                  (run! `(rg "-N" "--color=auto" "-z" "-i" ,@a ,(index-installed))))
                 ((ppcre "^(index-installed|i-i)$")
                  (run! `(pipe (barf ("query-installed")) (gzip "-c" (> ,(index-installed))))))
                 ((ppcre "^(describe-installed|d-i)$")
                  (barf `("query-installed" "--description" ,@a)))

                 ;; common
                 ((ppcre "^(uninstall|remove|erase|e)$")
                  (barf `("env" "--uninstall" ,@a)))
                 ((ppcre "^(build-index|b-i|index)$")
                  (loop :for command :in '("index-available" "upstream-index-available" "index-installed")
                        :do (barf `(,command))))
                 ((ppcre "^(upstream-update|u-u)$")
                  (with-current-directory ((nixpkgs))
                    (when (string= (run/ss `(git "rev-parse" "--abbrev-ref" "HEAD")) "master")
                      (run! `(git "pull" "origin" "master")))))
                 ((ppcre "^(full-update|f-u|complete-update)$")
                  (let* ((base-commands '("channel-update" "upstream-update" "build-index"))
                         (commands (if (nixosp)
                                       (append '("root-channel-update") base-commands)
                                       base-commands)))
                    (loop :for command :in commands
                          :do (barf `(,command)))))
                 ((ppcre "^(full-upgrade|f-U|complete-upgrade)$")
                  (loop :for command :in '("full-update" "upgrade" "upstream-upgrade" "rebuild-switch-upgrade")
                        :do (barf `(,command))))
                 ((ppcre "^(full-search|f-s)$")
                  (loop :for command :in '("search-available" "upstream-search-available")
                        :do (barf `(,command ,@a))))
                 ((ppcre "^(remove-profile|r-p)$")
                  (loop :for profile :in a :do (remove-profile profile)))

                 ;; miscellany
                 ((ppcre "^(view-packages|v-p)$")
                  (barf `("query-available" "-A" ,(format nil "~A.~A" (run/ss `(,self "channel-name")) (first a)))))
                 ((ppcre "^(upstream-view-packages|u-v-p)$")
                  (barf `("upstream-query-available" "-A" ,(first a))))

                 ((ppcre "^(make)$")
                  (barf `("pure-shell" "--run" "make" ,@a)))
                 ((ppcre "^(nix-version)$")
                  (barf `("env" "--version")))
                 ((ppcre "^(nixpkgs-version)$")
                  (barf `("instantiate" "--eval" "<nixpkgs>" "-A" "lib.version")))
                 ((ppcre "^(nixos-version)$")
                  (run! `(nixos-version)))
                 ((ppcre "^(version)$")
                  (loop :for command :in '("nix" "nixpkgs" "nixos")
                        :do (barf `(,(format nil "~A-version" command)))))
                 ((ppcre "^(cleanup)$")
                  (run! `(sudo "nix-collect-garbage" "--delete-older-than" "14d" ,@a))
                  (barf `("rebuild" "boot")))

                 ;; prefetch
                 ((ppcre "^(fetch-url)$")
                  (run! `(nix-prefetch-url ,@a)))
                 ((ppcre "^(fetch-file)$")
                  (barf `("fetch-url" ,(format nil "file://~A" (first a)))))
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
                 (t (display-usage))))))
    (success)))

(defun main (&rest args)
  "The top-level entry point."
  (handler-case (apply #'barf args)
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     #+lispworks mp:process-interrupt
     () nil)
    (error (c)
      (format t "Woops, an unknown error occured:~&~a~&" c)))
  (success))

(register-commands :barf/barf)
