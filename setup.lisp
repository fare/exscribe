(in-package :cl-user)

;;; 1- Configure your ASDF installation.
(dolist (p '(#p"/usr/share/common-lisp/systems/" ;; in case we're running on Debian without clc
             (merge-pathnames ".local/share/common-lisp/systems/" (user-homedir-pathname)))) ;; my stuff
  (pushnew p asdf:*central-registry* :test #'equal))

;;; 2- Tweak the compile settings to your heart's content
#-allegro
(proclaim '(optimize (speed 3) (safety 2) (space 1) (debug 1)
            #+sbcl (compilation-speed 0)
            ;;#+sbcl (sb-ext:inhibit-warnings 3)
            ;;#+cmu (ext:inhibit-warnings 3)
            ))

(cl-launch::DBG :exscribe-setup
		asdf:*central-registry*)

;; Add your favorite implementation if you manage to get cl-pdf working with it.
;; Need that in both asd and here for XCVB to be happy...
#+(or sbcl clisp openmcl lispworks)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :exscribe-typeset *features*))
