;;; -*- Lisp -*-

;; Add your favorite implementation if you manage to get cl-pdf working with it.
#|#+(or sbcl clisp clozure lispworks)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :exscribe-typeset *features*))|#

(defsystem :exscribe
  :description "Programmatically create HTML documents from a high-level syntax"
  :long-description "Exscribe helps you author documents and produce HTML output,
using a high-level syntax (Scribble) completely integrated with the CL syntax.
It notably features proper support for footnotes, table-of-contents, bibliography."
  :depends-on (:cl-launch ;; Always put cl-launch first
               :scribble :fare-matcher :fare-utils
	       #+exscribe-typeset :cl-typesetting)
  :components ((:file "packages")
	       (:file "macros" :depends-on ("packages"))
	       (:file "specials" :depends-on ("macros"))
	       (:file "scheme-compat" :depends-on ("specials"))
	       (:file "exscribe" :depends-on ("scheme-compat"))
	       (:file "exscribe-data" :depends-on ("exscribe"))
	       (:file "bibliography" :depends-on ("exscribe-data"))
	       (:file "html-dumper" :depends-on ("packages"))
	       (:file "exscribe-html" :depends-on ("exscribe-data" "bibliography" "html-dumper"))
	       (:file "exscribe-txt" :depends-on ("exscribe-data"))
	       #+exscribe-typeset
	       (:file "exscribe-typeset" :depends-on ("exscribe-txt"))))
