;;; -*- Lisp -*-

#-asdf3 (error "Exscribe requires ASDF3")

(defsystem "exscribe"
  :version (:read-file-form "specials.lisp" :at (1 2))
  :description "Programmatically create HTML documents from a high-level syntax"
  :long-description "Exscribe helps you author documents and produce HTML output,
using a high-level syntax (Scribble) completely integrated with the CL syntax.
It notably features proper support for footnotes, table-of-contents, bibliography."
  :author "Francois-Rene Rideau"
  :license "MIT" ;; also bugroff
  :depends-on ((:version "scribble" "1.0.0")
               (:version "fare-utils" "1.0.0")
               "trivia.quasiquote"
               "alexandria"
               "fare-memoization"
               "fare-scripts/rescript"
               "quri")
  :entry-point "exscribe::entry-point"
  :components ((:file "packages")
               (:file "macros" :depends-on ("packages"))
               (:file "specials" :depends-on ("macros"))
               (:file "scheme-compat" :depends-on ("specials"))
               (:file "exscribe" :depends-on ("scheme-compat"))
               (:file "exscribe-data" :depends-on ("exscribe"))
               (:file "bibliography" :depends-on ("exscribe-data"))
               (:file "html-dumper" :depends-on ("packages"))
               (:file "exscribe-html" :depends-on ("exscribe-data" "bibliography" "html-dumper"))
               (:file "exscribe-txt" :depends-on ("exscribe-data"))))

(defsystem "exscribe/typeset"
  :description "CL-Typesetting backend for Exscribe"
  :depends-on ("exscribe" "cl-typesetting")
  :components ((:file "exscribe-typeset"))
  :build-operation program-op
  :build-pathname "exscribe"
  :entry-point "exscribe::entry-point")

(defsystem "exscribe/executable"
  :description "Exscribe and all extensions"
  :in-order-to ((load-op (program-op "exscribe/typeset"))))
