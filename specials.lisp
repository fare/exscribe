;;; -*- lisp -*-

#+xcvb (module (:depends-on ("exscribe/packages")))

(in-package :exscribe)

(defparameter *exscribe-version* "0.95")
;;#.(multiple-value-bind (sec min hr day mo yr) (get-decoded-time) sec min hr (format nil "~A-~A-~A" yr mo day))

(defvar *exscribe-initialized* nil "has exscribe already been initialized?")

(defun ensure-directory-pathname (d)
  (typecase d
    (string (parse-namestring (concatenate 'string d "/")))
    (pathname (make-pathname :name nil :type nil :version nil :defaults d))))

(defvar *exscribe-directory*
  (ensure-directory-pathname
   (or #.(or *compile-file-pathname* *load-pathname*)
       *compile-file-pathname* *load-pathname*)))

(defvar *exscribe-path*
  (and *exscribe-directory* (list *exscribe-directory*))
  "The search path for exscribe style files")


(defvar *exscribe-mode* 'html "The current output mode used by exscribe")

(defvar *exscribe-verbose* nil)

(defparameter *mode-suffixes*
  '((html "html" "htm")
    (pdf "pdf")))

(defvar *document* nil "The current document")
(defvar *document-title* nil "The current document's title")

(defvar *header* nil
  "header for the current document body")
(defvar *footer* nil
  "footer for the current document body")

(defvar *toc* nil "table of contents")

(defvar *section-counter* 0 "counter for sections")
(defvar *subsection-counter* 0 "counter for subsections")
(defvar *subsubsection-counter* 0 "counter for subsubsections")
(defvar *section-name* nil "name of current section")
(defvar *subsection-name* nil "name of current subsection")
(defvar *subsubsection-name* nil "name of current subsubsection")
(defvar *sections-by-label* (make-hash-table :test 'equal) "mapping from names to sections")

(defvar *footnotes* nil "place-holder for footnotes being currently processed")
(defvar *footnote-counter* 0 "counter for footnotes")
(defvar *footnotes-title* "Notes")

(defvar *background* "white" "main text background color")
(defvar *foreground* "black" "main text foreground color")
(defvar *title-background* "#FFDB31" "header title background color")
(defvar *section-title-background* "#FFC189" "section title background color")

(defvar *bibliography* nil)
(defvar *bibliography-options* nil)
(defvar *bibliography-location* nil)
(defvar *bibliography-header* nil)

(defvar *exscribe-document-hook* nil)
(defvar *postprocess-hooks* nil
  "functions to run after a first pass on the document")

(defvar *loaded-styles* nil)
(defvar *latest-style-date* nil)
