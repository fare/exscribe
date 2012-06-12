#|
make -C ~/fare/lisp exscribe-install
ln -s exscribe ~/bin/scribe

time LISP=cmucl exscribe -I ~/fare/lisp/exscribe -I ~/fare/www -o ~/html/liberty/microsoft_monopoly.html liberty/microsoft_monopoly
time LISP=cmucl exscribe -o /dev/null /dev/null
time /usr/local/bin/scribe -I ~/fare/www -o ~/html/liberty/microsoft_monopoly.html ~/fare/www/liberty/microsoft_monopoly.scr
time /usr/local/bin/scribe -t html -o /dev/null /dev/null

(defparameter *a 'foo) (defparameter *b '(bar baz quux))
(defparameter *c '((list :x x) (list :y y)))
``(f ,@,@*c)

(fare-quasiquote:enable-quasiquote)
``(,@,@*c)

(let ((kw :hr)(options ())(tag 'hrule))
  `(define-markup (,tag &key ,@options &rest content)
     `((,',kw
	,@,@(mapcar #'(lambda (option) `(opt ,option))
		    options))
       ,@content)))


(load "/home/fare/src/cl-launch/header.lisp")
(load "/usr/share/common-lisp/source/asdf/asdf.lisp")
(cl-launch::load-system :exscribe)
(asdf:oos 'asdf:load-op :exscribe)
(asdf:oos 'asdf:load-op :environment)
;(asdf::class-for-type (asdf:find-system :exscribe) :module)

(setf *compile-verbose* t *compile-print* t)

(require :exscribe)
(asdf:oos 'asdf:load-op :exscribe)
(in-package :exscribe-user)
(exscribe::add-exscribe-path "/home/fare/fare/www")
(exscribe::reset-exscribe)
(setf *bibliography* (make-hash-table :test 'equalp))
(style 'liberty/microsoft_monopoly)

TODO:
(1) atomic change of output file.
(2) section :number #f

|#

(in-package :exscribe)

#|
(progn
  #.(progn (reenable-scribble-syntax) nil)
  (terpri)
  (html (values `[Hello, ,,(+ 2 2) ,[:b world]]) nil)
  (terpri))
|#

(defun slurp (f)
  (xcvb-driver:slurp-file-lines f))

(defun scrulp (f)
  (reenable-scribble-syntax)
  (slurp f))

(defvar *h* nil)

(defun in-home (f)
  (strcat (cl-launch:getenv "HOME") "/" f))

;;(setq *h* (scrulp (in-home "fare/www/index.scr")))

;(setf exscribe::*exscribe-mode* 'pdf)

(in-package :exscribe)
(add-exscribe-path "/home/fare/fare/www")
(trace exscribe-load-style cl-launch:compile-and-load-file compile-file load)
(process-file "/home/fare/fare/www/index.scr")


)

(load "/home/fare/fare/lisp/exscribe/setup")
(asdf:oos 'asdf:load-op :exscribe) (in-package :exscribe-user) (exscribe::add-exscribe-path "/home/fare/fare/www") (setf exscribe::*exscribe-mode* 'pdf) (exscribe::process-file "/home/fare/fare/www/liberty/microsoft_monopoly.scr" :into "/home/fare/fare/lisp/exscribe/microsoft_monopoly.pdf")


(in-package :exscribe)
(exscribe::reset-exscribe)
(setf *bibliography* (make-hash-table :test 'equalp))
