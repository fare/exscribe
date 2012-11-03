#+xcvb
(module
 (:depends-on
  ("exscribe/packages"
   "exscribe/macros"
   "exscribe/specials")))

(in-package :exscribe-data)

(defvar *todo-list*
'(book misc article
  title author year url journal month
  ))

(defun replace-cons! (c new-cons)
  (setf (car c) (car new-cons)
	(cdr c) (cdr new-cons)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-araneida-compat
  (progn
    (defmacro make-tag (tag attr contents)
      `(vector ,tag ,attr ,contents))
    (defpattern tag (tag attr contents) `(vector ,tag ,attr ,contents))
    (defun replace-tag! (x tag attr contents)
      (setf (svref x 0) tag
	    (svref x 1) attr
	    (svref x 2) contents))
    (defmacro tag-contents (x) `(svref ,x 2))
    (defun *list (&rest x) x))
  #+araneida-compat
  (progn
    (defmacro make-tag (tag attr contents)
      `(cons (cons ,tag ,attr) ,contents))
    (defpattern tag (tag attr contents) `(cons (cons ,tag ,attr) ,contents))
    (defun replace-tag! (x tag attr contents)
      (setf (car x) (cons tag attr)
	    (cdr x) contents))
    (defmacro tag-contents (x contents) `(cdr ,x))
    (defun *list (&rest x) (cons 'list x))))

(defun copy-tag (to from)
  (ematch from ((tag x y z) (replace-tag! to x y z))))

(defmacro deftag (tag &optional kw options)
  (unless kw (setf kw (conc-keyword tag)))
  (let* ((got-it (mapcar #'(lambda (x) (gensym (string x))) options))
	 (opt-spec (mapcar #'(lambda (x y) `(,x nil ,y)) options got-it))
	 (opt-copier (mapcar #'(lambda (x y)
				 `(when ,y `(,,(conc-keyword x) ,,x)))
			     options got-it)))
    `(define-markup (,tag &key ,@opt-spec &rest content)
       (make-tag ',kw (append ,@opt-copier) content))))

(defmacro defsimpletag (tag html)
  `(deftag ,tag ,html))
(defmacro defsimpletags (&rest r)
  `(progn
    ,@(loop while (and (consp r) (consp (cdr r)))
	    collect `(defsimpletag ,(car r) ,(cadr r))
	    do (setf r (cddr r)))))
(defsimpletags
    id () ;; identity function
    emph :em
    bold :b
    it :i
    underline :u
    blockquote :blockquote
    code ()
    sup ()
    sub ()
    br ()
    ;linebreak :br
    hrule :hr
    hr ()
    tt ()
    pre ()
    head ()
    html ()
    title ()
    )

(defmacro define-header-tag (tag)
  `(deftag ,tag nil (align textcolor bgcolor)))
(defmacro define-header-tags (&rest r)
  `(progn ,@(loop for i in r collect `(define-header-tag ,i))))
(define-header-tags
    h1 h2 h3 h4 h5 h6)

(deftag body () (color bgcolor))
(deftag p () (align style))
(deftag font () (size face color bgcolor))
(deftag td () (align valign width height color bgcolor colspan rowspan))
(deftag th () (align valign width height color bgcolor colspan rowspan))
(deftag tr () (align valign width height color bgcolor))
(deftag table () (align valign width height cellpadding))
(deftag img () (src border alt))
(deftag enumerate :ol ())
(deftag itemize :ul ())
(deftag item :li ())
(deftag a () (href name))
(deftag span () (style))
(deftag div () (style))

(deftag make-document :document (title author))
(deftag author () (name url email affiliation))
(deftag footnote () (note))
(deftag ref () (url bib section subsection subsubsection))
(deftag table-of-contents () (section subsection subsubsection))
(deftag section () (number heading title toc refid label))
(deftag subsection () (number heading title toc refid label))
(deftag subsubsection () (number heading title toc refid label))

(define-markup (center . x)
  (apply 'p :align 'center x))
(defun small (&rest x) (font :size -1 (apply 'id x)))

(defun document-walk-further (doc walker)
  (cond
    ((consp doc)
     #-araneida-compat
     (progn
       (funcall walker (car doc))
       (funcall walker (cdr doc)))
     #+araneida-compat
     (mapc walker (cdr doc)))
    ((vectorp doc)
     (map nil walker doc))
    (t nil))
  t)

(defmacro walking-document ((var doc) &body body)
  `(labels ((walk (x) (document-walk-further x #'recurse))
	    (recurse (,var) ,@body))
    (recurse ,doc)))

(define-markup (document . rest)
  (setf *document*
	(apply 'make-document rest))
  (funcall *exscribe-document-hook*))

(defun spacedlist (spc x)
  (cond ((null x) x)
	((null (cdr x)) (car x))
	(t (id (car x) spc (brlist (cdr x))))))
(defun spaced* (spc &rest x) (spacedlist spc x))
(defun brlist (x) (spacedlist (br) x))
(defun br* (&rest x) (brlist x))


;(defun sc (&rest x)
;  (apply 'font :face "smallcaps" x))

(define-markup (color (:fg) (:bg) . x)
  (apply 'font (append (when fg (list :color fg))
		       (when bg (list :bgcolor bg))
		       x)))
(define-markup (image &key file &rest x)
  (img :src file :alt (car x)))


(defun tag-attr (y kont x)
  (loop for (attr val . rest) on x by #'cddr
    while (keywordp attr) nconc (list attr val) into bindings
    finally (return (funcall kont y bindings rest))))
(defun make-xml-tag (tag attr content)
  (vector tag attr content :xml))
(defun make-open-tag (tag attr content)
  (declare (ignore content))
  (vector tag attr nil :open))
(defun make-close-tag (tag attr content)
  (declare (ignore attr content))
  (vector tag nil nil :close))
(defun tag (tag &rest args)
  (tag-attr tag #'vector args))
(defun xtag (tag &rest args)
  (tag-attr tag #'make-xml-tag args))
(defun otag (tag &rest args)
  (tag-attr tag #'make-open-tag args))
(defun ctag (tag &rest args)
  (tag-attr tag #'make-close-tag args))

(defun *list* (&rest x) (apply #'*list (apply #'list* x)))
