;;;
#+xcvb
(module
 (:depends-on
  ("packages"
   "specials"
   "exscribe-data"
   "bibliography"
   "exscribe-html"
   "exscribe-txt")))

#|
The xhtml renderer in the cl-typesetting contribs, as well as
the kw-extensions it depends on (part of cl-typesetting)
serve as a basic inspiration for what cl-typesetting code to output.

Also, some code stolen from com.gigamonkeys.markup, then mutilated.
|#

(in-package :exscribe-typeset)


(defparameter *page-width* #.(* 8.5 72))
(defparameter *page-height* #.(* 11 72))
(defparameter *default-font-size* 11)
(defparameter *margins* '(72 36 72 36))
(defparameter *header-height* 36)
(defparameter *footer-height* 36)

(defvar *significant-whitespace* nil)

(defvar *sections* nil)

(defvar *section-nums* nil)

(defconstant +copyright+ (code-char 169)) ;; FIXME: get rid of it
(defparameter *copyright* "No copyright") ;; FIXME: get rid of it

(defparameter *my-leading-ratio* 1.2)

(defvar *typeset-document*)
(defvar *typeset-point*)

(defparameter *delayed-forms* nil)
(defmacro delayed-emit (&body body)
  (with-gensyms (delayed)
    `(progn
       ,@(loop for x in body collect
	      `(let ((,delayed (list #'(lambda ()
					 ,x))))
		 (push ,delayed *delayed-forms*)
		 (emit ,delayed))))))
(defun process-delayed-forms ()
  (dolist (x *delayed-forms*)
    (destructuring-bind (car . cdr) (funcall (car x))
      (rplaca x car)
      (rplacd x cdr))))

(defun emit (x) (push x *typeset-point*))
(defmacro emit-list (&rest list)
  `(progn ,@(loop for x in list collect (list 'emit x))))
(defun collect-point ()
  (nreverse *typeset-point*))
(defmacro with-point (() &body body)
  `(let ((*typeset-point* nil))
     ,@body
     (collect-point)))


(defun load-unicode-fonts ()
  ;; FIXME: use find-scribe-file mechanism to find them?
  (let ((dir #P"/Users/max/src/fare/lisp/exscribe/Fonts/")
	(fontlist (list "Courier"
			#+nil"Courier-Oblique"
			"Courier-Bold"
			#+nil"Courier-BoldOblique"

			"Helvetica"
			#+nil"Helvetica-Oblique"
			"Helvetica-Bold"
			#+nil"Helvetica-BoldOblique"

			"Times-Roman"
			"Times-Italic"
			"Times-Bold"
			"Times-BoldItalic"

			#+nil"Symbol"

			#+nil"ZapfDingbats")))
    (loop for font in fontlist do
	 (pdf:load-ttu-font (merge-pathnames (make-pathname :name font
							    :type "ufm")
					     dir)
			    (merge-pathnames (make-pathname :name font
							    :type "ttf")
					     dir)))))

(defun render ()
  (let* (#+nil(pdf::*name-counter* 20000) ;check the implications
	 (*my-leading-ratio* 1.2)
	 (*delayed-forms* nil)
	 (*sections* nil)
	 (*section-nums* nil)
	 (*typeset-document*
	  (match *document*
		 ((tag :document options list)
		  (with-point ()
		    (when options (apply #'emit-title options))
		    (process list)

		    (emit-list '(vspace 24)
			       '(hrule :dy .1)
			       '(vspace 6))

		    (when *footnotes*
		      (emit (new-section 0 *footnotes-title* :number nil))

		      (let ((*default-font-size* 10))
			(loop for (counter . note) in (nreverse *footnotes*) do
			     (emit-note counter note))))

		    (emit `(mark-ref-point "DocumentEnd"))))

		 (_
		  (error "Top-level document malformed")))))
    (dolist (h (reverse *postprocess-hooks*))
      (funcall h))

    (tt::render-document *typeset-document* :file *standard-output*)))

(defun emit-note (counter note)
  (emit `(with-superscript ()
	   ,(princ-to-string counter)))
  (process note))

(defmacro compute-natural-width (&body body)
  `(typeset::compute-boxes-natural-size
    (typeset::boxes
     (compile-text ()
       ,@body))
    'typeset::dx))

(defun compile-header (content-width header-height)
  (declare (ignore header-height))
  (let* ((second-col-width (compute-natural-width (page-number-contents 999 999)))
         (first-col-width (- content-width second-col-width)))
    (tt:table
      (:col-widths (list first-col-width second-col-width)
                   :border 0
                   :padding 0
                   :cell-padding 0)
      (row ()
        (cell ()
          (paragraph
              (:h-align :left :font "Times-Italic" :font-size 10)
            (put-string
             (get-contextual-variable 'chapter-name))))
        (cell ()
          (page-number-contents
           pdf:*page-number*
           (find-ref-point-page-number :the-end)))))))



(defun page-number-contents (page max)
  (paragraph
      (:h-align :right :font "Times-Italic" :font-size 10)
    (put-string (format nil "Page ~d of ~d" page max))))


(defun compile-footer (content-width footer-height)
  (let ((cols 2))
    (tt:table
      (:col-widths (loop repeat cols collect (/ content-width cols)) :border 0
                   :padding 0 :cell-padding 0)
      (row (:height footer-height)
        (cell (:v-align :bottom)
          (paragraph
              (:h-align :left :font "Times-Italic" :font-size 10)
            (put-string
	     (format nil "Copyright ~c ~a" +copyright+ *copyright*))))

        (cell (:v-align :bottom)
          (paragraph
              (:h-align :right :font "Times-Italic" :font-size 10)
            (put-string (date-string))))))))



(defun date-string (&optional (utc (get-universal-time)))
  (multiple-value-bind (sec min hour date month year day daylight-p zone)
      (decode-universal-time utc)
    (declare (ignorable sec min hour date month year day daylight-p zone))
    (format nil "~d ~[~;~
                      January~;~
                      February~;~
                      March~;~
                      April~;~
                      May~;~
                      June~;~
                      July~;~
                      August~;~
                      September~;~
                      October~;~
                      November~;~
                      December~;~] ~4d, ~d:~2,'0d ~:[am~;pm~]" date month year (1+ (mod (1- hour) 12)) min (>= hour 12))))




(defparameter *font-variants*
  (list
   #("Courier"
     "Courier-Oblique"
     "Courier-Bold"
     "Courier-BoldOblique")

   #("Helvetica"
     "Helvetica-Oblique"
     "Helvetica-Bold"
     "Helvetica-BoldOblique")

   #("Times-Roman"
     "Times-Italic"
     "Times-Bold"
     "Times-BoldItalic")

   #("Symbol"
     "Symbol"
     "Symbol"
     "Symbol")

   #("ZapfDingbats"
     "ZapfDingbats"
     "ZapfDingbats"
     "ZapfDingbats")))

(defun transform-font (font &key add remove toggle)
  "The rationale behind this function is that for example when rendering emphasis, one needs to italicize text if it is typeset in roman, and deitalicize if it is typeset in italic."
  (flet ((mask (features)
	   (cond
	     ((equal features '(:bold)) #b10)
	     ((equal features '(:italic)) #b01)
	     ((equal features '(:bold :italic)) #b11)
	     ((equal features '(:italic :bold)) #b11))))
    (loop for f in *font-variants* do
	 (let ((pos (position font f :test #'string-equal)))
	   (when pos
	     (let ((font-features pos))
	       ; add = logior
	       ; 0 0 0
	       ; 0 1 1
	       ; 1 0 1
	       ; 1 1 1
	       (when add
		 (setq font-features
		       (logior font-features
			       (mask (ensure-list add)))))
	       ; remove = logandc2
	       ; 0 0 0
	       ; 0 1 0
	       ; 1 0 1
	       ; 1 1 0
	       (when remove
		 (setq font-features
		       (logandc2 font-features
			       (mask (ensure-list remove)))))
	       ; toggle = logxor
	       ; 0 0 0
	       ; 0 1 1
	       ; 1 0 1
	       ; 1 1 0
	       (when toggle
		 (setq font-features
		       (logxor font-features
			       (mask (ensure-list toggle)))))
	       (return (svref f font-features))))))))


(defun font-size (type)	;;FIXME: adapt (stolen from gigamonkeys markup)
  "Determine the font size for a given paragraph type, based on the current default."
  (case type
    (:blockquote (1- *default-font-size*))
    (:example (- *default-font-size* 4))
    (t *default-font-size*)))


(defun pdfize-string (s)
  ;;TODO: collapse spaces, escape special characters?
  (exscribe-txt::normalize-text s))

(defun emit-title (&key title author)
  (emit `(set-contextual-variable :title
				  ,(or *document-title*
				       (exscribe-txt::extract-text title))))
  (when title
    (emit `(paragraph
	       (:h-align :center :font "Times-Roman" :font-size 28)
	     ,@(with-point ()
		 (process title)))))
  (emit '(vspace 24))
  (process author))

(defun process-list (x)
  (with-point ()
    (process x)))

(defun process-section (level options list)
  (destructuring-bind (&key (number t) heading title (toc t) refid) options
    (declare (ignore toc))
    (progn ;;-- unless heading
      (setq heading (if (stringp title)
                        title
                        (exscribe-txt:extract-text title))))
    (emit (new-section level
		       heading
		       :title (with-point ()
				  (process title))
		       :number number
		       :refid refid))
    (process list)))

(defun process (x)
  (ematch x
    ('nil
     nil)

    ;; FIXME: the following is an ugly hack! understand why these list forms are not evaluated in the proper place as they should be
    ((cons 'list rest)
     (process (apply #'list rest)))

    ((cons a b)
     (process a)
     (process b))

    ((type character)
     (process (string x)))

    ((type string)
     (emit (if *significant-whitespace*
	       `(verbatim ,x)
	       `(put-string ,x))))

    ;; having full blown closures generated in order to print a single character  which can just be included as-is seems like a gratuitious complexity to me. or is there a deeper reason, maybe for other backends?
    ((type function)
     (emit `(verbatim ,(with-output-to-string (s) (funcall x s)))))

    ((tag :p options list)
     (destructuring-bind (&key (align :left)) options
       (emit `(let ((tt::*leading-ratio* ,*my-leading-ratio*))
		(paragraph
		    (:h-align ,align
			      :font "Times-Roman"
			      :font-size ,(font-size :p)
			      :first-line-indent 16)
		  ,@(process-list list)
		  :hfill)
		(vspace 12)))))

    ((tag :i _ list)
     (emit `(with-style (:font (transform-font (pdf:name typeset::*font*)
					       :add :italic))
	      ,@(process-list list))))

    ((tag :b _ list)
     (emit `(with-style (:font (transform-font (pdf:name typeset::*font*)
					       :add :bold))
	      ,@(process-list list))))

    ((tag :em _ list)
     (emit `(with-style (:font (transform-font (pdf:name typeset::*font*)
					       :toggle :italic))
	      ,@(process-list list))))

    ((tag :u _ list)
     (emit `(with-style (:font (transform-font (pdf:name typeset::*font*)
					       :add :italic))
	      ,@(process-list list))))

    ((tag :br _ _)
     (let ((*significant-whitespace* t))
       (process (string #\Newline))))

    ((tag :author options list)
     (destructuring-bind (&key name url) options
       (when name
	 (emit `(paragraph
		    (:h-align :center :font "Times-Italic" :font-size 24)
		  ,@(process-list name)
		  ,@(with-point ()
		      (when url
			(emit '(vspace 12))
			(emit `(with-style (:font "Helvetica" :font-size 12)
				 ,@(process-list url)))))))))
     (process list)
     (emit '(vspace 24)))

    ((tag :ref (list :url href) body)
     ;; FIXME: oudeis: make links clickable (and send patch to xhtml-renderer maintainer too)
     ;; FIXME: oudeis: give user an option whether to print the url beside link name
     (emit `(with-style ()
	      ,@(process-list body)
	      " ("
	      (with-style (:color :blue)
		(put-string ,href))
	      ")")))

    ((tag :tt () body)
     (emit `(with-style (:font "Helvetica" :font-size 12)
              ,@(process body))))

    ((tag :ref (list :bib entry) _)
     (emit `(format-string "[~a]" ,entry)))

    ((tag :ref (list :section section) _)
     (emit `(format-string "~s" ,section)))

    ((tag :footnote (list :note note) _)
     ;; FIXME: see what list variable may contain
     (let ((n (incf *footnote-counter*)))
       (emit `(with-superscript ()
		,(princ-to-string n)))
       (push (cons n note) *footnotes*)))

    ((tag :font _ list)
     (process list))

    ((tag :bibliography _ list)
     (destructuring-bind ((header . entries)) list
       (when header
	 (process header))
       (dolist (e entries)
	 (emit `(paragraph (:font "Times-Roman")
		  ,@(with-point ()
		      (let* ((fields (gethash :fields e)))
			(emit `(format-string "[~a] " ,(gethash :ident e)))
			(process (tag :i nil nil (gethash 'title fields)))
			(emit `(format-string "~@[, ~a~]~@[, ~a~]"
					      ,(gethash 'author fields)
					      ,(gethash 'year fields)))
			(process (tag :ref :url (gethash 'url fields) nil)))))))))

    ((tag :section options list)
     (process-section 0 options list))

    ((tag :subsection options list)
     (process-section 1 options list))

    ((tag :subsubsection options list)
     (process-section 2 options list))

    ((vector :table-of-contents _ _)
     (delayed-emit `(progn ,@(make-toc)))
     #+nil
     (dolist (e (nreverse *toc*))
       (destructuring-bind (&key num-str kind title ref-point) e
	 (when (getf options kind)
	   (emit `(paragraph (:h-align :left-but-last
				       :left-margin 20
				       :right-margin 20
				       :top-margin 0
				       :bottom-margin 0
				       :font-size 14)

		    ,@(with-point ()
				  (when num-str
				    (process num-str))
				  (process title))
		    (typeset::dotted-hfill)
		    (typeset::put-ref-point-page-number ,ref-point)))))))

    ((tag :id nil list)
     (process list))

    ((tag :hr _ _)
     #+nil(emit '(hrule :dy .1)))

    ((tag :a _ _))

    ((tag :blockquote _ list)
     (emit `(paragraph
		(:h-align :left
		 :font "Times-Roman"
		 :font-size ,(font-size :blockquote)
		 :left-margin 18)
	      (vspace 12)
	      ,@(process-list list))))


    #+nil(*
     `(format-string ,(exscribe-txt:extract-text x))))

  (values))

(defun display-bibliography (entries)
  (tag :bibliography nil nil
       (push *bibliography-header*
	     entries)))

(defun process-document ()
  #+nil(load-unicode-fonts)

  (apply 'process-bibliography
	 :display #'display-bibliography
	 *bibliography-options*)

  (render))

(defun init ()
  #+nil (setf scribble:*scribble-preprocess* #'pdfize-string)
  (setf *exscribe-document-hook* 'process-document)
  (setf *footnotes-title* "Notes")
  (push #'process-delayed-forms *postprocess-hooks*)
  (setf pdf:*compress-streams* t)
  nil)


;; make-toc and new-section(nee chapter-markup)
;; are originally from kw-extensions and use the framework set up there
;; FIXME: don't rely on kw-extensions framework
(defun make-toc-entry (chp)
  ;; format table of contents entry
  (let* ((ref (first chp))
	 (cnum (cdr ref))
	 (depth (length cnum))
	 (title (second chp)))
    (when (<= depth tt::*toc-depth*)
      `(paragraph
	(:h-align :left-but-last
	 :left-margin ,(case depth (1 0) (2 10) (t 20))
	 :top-margin ,(case depth (1 3) (t 0))
	 :bottom-margin ,(case depth (1 2) (t 0))
	 :font-size ,(case depth (1 12) (2 10) (t 9)))
	,@(let ((secnum-string (secnum-string cnum)))
	       (unless (equal secnum-string "")
		 (list (list 'put-string secnum-string)
		       (list 'put-string " "))))
	(put-string ,title)
	(tt::dotted-hfill)
	(with-style (:font-size 10)
	  (tt::put-ref-point-page-number ',ref))))))

(defun make-toc ()
  ;; FIXME: Indentation and font selection currently hardcoded
  (mapcar #'make-toc-entry (reverse *sections*)))

;; FIXME: move it somewhere
#+nil(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun secnum-string (nums)
  (let ((nums (loop for x in (reverse nums)
		 while (numberp x)
		 collect x into numeric
		 finally (return (nreverse numeric)))))
   (format nil "~{~S~^.~}" nums)))

(defun new-section-ref (level text &optional (number t) refid)
  "Insert current section information into *sections*, automatically
incrementing the elements of *section-nums*.
Returns an ID suitable for a reference.
  If number is nil, refid used for that part of the reference. If refid is not provided, text is used as refid."
  ;; FIXME: the implementation is quite ugly
(flet ((find-prev-current-level-numeric (higher)
	 (loop for ((nil . nums) nil) in *sections*
	       for hi = (subseq nums 0 level)
	       for cur = (nth level nums)
	       if (not (equal hi higher)) do (return 0)
 	       if (integerp cur) do (return cur)
	      finally (return 0))))
 (let* ((higher (subseq *section-nums* 0 level))
	(current (nth level *section-nums*))
	(refid (if number
		   (if current
		       (if (integerp current)
			   (1+ current)
			   (1+ (find-prev-current-level-numeric higher)))
		       1)
		   (or refid text))))
   (setq *section-nums* (append higher (list refid)))))
  (let ((cs (cons :section *section-nums*)))
    (push (list cs text) *sections*)
    cs))

(defun new-section (level outline-heading &key (number t) title refid)
  (let* ((ref-id (new-section-ref level outline-heading number refid))
	 (cprefix (if number
		      (concatenate 'string (secnum-string (cdr ref-id)) " ")
		      ""))
	 (numbered-heading (concatenate 'string cprefix outline-heading)))
    `(pdf:with-outline-level (,numbered-heading
			      (pdf::register-named-reference
			       (vector (tt::find-ref-point-page-content ',ref-id) "/Fit")
			       (pdf::gen-name "R")))
       ,(if (eql level 0) :fresh-page "")
       ,(if (eql level 0) `(set-contextual-variable :section ,outline-heading) "")
       (paragraph ,(nth level tt::*chapter-styles*)
	 (mark-ref-point ',ref-id :data ,outline-heading :page-content t)
	 (put-string ,cprefix)
	 ,@(if (null title)
	       `((put-string ,outline-heading))
	       title)))))

