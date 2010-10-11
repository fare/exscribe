;;;-*- Lisp -*-
;;; The original Scribe bibliography thingy was a crock.
;;; This emulation layer is worse.
;;; Code shamelessly stolen and maimed from Skribe.
;;; The real thing would be to interface to cl-bibtex

#+xcvb
(module
 (:depends-on
  ("fare-utils"
   "fare-matcher"
   "exscribe/packages"
   "exscribe/macros"
   "exscribe/specials"
   "exscribe/exscribe-data")))

(in-package :exscribe-data)

(defun make-bib-table ()
   (make-hash-table :test 'equalp))
(defun make-bib-fields (fields)
  (let ((h (make-hash-table :test 'eql)))
    (dolist (f fields)
      (ematch f
	((list (and fn (of-type symbol)) fv)
	  (setf (gethash fn h) fv))))
    h))
(defun init-bib-entry (m kind ident fields)
  (setf (gethash :kind m) kind
	(gethash :ident m) ident
	(gethash :fields m) (make-bib-fields fields))
  m)
(defun allocate-bib-entry ()
  (make-hash-table :test 'eql))
(defun make-bib-entry (kind ident fields)
  (init-bib-entry (allocate-bib-entry) kind ident fields))
(defun merge-bib-entry (m kind ident fields)
  (let ((f (gethash :fields m)))
    (if f
	(init-bib-entry m kind ident fields)
	(warn "Discarding duplicate bibliography entry ~A" ident))))
(defun bib-add! (table entry)
  (match entry
    ((list* (and kind (of-type symbol))
	    (and ident (of-type (or string symbol)))
	    fields)
     (let* ((ident (conc-string ident))
	    (old (gethash ident table)))
       (if old
	   (merge-bib-entry old kind entry fields)
	   (setf (gethash ident table)
		 (make-bib-entry kind ident fields)))))
    (* (error "bad bibliography entry ~A" entry))))
(define-markup (bibliography &rest r)
   (dolist (f r)
     (cond
       ((consp f) (bib-add! *bibliography* f))
       (t (error "Illegal bibliography entry ~A" f)))))
(defun print-bibliography (&rest r)
  (when *bibliography-location*
    (error "print-bibliography called twice"))
  (setf *bibliography-options* r
	*bibliography-location* (id))
  *bibliography-location*)
(defun bib-sort/author (x y)
  (flet ((a (m) (gethash 'author (gethash :fields m) "Anonymous")))
    (string< (a x) (a y))))
(defun sort-bibliography (&key all sort)
  (let* ((entries
	  (loop for m being the hash-values of *bibliography*
		when (and (gethash :fields m)
			  (or all (gethash :references m)))
		collect m))
	 (sorted (sort entries (or sort 'bib-sort/author))))
    (loop with count = 0 for m in sorted
	  do (setf (gethash :index m) (incf count)))
    sorted))

(defun get-bib-entry (ident doc)
  (let ((m (gethash ident *bibliography*)))
    (unless m
      (setf m (allocate-bib-entry)
	    (gethash ident *bibliography*) m))
    (push doc (gethash :references m))
    m))

(defun process-bibliography (&key all sort display)
  (when *bibliography-location*
    (let* ((entries (sort-bibliography :all all :sort sort))
	   (displayed
            (funcall display entries)))
      (setf (tag-contents *bibliography-location*) displayed)))
  t)

