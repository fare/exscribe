;;;;; -*- lisp -*-

#+xcvb
(module
 (:depends-on
  ("/fare-utils" "exscribe/packages")))

(in-package :exscribe)

;;;; Macros that define the markup funky calling conventions of Scribe.
;;; We make it even funkier, supporting a mix of Scribe and CL conventions.
;;; We support this: (req1 req2 &optional opt1 opt2 &key key1 key2 &rest rest)
;;; Instead of &key key1 key2, we also support a list of Scribe-style keywords:
;;; (:kw default-value) and instead of &rest rest we also accept . rest.
;;; The scribe source refers to DSSSL, but DSSSL keywords are like kw: not :kw
;;; and DSSSL seems dead, anyway (the standards site is down and no one cares).

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *lambda-markup-list-keywords*
  '(&optional &rest &aux
    &key &allow-other-keys
    &whole
    &environment &body))

(defparameter *argument-syntax*
  '(&optional &key &rest))

(defun symbol->keyword (x)
  (intern (symbol-name x) :keyword))
(defun keyword->symbol (x &optional (package *package*))
  (if (keywordp x)
      (intern (symbol-name x) package)
      (error "~A isn't a keyword" x)))

#| ; a better(?) version is in fare-utils/base/parse-cl-syntax.lisp
(defun identifierp (x)
  (and (symbolp x)
       (not #+sbcl (sb-c::constantp x)
	    #-sbcl (or (keywordp x) (typep x 'boolean)))))
|#

(defun decode-optional-spec (spec)
  (labels ((err () (error "invalid optional argument specification ~W" spec))
	   (decode0 ()
	     (cond ((symbolp spec) (decode1 spec nil))
		   ((consp spec) (decode1 (car spec) (cdr spec)))
		   (t (err))))
	   (decode1 (v r)
	     (unless (identifierp v) (err))
	     (cond ((null r) (decode3 v nil nil))
		   ((and (consp r) (null (cdr r))) (decode2 v (car r) (cdr r)))
		   (t (err))))
	   (decode2 (v d r)
	     (cond ((null r) (decode3 v d nil))
		   ((and (consp r) (identifierp (car r)) (null (cdr r)))
		    (decode3 v d (car r)))
		   (t (err))))
	   (decode3 (v d x)
	     (values v d x)))
        (decode0)))

(defun optional-spec-p (spec)
  (ignore-errors (decode-optional-spec spec) t))

(defun decode-key-spec (spec)
  (labels ((err () (error "invalid keyword argument specification ~W" spec))
	   (decode0 ()
	     (cond ((symbolp spec) (decode1 spec nil))
		   ((consp spec) (decode1 (car spec) (cdr spec)))
		   (t (err))))
	   (decode1 (v r)
	     (cond ((keywordp v) (decode2 v (keyword->symbol v) r))
		   ((symbolp v) (decode2 (symbol->keyword v) v r))
		   ((and (consp v) (consp (cdr v)) (null (cddr v)))
		    (decode2 (car v) (cadr v) r))
		   (t (err))))
	   (decode2 (k v r)
	     (unless (and (symbolp k) (identifierp v)) (err))
	     (cond ((consp r) (decode3 k v (car r) (cdr r)))
		   ((null r) (decode3 k v nil nil))
		   (t (err))))
	   (decode3 (k v d r)
	     (cond ((and (consp r) (null (cdr r))) (decode4 k v d (car r)))
		   ((null r) (decode4 k v d nil))
		   (t (err))))
	   (decode4 (k v d x)
	     (unless (symbolp x) (err))
	     (values k v d x)))
    (decode0)))

(defun key-spec-p (spec)
  (ignore-errors (decode-key-spec spec) t))

(defun processing-keys (args keys rest body)
  (loop with vars = nil with cases = nil with inits = nil
	for key in keys do
	(multiple-value-bind (k v d x) (decode-key-spec key)
	  (push v vars)
	  (unless x (setf x (gensym)))
	  (push x vars)
	  (push `((,k)
		  (unless ,x (setf ,v (cadr ,args) ,x t))
		  (setf ,args (cddr ,args)))
		cases)
	  (push `(unless ,x (setf ,v ,d)) inits))
	finally
	(return `(let ,(append (reverse vars) rest)
		  (loop while (and (consp ,args) (consp (cdr ,args)))
		    do (case (car ,args) ,@(reverse cases) (t (return))))
		  ,(if (car rest) `(setf ,(car rest) ,args)
		       `(when ,args
			 (error "unexpected extra arguments ~W" ,args)))
		  ,@(reverse inits)
		  (progn ,@body)))))

(defun scribe-style-keyword-arg-p (arg)
  (and (consp arg) (keywordp (car arg))
       (or (null (cdr arg)) (and (consp (cdr arg)) (null (cddr arg))))))

(defun do-define-markup (prefix formals body)
  (labels ((getargs (&optional checker)
	     (loop while (and (consp formals)
			      (not (member (car formals) *argument-syntax*))
			      (if checker (funcall checker (car formals)) t))
	       collect (pop formals)))
	   (getargs-for (keyword &optional checker)
	     (when (and (consp formals) (eq keyword (car formals)))
	       (pop formals)
	       (getargs checker)))
	   (get-1-arg-for (keyword &optional checker)
	     (when (and (consp formals) (eq keyword (car formals)))
	       (unless (and (consp (cdr formals))
			    (not (member (cadr formals) *argument-syntax*)))
		 (error "missing ~A argument" keyword))
	       (let ((arg (cadr formals)))
		 (when checker
		   (unless (funcall checker arg)
		     (error "bad ~A argument: ~W" keyword arg)))
		 (setf formals (cddr formals))
		 (list arg)))))
    (let* ((required (getargs 'identifierp))
	   (optional (getargs-for '&optional 'optional-spec-p))
	   (keys (or (getargs-for '&key 'key-spec-p)
		     (when (and (consp formals)
				(scribe-style-keyword-arg-p (car formals)))
		       (getargs 'scribe-style-keyword-arg-p))))
	   (rest (or (get-1-arg-for '&rest 'identifierp) ;; note: in CL, &rest comes *before* &key
		     (when (identifierp formals)
		       (prog1 (list formals) (setf formals nil)))))
	   #|(aux (or (getargs-for '&aux 'optional-spec-p)
		     (when (identifierp formals)
		       (prog1 (list formals) (setf formals nil)))))|#)
      (when formals
	(error "unexpected formal parameters: ~W" formals))
      (let* ((primitive-rest (if keys (gensym) (car rest)))
	     (primitive-formals
	      (append required
		      (when optional (cons '&optional optional))
		      (when primitive-rest (list '&rest primitive-rest))))
	     (cooked-body
	      (if keys
		  (list (processing-keys primitive-rest keys rest body))
		  body)))
      `(,@prefix ,primitive-formals ,@cooked-body)))))

(defmacro lambda-markup (formals &body body)
  (do-define-markup '(lambda) formals body))

(defmacro define-markup (name-formals &body body)
  (do-define-markup `(defun ,(car name-formals)) (cdr name-formals) body))

(defmacro define-markup-macro (name-formals &body body)
  (do-define-markup `(defmacro ,(car name-formals)) (cdr name-formals) body))

(defvar *xtime* #-debug nil #+debug t)

(defmacro xxtime (msg &body body)
  (with-gensyms (thunk)
    `(flet ((,thunk () ,@body))
       (if *xtime* (xtime ,msg (,thunk)) (,thunk)))))
)
