;;;; -*- lisp -*-
; A thin emulation layer to run simple Scheme code within CL.
#+xcvb
(module
 (:depends-on
  ("/fare-utils"
   "/exscribe/packages"
   "/exscribe/specials")))

#| "
This code aims to provide a very thin layer of Scheme compatibility in CL.
It is meant to leverage simple code written with Scheme syntax;
it does NOT try to actually implement deep Scheme semantics.
If you want a real Scheme-in-CL implementation, there are other places to look.
The goal is for me to more easily migrate my former Scribe documents,
and otherwise allow to reuse and share code between Scheme and CL
while minizing the amount of things to change on either side.

Here are some particular implementation choices and limitations:

* We don't do any extraneous error checking.
 If the Scheme program wasn't debugged, we don't care to support it.
 This allows for a very shallow implementation of Scheme primitives
 with less strict Common Lisp primitives that behave well
 in cases defined as erroneous or left undefined by the Scheme standard.

* We don't provide a unified namespace for functions and variables,
 but reuse what the Common Lisp evaluator provides.

  = Be sure to use (funcall ...) and (function ...) as appropriate;
   The macros funcall*, apply* and map* are provided to wrap (function ...)
   around their first argument. You can't use map, which means something
  different in Scheme and CL; use mapcar or map* instead.

  = be sure to not try to bind T or NIL or any other constant as a variable.
   you may have to rename a few variables in your Scheme program.

* We don't support distinction of NIL () and #f or inexact numbers.

* We won't provide any kind of support for first-class continuations,
 They could be added on top of Screamer, I suppose.

* I implement features on a need basis. I won't proactively implement
 any features from RnRS or SRFIs. But it's easy enough to port those features
 that you need, and I'll accept patches.

* We only support the symbols declared in package scheme-makeup,
 plus the following from the CL package:
	cons car cdr c([ad]+)r list
	let let*

* We recommend that you use the following CL features that are in Scribe
 if not in RnRS:
   labels => together with let, a good replacement for letrec and named let.

* Anything that clashes with CL semantics is unsupported; sometimes we provide
 some equivalent functionality under a different name:
   call-with-current-continuation => unsupported
   letrec => unsupported, because it doesn't distinguish namespaces. Use labels.
   let => named let is unsupported. Use labels.
   map => Use map* or mapcar.
   for-each => use for-each* or for-each


Reference for Scheme and CL semantics:
	http://swiss.csail.mit.edu/~jaffer/r5rs_toc.html
	http://lisp.org/HyperSpec/

If we really wanted to implement lisp1 semantics for let, letrec, etc.,
we could example a lisp1 binding for x to value into
  (macrolet ((,x (&rest ,args) `(funcall ,,x ,@,args)))
    (let ((,x ,value))
      ,@body))

|#

(in-package :scheme-compat)

(defparameter *scheme-true* t)
(defparameter *scheme-false* nil)
(defparameter else t)

;;; define
(defun process-scheme-formals (formals)
  (cond
    ((null formals) nil)
    ((symbolp formals) `(&rest ,formals))
    ((consp formals)
     (cons (car formals) (process-scheme-formals (cdr formals))))
    (t (error "bad scheme formals"))))
(defun make-function-definition (name formals body &optional (defun 'defun))
  `(,defun ,name ,(process-scheme-formals formals) ,@body))
(defmacro define (name &rest rest)
  (if (symbolp name)
      `(defparameter ,name ,@rest)
      (make-function-definition (first name) (rest name) rest)))

;;; forms implementable as simple aliases
(defmacro defalias (simpleargs sch cl)
  `(defun ,sch ,simpleargs (,cl ,@simpleargs)))
(defmacro defaliases (args &rest r)
  `(progn ,@(loop for (sch . cl) in (plist->alist r)
		  collect `(defalias ,args ,sch ,cl))))
(defmacro defxalias (sch cl)
  `(defun ,sch (&rest r) (apply ',cl r)))
(defmacro defxaliases (&rest r)
  `(progn ,@(loop for (sch . cl) in (plist->alist r)
		  collect `(defxalias ,sch ,cl))))
(defmacro defmalias (sch cl)
  `(defmacro ,sch (&rest r) `(,',cl ,@r)))
(defmacro defmaliases (&rest r)
  `(progn ,@(loop for (sch . cl) in (plist->alist r)
		  collect `(defmalias ,sch ,cl))))
(defmacro defvalias (sch cl)
  `(define-symbol-macro ,sch ,cl))
(defmacro defvaliases (&rest r)
  `(progn ,@(loop for (sch . cl) in (plist->alist r)
		  collect `(defvalias ,sch ,cl))))

(defaliases (x)
    string? stringp
    symbol? symbolp
    number? numberp
    integer? integerp
    pair? consp
    list? listp
    null? null
    zero? zerop
    string-length length
    symbol->string symbol-name
    number->string princ-to-string)
(defaliases (x y)
    eq? eq
    eqv? eql
    equal? equal
    remainder rem
    modulo mod)
(defxaliases
    display princ
    substring subseq
    ;map mapcar
    for-each mapc)
(defvaliases
    current-input-port *standard-input*
    current-output-port *standard-output*)
(defmaliases
    begin progn)

(defmacro set! (var val)
  `(progn (setq ,var ,val) nil))

(defmacro define-macro (name &rest rest)
  (make-function-definition (first name) (rest name) rest 'defmacro))


;;; slightly different forms
(defun string-append (&rest r)
  (apply 'concatenate 'string r))
(defun string->symbol (x)
  (intern x :exscribe-user))

;;; Scribe-only
(defun keyword->string (x)
  (if (keywordp x) (conc-string #\: x) (error "~A isn't a keyword" x)))
(defaliases (x)
    file-exists? probe-file
    keyword? keywordp)
(defvaliases
    *scribe-format* exscribe::*exscribe-mode*
    *scribe-header* exscribe::*header*
    *scribe-footer* exscribe::*footer*
    *scribe-background* exscribe::*background*
    *scribe-foreground* exscribe::*foreground*
    *scribe-tbackground* exscribe::*title-background*
    *scribe-sbackground* exscribe::*section-title-background*)

;;; Syntax
(defun set-scheme-macro-characters ()
  (set-dispatch-macro-character #\# #\t
      #'(lambda (stream subchar arg)
	  (declare (ignore stream subchar arg))
	  *scheme-true*))
  (set-dispatch-macro-character #\# #\f
      #'(lambda (stream subchar arg)
	  (declare (ignore stream subchar arg))
	  *scheme-false*))
  (set-dispatch-macro-character #\# #\!
      #'(lambda (stream subchar arg)
	  (declare (ignore subchar arg))
	  (let ((x (read stream)))
	    (case x
	      ((key) '&key)
	      ((rest) '&rest)
	      (t (error "unrecognized form #!~A" x))))))
  t)
