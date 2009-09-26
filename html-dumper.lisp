;;;; -*- Lisp -*-
;;;; dumping html from exscribe's vector representation.
;;;; Originally evolved from CTO's version of araneida's html.lisp

#+xcvb (module (:depends-on ("scribble/package" "exscribe/packages")))

#-araneida-compat
(in-package :html-dumper)
#+araneida-compat
(in-package :araneida)

#| This is for dynamic (runtime) printing of HTML expressions.
If you want something that's optimized at compile-time, try WHO or whatelse.

The code was evolved from araneida's original html.lisp by Daniel Barlow.
As compared to the original html.lisp, here are the changes:

* Most importantly, I changed the internal representation from
 ((:A :HREF "URL") "TEXT") to
 #(:A (:HREF "URL") "TEXT")
 ; And put the previous one inside #+araneida-compat

* I reimplemented the HTML function as a call to HTML-STREAM.
 Previously, there were two different functions
 HTML (that produces a string from some lisp-html) and
 HTML-STREAM (that writes some lisp-html to a stream)
 with subtly different semantics.
 To avoid the semantic gap, HTML was always used, which was inefficient.
 Now, HTML is a call to HTML-STREAM, which closes the semantic gap
 in a cleaner and more efficient way.
 HTML-STREAM is also slightly cleaned up as compared to the previous HTML.
 Finally, HTML-STREAM has two new features:
 It also recognizes :keyword things as simple literal tags to be included
 without opening/closing them at the same place.
 it is extensible through the :HTML-STREAM-CONVERTER symbol property
 (distinguished from the previous :HTML-CONVERTER symbol property
 so as to ease transition by helping distinguish between old API and current)
 in the spirit of the thing attempted with the original HTML function,
 but adapted to HTML-STREAM being the main (and cleaner) way to produce html.

* I avoided any runtime call to FORMAT, for enhanced efficiency.

* I split HTML-ESCAPE into HTML-ESCAPE (returns a string) and
 HTML-ESCAPE-STREAM (outputs to a stream).

* The HTML function now has an optional argument that specifies the destination
 of the output, similarly to FORMAT's first argument. The default, for
 backwards compatibility, is to produce a string (argument T). If a stream is
 supplied (with NIL designating the standard output), then output goes there.

* Old symbols to export:
	#:html #:html-stream #:html-escape
* New symbols to export:
	#:html-string #:html-escape-stream
|#

(defun html-reserved-p (x)
  (member x '(#\< #\> #\&)))

(defun html-escape-stream (s html-string &optional test)
  (unless test (setf test 'html-reserved-p))
  (loop for c across html-string do
	(if (funcall test c)
	    (if (eql c #\Newline)
		(write-string "<br />" s)
		(progn (write-string "&#" s) (princ (char-code c) s) (write-char #\; s)))
	    (write-char c s))))

(defun html-escape (html-string &optional test)
  (with-output-to-string (s) (html-escape-stream s html-string test)))

(defun html-escape-tag-stream (stream tag attr content &optional test)
  (declare (ignore tag attr))
  (dolist (element content) (html-escape-stream stream element test)))

(defun dolist-html-stream (stream tag attr content)
  (declare (ignore tag attr))
  (dolist (element content) (html-stream stream element)))

;;; FIXED: Mind that the html-stream-converter feature has a slightly different
;;; semantics than html-converter in araneida's original html.lisp, since the
;;; functions take as an additional argument a stream to which to output,
;;; instead of returning a string (!)
;;; FIXED: mind that the (get 'null :html-stream-converter)
;;; in the original html.lisp was buggy for any considered purpose.
(setf (get 'escape :html-stream-converter) #'html-escape-tag-stream)
(setf (get 'list :html-stream-converter) #'dolist-html-stream)
(setf (get :id :html-stream-converter) #'dolist-html-stream)
(setf (get 'scribble:pp :html-stream-converter) #'dolist-html-stream)
(setf (get 'null :html-stream-converter)
      #'(lambda (stream tag attr content)
	  (princ (list tag attr content) stream)))

(defun html-attr (attr)
  (with-output-to-string (o) (html-attr-stream o attr)))

(defun html-keyword (stream symbol)
  (write-string (string-downcase (symbol-name symbol)) stream))

(defun html-attr-stream (stream attr)
  (loop for (att val . rest) on attr by #'cddr do
    (when val
      rest ; mark the damn variable as used
      (write-char #\space stream)
      (html-keyword stream att)
      (unless (eq val t)
	(write-string "=\"" stream)
	(if (symbolp val)
	    (html-keyword stream val)
	    (html val stream))
	(write-char #\" stream)))))

(defun html-open-tag (stream tag attr &optional close)
  (write-char #\< stream)
  (html-keyword stream tag)
  (html-attr-stream stream attr)
  (when close (format stream "~%/"))
  (write-char #\> stream))

(defun html-close-tag (stream tag)
  (write-string "</" stream)
  (html-keyword stream tag)
  (write-string #.(format nil "~%>") stream))

(defun empty-element-p (tag)
   (member (symbol-name tag)
	   '#.(mapcar 'symbol-name '(img br hr link input))
	   :test 'equal))

; Fare: The only safe way to make html permanently compatible with html-stream
; and that a same extensions work on both cases,
; is to have html call html-stream.
; I think the name html-string is more congruent with html-stream below.
; But to remain mostly compatible, I define html as before.
; Note that I removed the optional argument to html.
(defun html-string (things)
  (with-output-to-string (stream)
    (html-stream stream things)))

(defun html (things &optional out)
  ;;; the optional output specifier argument out
  ;;; behaves a bit like the similar argument in FORMAT
  ;;; it defaults to NIL for backward compatibility with the original html.lisp
  (cond
    ((null out) (html-string things))
    ((eq out t) (html-stream *standard-output* things))
    ((streamp out) (html-stream out things))
    (t (error ""))))

; Fare: Here are the incompatibilities introduced by my patch
; wrt the original function html-stream from araneida CVS.
; * a newline is systematically inserted *before* the > in a closing tag
;  (see function html-close-tag above)
; * handlers like they existed in the original html function are introduced.
;  they are in the alist of the tag symbol, key :html-stream-converter;
;  a handler is a function that takes the output stream, tag, attr, content
;  and does its job. Function values are handlers, too.
; * functions in variable position are called with the output stream.

(defun html-stream-element (stream tag attr content &optional special)
  (let ((handler (cond
		   ((symbolp tag) (get tag :html-stream-converter))
		   ((functionp tag) tag))))
    (cond
      (handler (apply handler stream tag attr content
		      (if special (list special))))
      ((or (empty-element-p tag)
	   (and (null content) (eq special :xml)))
       (html-open-tag stream tag attr t))
      ((eq special :open)
       (html-open-tag stream tag attr))
      ((eq special :close)
       (html-close-tag stream tag))
      (t
       (html-open-tag stream tag attr)
       (dolist (c content)
	 (html-stream stream c))
       (html-close-tag stream tag)))))

(defun html-stream (stream thing)
  "Print supplied argument as HTML."
   (declare (optimize (speed 3))
	    (type stream stream))
   (cond
     ((and (typep thing 'simple-vector) (<= 3 (length thing) 4))
      (apply #'html-stream-element stream
	     (svref thing 0) (svref thing 1) (svref thing 2)
	     (if (= 4 (length thing)) (list (svref thing 3)))))
     #+araneida-compat
     ((and (consp thing) (or (typep (car thing) '(or symbol function))
			     (and (consp (car thing))
				  (typep (caar thing) '(or symbol function)))))
      (let* ((tag (if (consp (car thing)) (caar thing) (car thing)))
	     (attr (if (consp (car thing)) (cdar thing) ()))
	     (content (cdr thing)))
	(html-stream-element stream tag attr content)))
     ((consp thing)
      #+araneida-compat
      (dolist (thing thing) (html-stream stream thing))
      #-araneida-compat #-araneida-compat
      (html-stream stream (car thing))
      (html-stream stream (cdr thing)))
     ((keywordp thing)
      (write-char #\< stream)
      (html-keyword stream thing)
      (write-char #\> stream))
     ;;((null thing)); symbol has it
     ((symbolp thing))
     ((functionp thing)
      (funcall thing stream))
     (t
      (princ thing stream))))
