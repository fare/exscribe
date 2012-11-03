#+xcvb
(module
 (:depends-on
  ("exscribe/packages"
   "exscribe/specials"
   "exscribe/exscribe-data")))

(in-package :exscribe-txt)

(defun normalize-text (string)
  (with-output-to-string
    (s)
    (loop
        with has-space-p = nil
        for c across string
        as code = (char-code c) do
        (cond
          ((or (<= code 32) (<= 127 code 160))
           (setf has-space-p t))
          (t
           (when has-space-p
             (write-char #\Space s))
           (write-char c s)
           (setf has-space-p nil))))))

(defun extract-text (node &aux strings)
  "Extract text strings from a node."
  (labels
      ((emit (x) (push x strings))
       (space () (emit " "))
       (emit-quote () (emit "\""))
       (walk (x)
         (match x
           ((tag :p _ x) (space) (walk x) (space))
           ((tag :q _ x) (emit-quote) (walk x) (emit-quote))
           ((tag :br _ _) (space))
           ((tag :footnote _ _) nil)
           ((tag _ _ x) (walk x))
           ((typep string) (emit x))
           ((cons x y) (walk x) (walk y))
           (_ nil))))
    (walk node)
    (normalize-text
     (apply #'concatenate 'string (nreverse strings)))))

(defun process-document ()
  (NIY))

(defun init ()
  (setf *exscribe-document-hook* 'process-document))
