#+xcvb
(module
 (:depends-on ("scribble/package")))

(in-package :uiop)

(define-package :scheme-makeup
  (:documentation "a poor emulation of Scheme in CL.
Meant to leverage simple code with Scheme syntax,
not to actually implement deep Scheme semantics.")
  (:use :common-lisp)
  (:shadow #:map)
  (:export
   ;;; Standard scheme stuff
   #:define #:set! #:begin #:else
   #:substring #:string-append #:string-length
   #:symbol->string #:string->symbol #:number->string
   #:for-each
   #:display
   #:string? #:symbol? #:number? #:pair? #:null? #:integer? #:zero? #:list?
   #:eq? #:eqv? #:equal? #:remainder #:modulo
   ;;; Standard scheme, but would requires shadowing
   ; #:map
   ;;; Stuff that we don't support: #:let #:call/cc
   ;;; bigloo-specific stuff
   #:file-exists?
   #:keyword? #:keyword->string #:define-macro
   ;;; Scribe stuff
   #:*scribe-background* #:*scribe-foreground* #:*scribe-tbackground*
   #:*scribe-format* #:*scribe-header* #:*scribe-footer*))

(define-package :scheme-compat
  (:documentation "innards of the Scheme in CL emulation")
  (:mix :uiop :fare-utils :alexandria)
  (:use :scheme-makeup :fare-quasiquote :trivia :common-lisp)
  ;;(:shadowing-import-from :scheme-makeup :map)
  (:export
   #:set-scheme-macro-characters))

(define-package :exscribe
  (:documentation "core infrastructure for exscribe")
  (:mix :uiop :fare-utils :alexandria)
  (:reexport :uiop :fare-utils :alexandria)
  (:use :scribble :fare-quasiquote :trivia :common-lisp :quri)
  (:export
   #:init-exscribe #:ensure-exscribe
   #:add-exscribe-path
   ;;; scribble
   #:configure-scribble-for-exscribe
   #:klist #:*list
   ;;; macros
   #:xxtime
   ;;; globals
   #:*exscribe-version*
   #:*exscribe-path* #:*exscribe-mode* #:*exscribe-verbose*
   #:*exscribe-document-hook* #:*postprocess-hooks*
   #:*bibliography* #:*bibliography-location* #:*bibliography-options*
   #:*bibliography-header*
   #:*document* #:*document-title* #:*header* #:*footer*
   #:*footnotes* #:*footnote-counter* #:*footnotes-header*
   #:footnote-style #:*footnote-style* ;; Is this the correct package for that???
   #:*section-counter* #:*subsection-counter* #:*subsubsection-counter*
   #:*generate-label-counter*
   #:*section-name* #:*subsection-name* #:*subsubsection-name*
   #:*sections-by-label*
   #:*toc*
   #:*background* #:*foreground* #:*title-background* #:*section-title-background*
   ;;; the "markup" call syntax
   #:lambda-markup #:define-markup #:define-markup-macro
   ;;; Scribe stuff
   #:html #:pdf #:txt #:info
   #:document #:style))

(define-package :exscribe-data
  (:documentation "internal data representation for exscribe")
  (:use :exscribe :fare-quasiquote :trivia :common-lisp :quri)
  (:export
   #:tag-attr #:tag #:xtag #:otag #:ctag
   #:make-xml-tag #:make-open-tag #:make-close-tag
   #:*list #:*list*
   #:id
   #:replace-tag! #:replace-cons! #:tag-contents #:copy-tag
   #:make-bib-table #:bib-sort/author #:get-bib-entry #:sort-bibliography
   #:make-document #:author #:ref
   #:table-of-contents #:footnote #:section #:subsection #:subsubsection
   #:bibliography
   #:book #:misc #:article
   #:title #:author #:year #:url #:journal #:month
   #:emph #:bold #:it #:underline #:center #:hr #:hrule #:footnote #:tt #:pre #:a
   #:blockquote #:sup #:sub
   #:p #:font #:table #:tr #:td #:th #:itemize #:item #:br #:span #:div
   #:h1 #:h2 #:h3 #:h4 #:h5 #:h6
   #:small #:enumerate #:sc #:color #:code #:print-bibliography #:process-bibliography
   #:img #:image
   #:html #:head #:body
   #:address #:form #:input
   #:brlist #:br* #:spacedlist #:spaced*
   #:*trusted-hosts* #:url-trusted-p
   #:walking-document #:walk #:recurse
   #:*make-title-hook* #:*make-author-hook*
   ))

(define-package :html-dumper
  (:documentation "HTML dumping functions")
  (:use :common-lisp)
  (:export
   #:html #:html-stream #:html-escape
   #:html-string #:html-escape-stream
   #:html-tag-attr #:html-close-tag))

(define-package :exscribe-html
  (:documentation "HTML backend for exscribe")
  (:shadowing-import-from :exscribe-data #:html)
  (:use :exscribe-data :exscribe :html-dumper
        :fare-quasiquote :trivia :common-lisp))

(define-package :exscribe-txt
  (:documentation "Text backend for exscribe")
  (:use :exscribe-data :exscribe :fare-quasiquote :trivia :common-lisp)
  (:export #:extract-text #:normalize-text))

(define-package :exscribe-user
  ;(:shadowing-import-from :scheme-makeup :map)
  (:use :exscribe-html :exscribe-data :exscribe :scheme-makeup
        :fare-quasiquote :trivia :common-lisp :fare-scripts/rescript :quri)
  (:export))
