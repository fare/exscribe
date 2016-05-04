Exscribe
========

[Exscribe](http://www.cliki.net/Exscribe) is
a programmable document generation system
written as an extension to [Common Lisp](http://cliki.net/).
It is designed after Manuel Serrano's [Scribe](http://www-sop.inria.fr/members/Manuel.Serrano/scribe/)
(itself based on [Bigloo Scheme](https://www-sop.inria.fr/indes/fp/Bigloo/)).

See the web page on:
   [http://www.cliki.net/Exscribe](http://www.cliki.net/Exscribe)

I use Exscribe for my [personal website](http://fare.tunes.org/),
but it is also used by the other editor at [Bastiat.org](http://bastiat.org/):
   [https://github.com/fare/bastiat.org](https://github.com/fare/bastiat.org)


Copyright
---------

Exscribe is written and Copyright (c) 2005-2016 by François-René Rideau.

Exscribe is available under the terms of the bugroff license:
	[http://tunes.org/legalese/bugroff.html](http://tunes.org/legalese/bugroff.html)

You may at your leisure use the MIT license instead:
	[https://opensource.org/licenses/MIT](https://opensource.org/licenses/MIT)


Dependencies
------------

All dependencies for Exscribe are in [Quicklisp](http://www.quicklisp.org/)
except [fare-scripts](http://cliki.net/fare-scripts) (as of 2016-05-03).

Exscribe depends on:
[alexandria](http://cliki.net/alexandria),
[cl-launch](http://cliki.net/cl-launch),
[fare-memoization](http://cliki.net/fare-memoization),
[fare-quasiquote](http://cliki.net/fare-quasiquote),
[fare-scripts](http://cliki.net/fare-scripts),
[fare-utils](fare-utils),
[meta](http://cliki.net/meta),
[optima](http://cliki.net/optima), and
[scribble](http://cliki.net/scribble).

If compiled with the PDF backend, then Exscribe also depends on
[CL-Typesetting](http://cliki.net/cl-typesetting) for generating documents.
The PDF backend was written by Denis Mashkevich.
However, the PDF backend isn't fully featured enough to be very useful at this time.
Contributions welcome.

For projects regarding the extension of Exscribe, see
        [http://fare.tunes.org/computing/term-project-proposal.html](http://fare.tunes.org/computing/term-project-proposal.html)


Installing Exscribe
-------------------

You need CL-Launch to be already installed:
	[http://www.cliki.net/CL-Launch](http://www.cliki.net/CL-Launch)

Create and install an exscribe binary by editing the [Makefile](Makefile)
to suit your installation paths and then running `make install`:

    mkdir ~/lib
	make LISPS=sbcl INSTALL_LIB=~/lib install

The Lisp implementations are listed there in faster-to-slower order with
respect to running my Exscribe document set (the largest one in the world!)


Invoking Exscribe
-----------------

You may compile files your former Scribe files or new Exscribe documents with:

	`exscribe -I ${INCLUDE_DIR1} -I ${INCLUDE_DIR2} -v -o foo.html foo.scr`

where `INCLUDE_DIR1`, etc., is where you store your style files.


    Options:
     -h -?  --help                          show some help
     -v     --verbose                       output some information along the way
     -I     --include   /PATH/to/style/     add directory to include path
     -o     --output    destination-file    which file to create
     -H     --html                          select the html backend
     -P     --pdf                           select the PDF backend
     -M     --many      src dst files...    compile files from src to dst
     -D     --debug                         enable the Lisp debugger
            --repl                          provide the user a REPL

If you have trouble running your Scribe documents, tell me and I'll either
add features to Exscribe or tell you how to tweak your document to make it
compatible with both Scribe and Exscribe.


Calling Exscribe from Lisp
--------------------------

If you are debugging Exscribe or integrating it with your code, you may:

    (asdf:load-system :exscribe) (in-package :exscribe)

And then you can compile a file with:

    (add-exscribe-path "/home/fare/fare/www/")
    (exscribe-load-document "/home/fare/fare/www/liberty/microsoft_monopoly.scr")

Or compile it as PDF with:

     (add-exscribe-path "/home/fare/fare/www/")
     (setf *exscribe-mode* 'pdf)
     (with-open-file (*standard-output* "/home/fare/microsoft_monopoly.pdf"
                      :direction :output
                      :if-exists :rename-and-delete :if-does-not-exist :create
                      #+sbcl :element-type #+sbcl :default)
       (exscribe-load-document "/home/fare/fare/www/liberty/microsoft_monopoly.scr"))


Documentation
-------------

Exscribe is unhappily vastly under-documented.

Exscribe was originally modeled after Manuel Serrano's
[Scribe](http://www-sop.inria.fr/members/Manuel.Serrano/scribe/),
and is indeed mostly compatible with it as far as the supported basic primitives go.
You may read Scribe's documentation to get familiar with these basics:
	[http://www-sop.inria.fr/members/Manuel.Serrano/scribe/](http://www-sop.inria.fr/members/Manuel.Serrano/scribe/)

However, bear in mind the following differences and limitations:

 * Exscribe is based on Common Lisp rather than Scheme.
  A simple Scheme compatibility layer is provided in simple case only,
  but you should assume Common Lisp for any advanced programming.

 * My websites used to work equivalently with either Exscribe and Scribe
  based on a compatibility layer. But this compatibility layer has probably bitrotted,
  since Scribe hasn't being used since 2005 or so
  (and indeed it can't be compiled anymore from unmodified sources).

 * Error checking and reporting is minimal.
  I use my Common Lisp's backtrace to locate and identify errors (with option `--debug`).

 * Only those Scribe features that I use have been implemented.

 * The HTML backend is used and well-maintained.

 * There is a proof-of-concept PDF backend, but it's not very featureful.

If you wonder about how some feature works, you may contact me.
If you require more features, you may contact me, or even contract me.
The source is also open for you to inspect and modify.

Also note that Manuel Serrano's Scribe is not directly related to Brian Reid's
original [Scribe](https://en.wikipedia.org/wiki/Scribe_(markup_language)),
although it obviously sports some of the concepts originally developed in it.
When pointed to this fact, Manuel Serrano's called his subsequent rewrite
[Skribe](http://www-sop.inria.fr/mimosa/fp/Skribe/),
but Skribe is somewhat incompatible with Manuel Serrano's Scribe,
and is now just as unmaintained.


Examples
--------

The website [Bastiat.org](http://bastiat.org/) makes extensive use of Exscribe,
and its source code is available on github:

   * [http://github.com/fare/bastiat.org](http://github.com/fare/bastiat.org)

I used to have a small tarball of examples,
that included one essay that stresses most of the features of exscribe,
as well as a `cl-compat.scr` file for Manuel Serrano's Scribe, that shows you
(together with the `cl-compat.scr` file included in exscribe) how to write
programs that display correctly in both Scribe and Exscribe.
If you are interested in more, contact me.


Supported Implementations
-------------------------

I use [SBCL](http://sbcl.org/), and it is the only implementation
that I extensively and regularly test Exscribe with.
But Exscribe should be portable and work on whichever is your favorite CL implementation.
Earlier versions have been tested wit CMUCL, CLISP, Clozure CL, Allegro.
The PDF backend depends on [cl-typesetting](http://cliki.net/cl-typesetting)
that is not completely portable, but works at least on SBCL.


Performance Issues
------------------

If you're interested in improving performance, here are the main issues.

   * Our PDF backend, in addition to being rather poor in terms of features,
     is also quite slow. The whole architecture of it is rather poor.


Projects
--------

   * Improve the cl-typesetting backend: add styles, boxes, real footnotes, etc.

   * Implement pictures and/or embedded cl-typesetting rendering code that
     appears correctly on both PDF and HTML output.

   * Use Matthias Koeppe's [cl-bibtex](http://www.nongnu.org/cl-bibtex/)
     and/or Cyrus Harmon's [smarkup-latex](https://github.com/slyrus/smarkup-latex).

   * Use generic functions as in [Skribe](http://www-sop.inria.fr/mimosa/fp/Skribe/)
     and/or [DefDoc](https://common-lisp.net/project/defdoc/)


Last Words
----------

Share and enjoy!
