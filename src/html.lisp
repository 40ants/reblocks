(uiop:define-package #:weblocks/html
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*)
  (:import-from #:40ants-doc
                #:defsection)
  (:export #:with-html
           #:*pretty-html*
           #:*lang*
           #:with-html-string
           #:*stream*))
(in-package weblocks/html)


(defsection @html (:title "HTML Rendering"
                   :ignore-words ("HTML"
                                  "DIV"
                                  "CSS"))
  "Weblocks provides a thin wrapper for

   Use WEBLOCKS/HTML:WITH-HTML macro to render HTML.
   You can use any other templating engine, just ensure
   it writes output to WEBLOCKS/HTML:*STREAM*"

  (with-html macro)
  (with-html-string macro)
  (*stream* variable)
  (*lang* variable)
  (*pretty-html* variable)) 


(defvar *stream*
  (make-synonym-stream '*standard-output*)
  "Weblocks will write all output into this stream.

   This stream is created for each request and available to
   code executed within a request as a special
   variable. All html should be rendered to this stream,
   but don't worry, if you are using `with-html` or
   `with-html-string` macroses, they handle this for you.")


(defvar *lang* "en"
  "Language to add into the root html element.")

(defvar *pretty-html* t
  ;; pretty html, but allow to disable in tests.
  "We want an HTML nice to read by default.")

(defmacro with-html (&body body)
  "Renders body using [Spinneret](https://github.com/ruricolist/spinneret) HTML generator.

   "
  `(let ((spinneret:*html-lang* *lang*)
         (spinneret:*html* *stream*)
         ;; We want to an HTML which is nice to read, by default
         (*print-pretty* *pretty-html*))
     (spinneret:with-html
       ,@body)))


(defmacro with-html-string (&body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*stream*)
     (with-html
       ,@body)))


(defun get-rendered-chunk ()
  "Returns a string containing HTML redered since last call to
   with-html, with-html-string or get-rendered-chunk.

   Call to this function clears internal stream, used for rendering!"
  (get-output-stream-string *stream*))
