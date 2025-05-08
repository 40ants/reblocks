(uiop:define-package #:reblocks/html
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*)
  (:export #:with-html
           #:*pretty-html*
           #:*lang*
           #:with-html-string
           #:*stream*))
(in-package #:reblocks/html)


(defvar *stream*
  (make-synonym-stream '*standard-output*)
  "Reblocks will write all output into this stream.

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

(defmacro with-html ((&key
                      (pretty *pretty-html*)
                      (lang *lang*))
                     &body body)
  "Renders body using [Spinneret](https://github.com/ruricolist/spinneret) HTML generator.

   "
  `(let ((spinneret:*html* *stream*)
         (spinneret:*html-lang* ,lang)
         ;; We want to an HTML which is nice to read, by default
         (*print-pretty* ,pretty))
     (spinneret:with-html 
       ,@body)))


(defmacro with-html-string ((&key
                             (pretty *pretty-html*)
                             (lang *lang*))
                            &body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*stream*)
     (with-html (:pretty ,pretty :lang ,lang)
       ,@body)))


(defun get-rendered-chunk ()
  "Returns a string containing HTML redered since last call to
   with-html, with-html-string or get-rendered-chunk.

   Call to this function clears internal stream, used for rendering!"
  (get-output-stream-string *stream*))
