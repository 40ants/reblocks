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
                      (pretty nil pretty-given-p)
                      (lang nil lang-given-p))
                     &body body)
  "Renders body using [Spinneret](https://github.com/ruricolist/spinneret) HTML generator.

   "
  `(let* ((spinneret:*html* *stream*)
          (spinneret:*html-lang* ,(if lang-given-p
                                      lang
                                      '*lang*))
          (*print-pretty* ,(if pretty-given-p
                               pretty
                               ;; We want to an HTML which is nice to read, by default,
                               ;; or reuse setting set by outer with-html form:
                               '*pretty-html*))
          ;; To make nested with-html and with-html-string work as expected,
          ;; we need to bind this variable, because it is used as default
          ;; for these macro:
          (*pretty-html* *print-pretty*))
     
     (spinneret:with-html 
       ,@body)))


(defmacro with-html-string ((&key
                             (pretty nil pretty-given-p)
                             (lang nil lang-given-p))
                            &body body)
  "Like WITH-HTML, but capture the output as a string."
  (let ((with-html-args
            (append
             (when pretty-given-p
               (list :pretty
                     pretty))
             (when lang-given-p
               (list :lang lang)))))
    `(with-output-to-string (*stream*)
       (with-html (,@with-html-args)
         ,@body))))


(defun get-rendered-chunk ()
  "Returns a string containing HTML redered since last call to
   with-html, with-html-string or get-rendered-chunk.

   Call to this function clears internal stream, used for rendering!"
  (get-output-stream-string *stream*))
