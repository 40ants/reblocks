(uiop:define-package #:reblocks/html
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:export #:with-html
           #:*pretty-html*
           #:*lang*
           #:with-html-string
           #:*stream*))
(in-package reblocks/html)

(in-readtable pythonic-string-syntax)


(defsection @html (:title "HTML Rendering"
                   :ignore-words ("HTML"
                                  "DIV"
                                  "CSS"
                                  "CL-WHO"
                                  "UI"
                                  "WEBLOCKS-UI")
                   :external-links (("Spinneret" . "https://github.com/ruricolist/spinneret")
                                    ("CL-WHO" . "https://edicl.github.io/cl-who/")
                                    ("WEBLOCKS-UI" . "https://github.com/40ants/weblocks-ui")))
"""
Out of the box, Reblocks provides a few facilities for HTML generation.
They are based on [`Spinneret`][Spinneret] templating engine. Old version of Weblocks used
[CL-WHO][CL-WHO] instead. But Spinneret is more flexible and what is more important,
it escapes content by default, preventing HTML injection vulnerability.

Most of the time, you only will need a REBLOCKS/HTML:WITH-HTML macro, which is
similary to Spinneret's one, but binds a few special variables to a stream
to write output to and how to write it:

```cl-transcript
(reblocks/html:with-html
   (:ul
    (:li "One")
    (:li "Two")
    (:li "Three")))
;.. <ul>
;..  <li>One
;..  <li>Two
;..  <li>Three
;.. </ul>
;=> NIL
```

Sometimes you might want to get a HTML string instead. In this case you might use
REBLOCKS/HTML:WITH-HTML-STRING:

```cl-transcript
(reblocks/html:with-html-string
   (:ul
    (:li "One")
    (:li "Two")
    (:li "Three")))
;..
;=> "<ul>
;->  <li>One
;->  <li>Two
;->  <li>Three
;-> </ul>"
```
  
You can use any other templating engine, just ensure
it writes output to the REBLOCKS/HTML:*STREAM* variable.

For more advanced UI, look at the [WEBLOCKS-UI][WEBLOCKS-UI] documentation.

## API
  
"""

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
