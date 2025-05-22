(uiop:define-package #:reblocks/doc/rendering
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:reblocks/html)
  (:import-from #:reblocks/page)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/rendering)


(in-readtable pythonic-string-syntax)


(defsection @rendering (:title "Content Rendering"
                        :ignore-words ("HTML"
                                       "DIV"
                                       "CSS"
                                       "JS"
                                       "CL-WHO"
                                       "UI"
                                       "REBLOCKS-UI"))
  (@page section)
  (@html section)
  (@widgets section))


(defsection @html (:title "HTML Rendering"
                   :external-links (("Spinneret" . "https://github.com/ruricolist/spinneret")
                                    ("CL-WHO" . "https://edicl.github.io/cl-who/")
                                    ("REBLOCKS-UI" . "https://github.com/40ants/reblocks-ui")))
"""
Out of the box, Reblocks provides a few facilities for HTML generation.
They are based on [`Spinneret`][Spinneret] templating engine. Old version of Weblocks used
[CL-WHO][CL-WHO] instead. But Spinneret is more flexible and what is more important,
it escapes content by default, preventing HTML injection vulnerability.

Most of the time, you only will need a REBLOCKS/HTML:WITH-HTML macro, which is
similary to Spinneret's one, but binds a few special variables to a stream
to write output to and how to write it:

```cl-transcript
(reblocks/html:with-html ()
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
(reblocks/html:with-html-string ()
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

For more advanced UI, look at the [REBLOCKS-UI][REBLOCKS-UI] documentation.

  """
)


(defsection @widgets (:title "Widget Rendering")
  """
  For rendering a widget content you have to define a method for REBLOCKS/WIDGET:RENDER generic-function.
  Use REBLOCKS/HTML:WITH-HTML macro to write HTML using lisp expression. Under the hood, this macro
  uses a great [Spinneret](https://github.com/ruricolist/spinneret) library:
  
  ```
  (defwidget foo ()
    ())


  (defmethod reblocks/widget:render ((obj foo))
    (reblocks/html:with-html ()
      (:p "Hello world!")))

  ```

  Also, any string or object, returned from the render method, will be used
  to render widget's content. For example, you might do:

  ```
  (defmethod reblocks/widget:render ((obj foo))
    "<b>Hello</b> world!")
  ```

  But in this case, symbols `<` and `>` will be escaped and you will not see "Hello" in bold font.

  If you don't want to use Spinneret for rendering but want to get HTML non-escaped, you need to write
  HTML string into the REBLOCKS/HTML:*STREAM* stream like this:
  Note, in this case we should explicitly say that our method does not return anything useful:

  ```
  (defmethod reblocks/widget:render ((obj foo))
    (write-string "<b>Hello</b> world!"
                  reblocks/html:*stream*
                  )
    (values))
  ```

  This will render "Hello" in bold as desired.

  You can use this method for rendering some widgets using template engines other than Spinneret, such as:

  - https://github.com/mmontone/djula
  - https://github.com/kanru/cl-mustache
  - https://github.com/moderninterpreters/markup

  Just use engine you like and write it's output to REBLOCKS/HTML:*STREAM*!
  
  """)


(defsection @page (:title "Page Rendering")
  """
  These functions can be used during rendering
  to retrieve an information about the page:

  - REBLOCKS/PAGE:RENDER generic-function
  - REBLOCKS/PAGE:RENDER-BODY generic-function
  - REBLOCKS/PAGE:RENDER-DEPENDENCIES generic-function
  
  Render protocol first renders the widget tree and
  only after that renders page HTML headers.
  Thus you might use `setf` on these functions during
  widget rendering to change the title, description
  or keywords:
  
  - REBLOCKS/PAGE:GET-TITLE
  - REBLOCKS/PAGE:GET-DESCRIPTION
  - REBLOCKS/PAGE:GET-KEYWORDS
  - REBLOCKS/PAGE:GET-LANGUAGE

  If you want to change these variables globally for the whole
  application, then define a before method like this:

  ```lisp
  (defmethod reblocks/page:render :before ((app disk-analyzer) inner-html &rest rest)
    (declare (ignore rest))
  
    (setf (reblocks/page:get-title)
          "Cloud Analyzer - Saves space and money!")
    (setf (reblocks/page:get-description)
          "Helps to save money when storing data in the Cloud.")
    (setf (reblocks/page:get-keywords)
          (list "cloud"
                "storage"
                "analyzer")))

  ```

  """
  )
