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
  (@widgets section)
  (@sub-widgets section))


(defsection @html (:title "HTML Rendering"
                   :external-links (("Spinneret" . "https://github.com/ruricolist/spinneret")
                                    ("CL-WHO" . "https://edicl.github.io/cl-who/")
                                    ("REBLOCKS-UI" . "https://github.com/40ants/reblocks-ui")))
  """
Out of the box, Reblocks provides a few facilities for HTML generation.
They are based on the [`Spinneret`][Spinneret] templating engine. The old version of Weblocks used
[CL-WHO][CL-WHO] instead. But Spinneret is more flexible and, more importantly,
it escapes content by default, preventing HTML injection vulnerability.

Most of the time, only the REBLOCKS/HTML:WITH-HTML macro will be needed, which is
similar to Spinneret's one, but binds a few special variables to a stream
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

Sometimes it might be desirable to get an HTML string instead. In this case, use
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
  For rendering widget content, a method for the REBLOCKS/WIDGET:RENDER generic function must be defined.
  Use the REBLOCKS/HTML:WITH-HTML macro to write HTML using lisp expressions. Under the hood, this macro
  uses the great [Spinneret](https://github.com/ruricolist/spinneret) library:
  
  ```
  (defwidget foo ()
    ())


  (defmethod reblocks/widget:render ((obj foo))
    (reblocks/html:with-html ()
      (:p "Hello world!")))

  ```

  Also, any string or object returned from the render method will be used
  to render the widget's content. For example:

  ```
  (defmethod reblocks/widget:render ((obj foo))
    "<b>Hello</b> world!")
  ```

  But in this case, the symbols `<` and `>` will be escaped and "Hello" will not appear in bold font.

  If Spinneret is not desired for rendering but unescaped HTML is wanted, write
  the HTML string into the REBLOCKS/HTML:*STREAM* stream like this:
  Note that in this case it should be explicitly stated that the method does not return anything useful:

  ```
  (defmethod reblocks/widget:render ((obj foo))
    (write-string "<b>Hello</b> world!"
                  reblocks/html:*stream*
                  )
    (values))
  ```

  This will render "Hello" in bold as desired.

  This method can be used for rendering some widgets using template engines other than Spinneret, such as:

  - https://github.com/mmontone/djula
  - https://github.com/kanru/cl-mustache
  - https://github.com/moderninterpreters/markup

  Just use the engine of choice and write its output to REBLOCKS/HTML:*STREAM*!
  
  """)

(defsection @sub-widgets (:title "Sub-widget Rendering")
  """
  Reblocks pages are represented by a root widget which can have sub-widgets. Thus there is
  a widget tree.

  To render subwidgets, call the REBLOCKS/WIDGET:RENDER generic function
  on each subwidget inside the widget's REBLOCKS/WIDGET:RENDER generic function method:

  ```
  (defmethod reblocks/widget:render ((obj some-widget))
    (reblocks/widget:render (header-widget obj))
    (reblocks/widget:render (table-widget obj)))
  ```

  If using the REBLOCKS/HTML:WITH-HTML macro and wrapping subwidgets with a custom HTML
  tag, then implicit rendering can be used by just passing subwidgets to Spinneret's HTML tag:

  ```
  (defmethod reblocks/widget:render ((obj some-widget))
    (reblocks/html:with-html ()
      (:div :class "flex flex-col"
            (header-widget obj)
            (table-widget obj))))
  ```

  This works because Reblocks defines a method for the `spinneret:html` generic function.
  """
  )

(defsection @page (:title "Page Rendering")
  """
  These functions can be used during rendering
  to retrieve information about the page:

  - REBLOCKS/PAGE:RENDER generic-function
  - REBLOCKS/PAGE:RENDER-BODY generic-function
  - REBLOCKS/PAGE:RENDER-DEPENDENCIES generic-function
  
  The render protocol first renders the widget tree and
  only after that renders the page HTML headers.
  Thus `setf` can be used on these functions during
  widget rendering to change the title, description
  or keywords:
  
  - REBLOCKS/PAGE:GET-TITLE
  - REBLOCKS/PAGE:GET-DESCRIPTION
  - REBLOCKS/PAGE:GET-KEYWORDS
  - REBLOCKS/PAGE:GET-LANGUAGE

  If changing these variables globally for the whole
  application is desired, then define a before method like this:

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
