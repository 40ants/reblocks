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


(defsection @rendering (:title "Content Rendering")
  (@page section)
  (@html section))


(defsection @html (:title "HTML Rendering"
                   :ignore-words ("HTML"
                                  "DIV"
                                  "CSS"
                                  "CL-WHO"
                                  "UI"
                                  "REBLOCKS-UI")
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

For more advanced UI, look at the [REBLOCKS-UI][REBLOCKS-UI] documentation.

## API
  
"""

  (reblocks/html:with-html macro)
  (reblocks/html:with-html-string macro)
  (reblocks/html:*stream* variable)
  (reblocks/html:*lang* variable)
  (reblocks/html:*pretty-html* variable))


(defsection @page (:title "Page Rendering"
                   :ignore-words ("HTML"
                                  "CSS"
                                  "JS"))
  (reblocks/page:render generic-function)
  (reblocks/page:render-body generic-function)
  (reblocks/page:render-dependencies generic-function)

  """
  These functions can be used during rendering
  to retrieve an information about the page.

  Render protocol first renders the widget tree and
  only after that renders page HTML headers.
  Thus you might use `setf` on these functions during
  widget rendering to change the title, description
  or keywords.
  """
  (reblocks/page:get-title function)
  (reblocks/page:get-description function)
  (reblocks/page:get-keywords function)
  (reblocks/page:get-language function))
