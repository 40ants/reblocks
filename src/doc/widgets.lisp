(defpackage #:reblocks/doc/widgets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/dependencies)
  (:import-from #:reblocks/doc/example
                #:defexample))
(in-package reblocks/doc/widgets)


(defsection @widgets (:title "Widgets"
                      :ignore-words ("CSS"
                                     "API"
                                     "JS"
                                     "HTML"
                                     "DIV"))
  "Widget objects should subclasses of REBLOCKS/WIDGET:WIDGET class
   also, minimally you have to define a method for the
   REBLOCKS/WIDGET:RENDER generic-function. This function should return
   use REBLOCKS/HTML:WITH-HTML macro to render the widget.

   Other parts of API around widgets are:

   - REBLOCKS/DEPENDENCIES:GET-DEPENDENCIES - returns a list of CSS/JS dependencies.
   - REBLOCKS/WIDGET:UPDATE - marks a widget as need to update on the frontend.
   - REBLOCKS/WIDGET:GET-HTML-TAG - returns a HTML tag instead of standard DIV.
   - REBLOCKS/WIDGET:GET-CSS-CLASSES - returns a list of CSS classes. By default returns :WIDGET and a widget class's name.
   - REBLOCKS/WIDGET:CREATE-WIDGET-FROM - return a widget for representing an object. This way widgets can be created out of strings, functions, etc.

   # Example

   To define a widget, use REBLOCKS/WIDGET:DEFWIDGET macro. It creates a class
   with a proper meta-class. Old Weblocks version used this metaclass to
   discover changes slots, and probably this feature will be returned back some day.

   ```
   CL-USER> (reblocks/widget:defwidget hello ()
              ((name :initarg :name
                     :reader get-name)))
   #<REBLOCKS/WIDGETS/MOP:WIDGET-CLASS COMMON-LISP-USER::HELLO>

   CL-USER> (defmethod reblocks/widget:render ((widget hello))
              (reblocks/html:with-html
                (:span (\"Hello ~A\" (get-name widget)))))
   #<STANDARD-METHOD REBLOCKS/WIDGET:RENDER (HELLO) {1004E27BC3}>
   ```

   Then call this, to run a webserver and preview your widget in the browser:

   ```
   CL-USER> (reblocks/preview:preview
             (make-instance 'hello
                            :name \"Bob\"))
   ```

   A result will look like this:"

  (hello-world weblocks-example)

  
  "# API"
  
  (reblocks/widget:widget class)
  (reblocks/widget:defwidget macro)
  (reblocks/widget:render generic-function)
  (reblocks/widget:update generic-function)
  (reblocks/widget:get-html-tag generic-function)
  (reblocks/widget:get-css-classes generic-function)
  (reblocks/widget:create-widget-from generic-function))


(defexample hello-world ()
  (reblocks/widget:defwidget hello ()
    ((name :initarg :name
           :reader get-name)))

  (defmethod reblocks/widget:render ((widget hello))
    (reblocks/html:with-html
      (:h1 ("Hello ~A" (get-name widget)))
      (:ul (:li "Just a list.")
           (:li "To demonstrate templating.")
           (:li "Engine."))))

  (defun make-example ()
    (make-instance 'hello :name "Bob")))


;; It will be cool to create an extension to 40ANTS-DOC, to render widget examples in the documentation,
;; But I was unable to run cl-selenium (https://github.com/TatriX/cl-selenium-webdriver) headless yet.
