(defpackage #:weblocks/doc/widgets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/dependencies)
  (:import-from #:weblocks/doc/example
                #:defexample))
(in-package weblocks/doc/widgets)


(defsection @widgets (:title "Widgets"
                      :ignore-words ("CSS"
                                     "API"
                                     "JS"
                                     "HTML"
                                     "DIV"))
  "Widget objects should subclasses of WEBLOCKS/WIDGET:WIDGET class
   also, minimally you have to define a method for the
   WEBLOCKS/WIDGET:RENDER generic-function. This function should return
   use WEBLOCKS/HTML:WITH-HTML macro to render the widget.

   Other parts of API around widgets are:

   - WEBLOCKS/DEPENDENCIES:GET-DEPENDENCIES - returns a list of CSS/JS dependencies.
   - WEBLOCKS/WIDGET:UPDATE - marks a widget as need to update on the frontend.
   - WEBLOCKS/WIDGET:GET-HTML-TAG - returns a HTML tag instead of standard DIV.
   - WEBLOCKS/WIDGET:GET-CSS-CLASSES - returns a list of CSS classes. By default returns :WIDGET and a widget class's name.
   - WEBLOCKS/WIDGET:CREATE-WIDGET-FROM - return a widget for representing an object. This way widgets can be created out of strings, functions, etc.

   # Example

   To define a widget, use WEBLOCKS/WIDGET:DEFWIDGET macro. It creates a class
   with a proper meta-class. Old Weblocks version used this metaclass to
   discover changes slots, and probably this feature will be returned back some day.

   ```
   CL-USER> (weblocks/widget:defwidget hello ()
              ((name :initarg :name
                     :reader get-name)))
   #<WEBLOCKS/WIDGETS/MOP:WIDGET-CLASS COMMON-LISP-USER::HELLO>

   CL-USER> (defmethod weblocks/widget:render ((widget hello))
              (weblocks/html:with-html
                (:span (\"Hello ~A\" (get-name widget)))))
   #<STANDARD-METHOD WEBLOCKS/WIDGET:RENDER (HELLO) {1004E27BC3}>
   ```

   Then call this, to run a webserver and preview your widget in the browser:

   ```
   CL-USER> (weblocks/preview:preview
             (make-instance 'hello
                            :name \"Bob\"))
   ```

   A result will look like this:"

  (hello-world weblocks-example)

  
  "# API"
  
  (weblocks/widget:widget class)
  (weblocks/widget:defwidget macro)
  (weblocks/widget:render generic-function)
  (weblocks/widget:update generic-function)
  (weblocks/widget:get-html-tag generic-function)
  (weblocks/widget:get-css-classes generic-function)
  (weblocks/widget:create-widget-from generic-function))


(defexample hello-world ()
  (weblocks/widget:defwidget hello ()
    ((name :initarg :name
           :reader get-name)))

  (defmethod weblocks/widget:render ((widget hello))
    (weblocks/html:with-html
      (:h1 ("Hello ~A" (get-name widget)))
      (:ul (:li "Just a list.")
           (:li "To demonstrate templating.")
           (:li "Engine."))))

  (defun make-example ()
    (make-instance 'hello :name "Bob")))


;; It will be cool to create an extension to 40ANTS-DOC, to render widget examples in the documentation,
;; But I was unable to run cl-selenium (https://github.com/TatriX/cl-selenium-webdriver) headless yet.
