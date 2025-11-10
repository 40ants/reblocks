(defpackage #:reblocks/doc/widgets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/dependencies)
  (:import-from #:reblocks/doc/example
                #:defexample))
(in-package #:reblocks/doc/widgets)


(defsection @widgets (:title "Widgets"
                      :ignore-words ("CSS"
                                     "API"
                                     "DOM"
                                     "JS"
                                     "HTML"
                                     "DIV"))
  "Widget objects should be subclasses of the REBLOCKS/WIDGET:WIDGET class.
   Also, minimally you have to define a method for the
   REBLOCKS/WIDGET:RENDER generic function. This function should
   use the REBLOCKS/HTML:WITH-HTML macro to render the widget.

   Other parts of the API around widgets are:

   - REBLOCKS/DEPENDENCIES:GET-DEPENDENCIES - returns a list of CSS/JS dependencies.
   - REBLOCKS/WIDGET:UPDATE - marks a widget as needing to update on the frontend.
   - REBLOCKS/WIDGET:GET-HTML-TAG - returns an HTML tag instead of the standard DIV.
   - REBLOCKS/WIDGET:GET-CSS-CLASSES - returns a list of CSS classes. By default returns :WIDGET and the widget class's name.
   - REBLOCKS/WIDGET:CREATE-WIDGET-FROM - returns a widget for representing an object. This way widgets can be created out of strings, functions, etc.

   # Example

   To define a widget, use the REBLOCKS/WIDGET:DEFWIDGET macro. It creates a class
   with a proper meta-class. The old Weblocks version used this metaclass to
   discover slot changes, and probably this feature will be returned some day.

   ```
   CL-USER> (reblocks/widget:defwidget hello ()
              ((name :initarg :name
                     :reader get-name)))
   #<REBLOCKS/WIDGETS/MOP:WIDGET-CLASS COMMON-LISP-USER::HELLO>

   CL-USER> (defmethod reblocks/widget:render ((widget hello))
              (reblocks/html:with-html ()
                (:span (\"Hello ~A\" (get-name widget)))))
   #<STANDARD-METHOD REBLOCKS/WIDGET:RENDER (HELLO) {1004E27BC3}>
   ```

   Then call this to run a webserver and preview your widget in the browser:

   ```
   CL-USER> (reblocks/preview:preview
             (make-instance 'hello
                            :name \"Bob\"))
   ```

   The result will look like this:"

  (hello-world reblocks-example)

  
  ;; "# API"
  
  ;; (reblocks/widget:widget class)
  ;; (reblocks/widget:defwidget macro)
  ;; (reblocks/widget:render generic-function)
  ;; (reblocks/widget:update generic-function)
  ;; (reblocks/widget:get-html-tag generic-function)
  ;; (reblocks/widget:get-css-classes generic-function)
  ;; (reblocks/widget:create-widget-from generic-function)

  "# String widget

   This is a simple type of widget which can be made out of any string.

   Create it using the REBLOCKS/WIDGETS/STRING-WIDGET:MAKE-STRING-WIDGET function."

  ;; (reblocks/widgets/string-widget:string-widget class)
  ;; (reblocks/widgets/string-widget:make-string-widget function)
  ;; (reblocks/widgets/string-widget:get-content (accessor reblocks/widgets/string-widget:string-widget))
  ;; (reblocks/widgets/string-widget:escape-p (accessor reblocks/widgets/string-widget:string-widget))
  )


(defexample hello-world ()
  (reblocks/widget:defwidget hello ()
    ((name :initarg :name
           :reader get-name)))

  (defmethod reblocks/widget:render ((widget hello))
    (reblocks/html:with-html ()
      (:h1 ("Hello ~A" (get-name widget)))
      (:ul (:li "Just a list.")
           (:li "To demonstrate templating.")
           (:li "Engine."))))

  (defun make-example ()
    (make-instance 'hello :name "Bob")))


;; It will be cool to create an extension to 40ANTS-DOC, to render widget examples in the documentation,
;; But I was unable to run cl-selenium (https://github.com/TatriX/cl-selenium-webdriver) headless yet.
