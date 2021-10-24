(defpackage #:weblocks/doc/widgets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/dependencies))
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

   # API"
  (weblocks/widget:widget class)
  (weblocks/widget:defwidget macro)
  (weblocks/widget:render generic-function)
  (weblocks/widget:update generic-function)
  (weblocks/widget:get-html-tag generic-function)
  (weblocks/widget:get-css-classes generic-function)
  (weblocks/widget:create-widget-from generic-function)

  "# MOVE THESE TO THEIR OWN SECTIONS"

  (weblocks/dependencies:get-dependencies generic-function))
