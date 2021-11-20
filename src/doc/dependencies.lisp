(uiop:define-package #:weblocks/doc/dependencies
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package weblocks/doc/dependencies)


(defsection @dependencies (:title "Dependencies"
                           :ignore-words ("CSS"
                                          "JS"
                                          "HTML"
                                          "CDN"
                                          "LASS"
                                          "ASDF"))
  "When Weblocks renders page, it collects CSS and JS dependencies from all widgets
   by calling generic-function WEBLOCKS/DEPENDENCIES:GET-DEPENDENCIES. Also, this function
   is called on the current app to get such common dependencies as jQuery and CSS framework.

   If you are defining your own method for WEBLOCKS/DEPENDENCIES:GET-DEPENDENCIES generic-function,
   make sure it returns a list of WEBLOCKS/DEPENDENCIES:DEPENDENCY objects.

   Typically, you already have JS or CSS file somewhere near your ASDF system. In this case,
   you can use WEBLOCKS/DEPENDENCIES:MAKE-DEPENDENCY function like this:

   ```
   (defmethod weblocks/dependencies:get-dependencies ((app my-app))
     (list* (weblocks/dependencies:make-dependency \"js/my-app.js\"
                                                   :system :some-asdf-system)
            (call-next-method)))
   ```

   Other way to specify a dependency is to use Parenscript or LASS to define JS or CSS right inside
   the method. Here we define CSS code for the widget:

   ```
   (defmethod weblocks/dependencies:get-dependencies ((widget my-custom-widget))
     (list*
      (weblocks-lass:make-dependency
       `(.my-custom-widget
         :border 2px solid red
         :padding 1em))
      (call-next-method)))
   ```

   Pay attention, this code uses WEBLOCKS-LASS:MAKE-DEPENDENCY function which is available from
   the separate asdf system WEBLOCKS-LASS.
"
  (weblocks/dependencies:dependency class)
  (weblocks/dependencies:remote-dependency class)
  (weblocks/dependencies:local-dependency class)
  
  (weblocks/dependencies:make-dependency function)
  (weblocks/dependencies:get-dependencies generic-function))


