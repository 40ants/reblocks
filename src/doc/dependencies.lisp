(uiop:define-package #:weblocks/doc/dependencies
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:weblocks/dependencies
                #:remote-dependency
                #:dependency
                #:local-dependency))
(in-package weblocks/doc/dependencies)


(defsection @dependencies (:title "Dependencies"
                           :ignore-words ("CSS"
                                          "JS"
                                          "MIME"
                                          "HTTP"
                                          "HTML"
                                          "AJAX"
                                          "CDN"
                                          "LASS"
                                          "ASDF"
                                          ;; TODO: Make an external link
                                          ;; after documenting weblocks-lass using 40ants-doc
                                          "WEBLOCKS-LASS:MAKE-DEPENDENCY"
                                          "WEBLOCKS-LASS"))
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

   ## API
"
  (dependency class)
  (remote-dependency class)
  (local-dependency class)

  (weblocks/dependencies:*cache-remote-dependencies-in* variable)
  (weblocks/dependencies:get-content-type function)
  (weblocks/dependencies:get-crossorigin (reader remote-dependency))
  (weblocks/dependencies:get-integrity (reader remote-dependency))
  (weblocks/dependencies:get-path generic-function)
  (weblocks/dependencies:get-route generic-function)
  (weblocks/dependencies:get-type (reader dependency))
  (weblocks/dependencies:get-url generic-function)
  (weblocks/dependencies:infer-type-from generic-function)
  (weblocks/dependencies:push-dependency function)
  (weblocks/dependencies:push-dependencies function)
  (weblocks/dependencies:render-in-ajax-response generic-function)
  (weblocks/dependencies:render-in-head generic-function)
  (weblocks/dependencies:serve generic-function)
  (weblocks/dependencies:with-collected-dependencies macro)
  
  (weblocks/dependencies:make-dependency function)
  (weblocks/dependencies:get-dependencies generic-function))


