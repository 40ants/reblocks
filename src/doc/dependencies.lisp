(uiop:define-package #:reblocks/doc/dependencies
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/dependencies
                #:remote-dependency
                #:dependency
                #:local-dependency))
(in-package reblocks/doc/dependencies)


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
   by calling generic-function REBLOCKS/DEPENDENCIES:GET-DEPENDENCIES. Also, this function
   is called on the current app to get such common dependencies as jQuery and CSS framework.

   If you are defining your own method for REBLOCKS/DEPENDENCIES:GET-DEPENDENCIES generic-function,
   make sure it returns a list of REBLOCKS/DEPENDENCIES:DEPENDENCY objects.

   Typically, you already have JS or CSS file somewhere near your ASDF system. In this case,
   you can use REBLOCKS/DEPENDENCIES:MAKE-DEPENDENCY function like this:

   ```
   (defmethod reblocks/dependencies:get-dependencies ((app my-app))
     (list* (reblocks/dependencies:make-dependency \"js/my-app.js\"
                                                   :system :some-asdf-system)
            (call-next-method)))
   ```

   Other way to specify a dependency is to use Parenscript or LASS to define JS or CSS right inside
   the method. Here we define CSS code for the widget:

   ```
   (defmethod reblocks/dependencies:get-dependencies ((widget my-custom-widget))
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

  (reblocks/dependencies:*cache-remote-dependencies-in* variable)
  (reblocks/dependencies:get-content-type function)
  (reblocks/dependencies:get-crossorigin (reader remote-dependency))
  (reblocks/dependencies:get-integrity (reader remote-dependency))
  (reblocks/dependencies:get-path generic-function)
  (reblocks/dependencies:get-route generic-function)
  (reblocks/dependencies:get-type (reader dependency))
  (reblocks/dependencies:get-url generic-function)
  (reblocks/dependencies:infer-type-from generic-function)
  (reblocks/dependencies:push-dependency function)
  (reblocks/dependencies:push-dependencies function)
  (reblocks/dependencies:render-in-ajax-response generic-function)
  (reblocks/dependencies:render-in-head generic-function)
  (reblocks/dependencies:serve generic-function)
  (reblocks/dependencies:with-collected-dependencies macro)
  
  (reblocks/dependencies:make-dependency function)
  (reblocks/dependencies:get-dependencies generic-function))


