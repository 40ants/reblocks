(defpackage #:weblocks/doc/api
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:@api))
(in-package weblocks/doc/api)


(defsection @api (:title "API"
                  :ignore-words ("API"
                                 "URI"))
  (weblocks/app:defapp macro)
  (weblocks/widget:defwidget macro)
  (weblocks/widgets/mop:widget-class class)
  (weblocks/widget:widget class)
  (weblocks/widget:render generic-function)
  (weblocks/widget:update generic-function)
  (weblocks/html:with-html macro)
  (weblocks/actions:make-js-action function)
  (weblocks/response:redirect function)
  (weblocks/session:init generic-function))
