(defpackage #:weblocks/doc/api
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/html)
  (:import-from #:weblocks/actions)
  (:import-from #:weblocks/response)
  (:import-from #:weblocks/session)
  (:export
   #:@api))
(in-package weblocks/doc/api)


(defsection @api (:title "Unsorted API"
                  :ignore-words ("API"
                                 "URI"))
  (weblocks/widgets/mop:widget-class class)
  (weblocks/actions:make-js-action function)
  (weblocks/response:redirect function)
  (weblocks/session:init generic-function))
