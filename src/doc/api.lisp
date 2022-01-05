(defpackage #:reblocks/doc/api
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/app)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/html)
  (:import-from #:reblocks/actions)
  (:import-from #:reblocks/response)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/variables)
  (:export
   #:@api))
(in-package #:reblocks/doc/api)


(defsection @api (:title "Unsorted API"
                  :ignore-words ("API"
                                 "URI"))
  (reblocks/widgets/mop:widget-class class)
  (reblocks/response:redirect function)
  (reblocks/session:init generic-function)

  "## Variables"

  (reblocks/variables:*current-app* variable)
  (reblocks/variables:*default-content-type* variable)
  (reblocks/variables:*invoke-debugger-on-error* variable)
  (reblocks/variables:*backtrace-on-session-init-error* variable))
