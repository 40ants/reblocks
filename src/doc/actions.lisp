(uiop:define-package #:weblocks/doc/actions
  (:use #:cl)
  (:import-from #:weblocks/actions
                #:eval-action
                #:make-action
                #:make-js-action
                #:make-js-form-action
                #:on-missing-action)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:weblocks/variables
                #:*action-string*))
(in-package weblocks/doc/actions)


(defsection @actions (:title "Actions"
                      :ignore-words ("AJAX"
                                     "JS"
                                     "POST"
                                     "GET"))
  "Actions is the core component allowing interactivity in Weblocks applications.

   Actions are callbacks that are stored in a session and can be called from the browser
   using AJAX, POST or GET requests.

   ## API"

  (eval-action generic-function)
  (on-missing-action generic-function)
  (make-action function)
  (make-js-action function)
  (make-js-form-action function)
  (*action-string* variable))
