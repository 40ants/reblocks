(uiop:define-package #:weblocks/doc/actions
  (:use #:cl)
  (:import-from #:weblocks/actions
                #:eval-action)
  (:import-from #:40ants-doc
                #:defsection))
(in-package weblocks/doc/actions)


(defsection @actions (:title "Actions"
                      :ignore-words ("AJAX"
                                     "POST"
                                     "GET"))
  "Actions is the core component allowing interactivity in Weblocks applications.

   Actions are callbacks that are stored in a session and can be called from the browser
   using AJAX, POST or GET requests.

   ## API"

  (eval-action generic-function))
