(uiop:define-package #:reblocks/doc/commands
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:reblocks/commands
                #:add-command
                #:add-commands
                #:get-collected-commands
                #:with-collected-commands))
(in-package #:reblocks/doc/commands)


(in-readtable pythonic-string-syntax)


(defsection @commands (:title "Commands"
                       :ignore-words ("JS"
                                      "CSS"
                                      "AJAX"))
  """
  Commands are small pieces of information sent to the frontend for execution.
  They are used to update widgets, to load JS or CSS code or to call some other functions
  inside the frontend.

  Right now this mechanism is not really useful because there is no way to define
  a custom commands handler on the frontend. But in the future such a facility may be added.
  """
  
  ;; (@commands-api section)
  )


;; (defsection @commands-api (:title "API")
;;   (with-collected-commands macro)
;;   (get-collected-commands function)
;;   (add-command function)
;;   (add-commands function))
