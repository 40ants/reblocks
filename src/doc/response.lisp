(uiop:define-package #:weblocks/doc/response
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:weblocks/response
                #:response)
  (:import-from #:40ants-doc
                #:defsection))
(in-package weblocks/doc/response)


(in-readtable pythonic-string-syntax)


(defsection @response (:title "Response"
                       :ignore-words ("URL"
                                      "AJAX"
                                      "HTTP"))
  """
  

  ## API
  """

  (weblocks/response:add-header function)
  (weblocks/response:add-retpath-to function)
  (response class)
  (weblocks/response:get-code (reader response))
  (weblocks/response:get-content (reader response))
  (weblocks/response:get-content-type (reader response))
  (weblocks/response:get-custom-headers (reader response))
  (weblocks/response:get-headers function)
  (weblocks/response:get-response generic-function)
  (weblocks/response:immediate-response function)
  (weblocks/response:immediate-response condition)
  (weblocks/response:make-response function)
  (weblocks/response:make-uri function)
  (weblocks/response:send-script function))
