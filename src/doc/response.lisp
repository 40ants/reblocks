(uiop:define-package #:reblocks/doc/response
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:reblocks/response
                #:response)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/response)


(in-readtable pythonic-string-syntax)


(defsection @response (:title "Response"
                       :ignore-words ("URL"
                                      "AJAX"
                                      "HTTP"))
  """
  

  ## API
  """

  (reblocks/response:add-header function)
  (reblocks/response:add-retpath-to function)
  (reblocks/response:get-code function)
  (reblocks/response:get-content function)
  (reblocks/response:get-content-type function)
  (reblocks/response:get-custom-headers function)
  (reblocks/response:get-headers function)
  ;; (reblocks/response:get-response generic-function)
  (reblocks/response:get-response (reader reblocks/response:immediate-response))
  (reblocks/response:immediate-response function)
  (reblocks/response:immediate-response condition)
  (reblocks/response:make-response function)
  (reblocks/response:make-uri function)
  (reblocks/response:send-script function))
