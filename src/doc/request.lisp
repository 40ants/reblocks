(uiop:define-package #:reblocks/doc/request
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:reblocks/request))
(in-package #:reblocks/doc/request)


(in-readtable pythonic-string-syntax)


(defsection @request (:title "Request"
                      :ignore-words ("POST"
                                     "HTTP"
                                     "AJAX"
                                     "HTML"
                                     "JSON"
                                     "URL"))
  """

  ## API
  """

  (reblocks/request:get-parameters function)
  (reblocks/request:get-parameter function)
  (reblocks/request:get-header function)
  (reblocks/request:get-cookie function)
  (reblocks/request:get-scheme function)
  (reblocks/request:ajax-request-p function)
  (reblocks/request:get-host function)
  (reblocks/request:get-port function)
  (reblocks/request:get-method function)
  (reblocks/request:refresh-request-p function)
  (reblocks/request:remove-header function)
  (reblocks/request:get-uri function)
  (reblocks/request:get-path function)
  (reblocks/request-handler:handle-request generic-function)
  (reblocks/request-handler:handle-ajax-request generic-function)
  (reblocks/request-handler:*request-timeout* variable)
  (reblocks/request-handler:page-not-found-handler generic-function)
  (reblocks/request:with-request macro)
  (reblocks/request:pure-request-p function))
