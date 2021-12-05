(uiop:define-package #:weblocks/doc/request
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:weblocks/request))
(in-package weblocks/doc/request)


(in-readtable pythonic-string-syntax)


(defsection @request (:title "Request"
                      :ignore-words ("POST"
                                     "HTTP"
                                     "AJAX"
                                     "URL"))
  """

  ## API
  """

  (weblocks/request:get-parameters function)
  (weblocks/request:get-parameter function)
  (weblocks/request:get-header function)
  (weblocks/request:get-scheme function)
  (weblocks/request:ajax-request-p function)
  (weblocks/request:get-host function)
  (weblocks/request:get-port function)
  (weblocks/request:get-method function)
  (weblocks/request:refresh-request-p function)
  (weblocks/request:remove-header function)
  (weblocks/request:get-uri function)
  (weblocks/request:get-path function)
  (weblocks/request:with-request macro)
  (weblocks/request:pure-request-p function))
