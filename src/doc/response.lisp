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
                                      "SETF"
                                      "AJAX"
                                      "HTTP"))
  (@best-practice section)
  (@api section))


(defsection @best-practice (:title "Best Practice")
  "To simplify debugging, it is better to use structured logging and include a request id into all log messages and HTTP server response.

   Adding such request id  is as simple as adding a method for REBLOCKS/REQUEST-HANDLER:HANDLE-REQUEST generic-function:

   ```lisp
   (defmethod reblocks/request-handler:handle-request ((app app))
     (let ((*request-id* (make-request-id)))
       (reblocks/response:add-header :x-request-id
                                     *request-id*)
       (with-fields (:request-id *request-id*)
         (call-next-method))))
   ```

   Here we use LOG4CL-EXTRAS/CONTEXT:WITH-FIELDS macro for structured logging and REBLOCKS/RESPONSE:ADD-HEADER
   to add the `X-Request-Id` header to webserver's response.

   Also, you might want to define a method for REBLOCKS/ERROR-HANDLER:ON-ERROR generic-function and show
   current request-id to the user. This way he could provide id to support simplifying issue investigation.
")


(defsection @api (:title "API")
  (reblocks/response:add-header function)
  (reblocks/response:add-retpath-to function)

  (reblocks/response:status-code function)
  (reblocks/response:get-content function)
  (reblocks/response:get-content-type function)
  (reblocks/response:get-headers function)
  (reblocks/response:set-cookie function)
  (reblocks/response:cookies-to-set function)
  ;; (reblocks/response:get-response generic-function)
  (reblocks/response:get-response (reader reblocks/response:immediate-response))
  (reblocks/response:immediate-response function)
  (reblocks/response:immediate-response condition)
  (reblocks/response:make-response function)
  (reblocks/response:make-uri function)
  (reblocks/response:send-script function)

  "# Deprecated"
  (reblocks/response:get-code function)
  (reblocks/response:get-custom-headers function))
