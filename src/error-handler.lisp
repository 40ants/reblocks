(defpackage #:reblocks/error-handler
  (:use #:cl)
  (:import-from #:reblocks/debug)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:reblocks/response
                #:get-response
                #:immediate-response)
  (:import-from #:log4cl)
  (:import-from #:reblocks/variables
                #:*current-app*
                #:*invoke-debugger-on-error*)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:reblocks/html
                #:with-html-string)
  
  (:export #:on-error
           #:with-handled-errors))
(in-package reblocks/error-handler)


(defgeneric on-error (app condition)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call reblocks/response:immediate-response like this:

                       \(reblocks/response:immediate-response
                           \"Unhandled condition\"
                           :code 500)

"))


(defmethod on-error (app condition)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))

  (let ((traceback (when condition
                     (print-backtrace
                      condition :output nil))))
    (when traceback
      (log:error "Returning 500 error to user" traceback))

    (let ((page (with-html-string
                  (:h1 "Unhandled exception")
                  (:h2 ("~A" condition))
                  (when (and (reblocks/debug:status)
                             traceback)
                    (:pre traceback)))))
      (immediate-response page
                          :code 500))))


(defmacro with-handled-errors (&body body)
  (with-gensyms (block-name)
    `(block ,block-name
       (let (debugger-was-invoked-on-cond)
         ;; We need to have this handler-bind block a separate from the inner one,
         ;; because when (on-error) call happens, bindings from the inner handler-bind
         ;; aren't available, but we need to catch an immediate-response condition
         (handler-bind ((immediate-response (lambda (c)
                                              (return-from ,block-name
                                                (get-response c)))))
           (handler-bind ((error (lambda (c)
                                   (cond (*invoke-debugger-on-error*
                                          (log:warn "Invoking interactive debugger because Reblocks is in the debug mode")
                                          (setf debugger-was-invoked-on-cond c)
                                          (invoke-debugger c))
                                         (t
                                          (log:warn "Returning error because Reblocks is not in the debug mode")
                                          (on-error *current-app* c))))))
             (restart-case
                 (progn ,@body)
               (abort ()
                 :report "Abort request processing and return 500."
                 (log:error "Aborting request processing")
                 (on-error *current-app*
                           debugger-was-invoked-on-cond)))))))))
