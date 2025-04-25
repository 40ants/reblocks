(uiop:define-package #:reblocks/error-handler
  (:use #:cl)
  (:import-from #:reblocks/debug)
  (:import-from #:log4cl-extras/error
                #:print-backtrace)
  (:import-from #:reblocks/response
                #:get-response
                #:immediate-response)
  (:import-from #:log)
  (:import-from #:reblocks/variables
                #:*current-app*
                #:*invoke-debugger-on-error*)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/page
                #:with-page-defaults)
  
  (:export #:on-error))
(in-package #:reblocks/error-handler)


(defgeneric on-error (app condition &key backtrace)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call reblocks/response:immediate-response like this:

                       \(reblocks/response:immediate-response
                           \"Unhandled condition\"
                           :code 500)

"))


(defmethod on-error (app condition &key backtrace)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))

  (let ((page (with-html-string
                (:h1 "Unhandled exception")
                (:h2 ("~A" condition))
                (when (and (reblocks/debug:status)
                           backtrace)
                  (:pre backtrace)))))
    (immediate-response page
                        :code 500)))


(defun call-with-handled-errors (body-func)
  (let ((debugger-was-invoked-on-cond nil)
        (backtrace nil))
    ;; We need to have this handler-bind block a separate from the inner one,
    ;; because when (on-error) call happens, bindings from the inner handler-bind
    ;; aren't available, but we need to catch an immediate-response condition
    (handler-bind ((immediate-response
                     (lambda (condition)
                       (return-from call-with-handled-errors
                         (get-response condition)))))
        (handler-bind ((error
                         (lambda (condition)
                           (setf backtrace
                                 (print-backtrace :condition condition
                                                  :stream nil))
                           (cond (*invoke-debugger-on-error*
                                  (log:warn "Invoking interactive debugger because Reblocks is in the debug mode")
                                  (setf debugger-was-invoked-on-cond
                                        condition)
                                  (invoke-debugger condition))
                                 (t
                                  (log:warn "Returning error because Reblocks is not in the debug mode")
                                  (with-page-defaults
                                    (on-error *current-app*
                                              condition
                                              :backtrace backtrace)))))))
          (restart-case
              (funcall body-func)
            (abort ()
              :report "Abort request processing and return 500."
              (log:warn "Aborting request processing")
              (with-page-defaults
                (on-error *current-app*
                          debugger-was-invoked-on-cond
                          :backtrace backtrace))))))))


(defmacro with-handled-errors (() &body body)
  `(call-with-handled-errors (lambda ()
                               ,@body)))
