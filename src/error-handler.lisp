(uiop:define-package #:reblocks/error-handler
  (:use #:cl)
  (:import-from #:reblocks/debug)
  (:import-from #:log4cl-extras/error
                #:print-backtrace)
  (:import-from #:reblocks/response
                #:not-found-error
                #:get-response
                #:immediate-response)
  (:import-from #:log)
  (:import-from #:reblocks/variables
                #:*current-app*
                #:*invoke-debugger-on-error*)
  (:import-from #:reblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:reblocks/page
                #:with-page-defaults)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget
                #:widget)
  (:import-from #:local-time
                #:now
                #:adjust-timestamp!)
  (:import-from #:reblocks/app
                #:page-constructor)
  (:import-from #:reblocks/hooks
                #:with-render-hook)
  (:import-from #:reblocks/page-dependencies
                #:with-collected-dependencies)
  (:import-from #:reblocks/request-handler
                #:handle-normal-request)
  
  (:export #:on-error))
(in-package #:reblocks/error-handler)


(defgeneric on-error (app condition &key backtrace)
  (:documentation "This method is called when some unhandled error was raised by application.
                   It should call reblocks/response:immediate-response like this:

                       \(reblocks/response:immediate-response
                           \"Unhandled condition\"
                           :code 500)

"))


(defwidget error-page-widget ()
  ((condition :initarg :condition
              :reader error-page-condition)
   (backtrace :initarg :backtrace
              :reader error-page-backtrace)))


(defmethod render ((widget error-page-widget))
  (let ((title "Unhandled exception"))
    (setf (reblocks/page:get-title)
          title)
    (with-html ()
      (:h1 title)
      (:h2 ("~A" (error-page-condition widget)))
      (when (and (reblocks/debug:status)
                 (error-page-backtrace widget))
        (:pre (error-page-backtrace widget))))))


(defmethod on-error (app condition &key backtrace)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  
  (make-instance 'error-page-widget
                 :condition condition
                 :backtrace backtrace))


(defun call-with-handled-errors (body-func)
  (let ((catched-condition nil)
        (backtrace nil))
    ;; We need to have this handler-bind block a separate from the inner one,
    ;; because when (on-error) call happens, bindings from the inner handler-bind
    ;; aren't available, but we need to catch an immediate-response condition
    (flet ((return-error-response (condition backtrace)
             (with-page-defaults ()
               (let* ((error-response
                        (on-error *current-app*
                                  condition
                                  :backtrace backtrace))
                      (response-as-str
                        (etypecase error-response
                          (string error-response)
                          (widget
                           (with-collected-dependencies ()
                             (with-render-hook (*current-app*)
                               (with-html-string ()
                                 (handle-normal-request
                                  *current-app*
                                  :page
                                  ;; TODO: probably extract common error page
                                  ;; making code with server.lisp:
                                  (let ((wrapped-result
                                          (funcall (page-constructor *current-app*)
                                                   error-response)))
                                    (make-instance 'reblocks/page::page
                                                   :root-widget wrapped-result
                                                   :path (reblocks/request:get-path)
                                                   :expire-at (adjust-timestamp!
                                                                  (now)
                                                                ;; It is no make sence to cache 404 pages
                                                                (:offset :sec 5))))))))))))
                 (list 500
                       (list :content-type "text/html")
                       (list
                        response-as-str))))))
      
      (handler-bind ((immediate-response
                       (lambda (condition)
                         (return-from call-with-handled-errors
                           (get-response condition)))))
        (handler-bind ((serious-condition
                         (lambda (condition)
                           ;; We don't want to open debugger on 404 errors
                           (unless (typep condition 'not-found-error)
                             (setf backtrace
                                   (print-backtrace :condition condition
                                                    :stream nil))
                             (setf catched-condition
                                   condition)
                             (cond ((and *invoke-debugger-on-error*
                                         *debugger-hook*)
                                    (log:warn "Invoking interactive debugger because Reblocks is in the debug mode")

                                    (invoke-debugger condition))
                                   (t
                                    (log:warn "Returning error because Reblocks is not in the debug mode")
                                    (return-from call-with-handled-errors
                                      (return-error-response condition
                                                             (print-backtrace :condition condition
                                                                              :stream nil)))))))))
          (restart-case
              (funcall body-func)
            (abort ()
              :report "Abort request processing and return 500."
              (log:warn "Aborting request processing")
              (return-error-response catched-condition
                                     backtrace))))))))



(defmacro with-handled-errors (() &body body)
  `(call-with-handled-errors (lambda ()
                               ,@body)))


(defun call-with-immediate-response-handler (body-func)
  ;; We need to have this handler-bind block a separate from the inner one,
  ;; because when (on-error) call happens, bindings from the inner handler-bind
  ;; aren't available, but we need to catch an immediate-response condition
  (handler-bind ((immediate-response
                   (lambda (condition)
                     (return-from call-with-immediate-response-handler
                       (get-response condition)))))
    (funcall body-func)))


(defmacro with-immediate-response-handler (() &body body)
  `(flet ((immediate-response-handler-thunk ()
            ,@body))
     (declare (dynamic-extent #'immediate-response-handler-thunk))
     (call-with-immediate-response-handler #'immediate-response-handler-thunk)))
