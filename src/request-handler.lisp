(defpackage #:reblocks/request-handler
  (:use #:cl
        #:f-underscore)
  (:import-from #:reblocks/request
                #:get-path
                #:get-action-name-from-request
                #:get-parameter
                #:get-parameters
                #:pure-request-p
                #:ajax-request-p)
  (:import-from #:reblocks/utils/list
                #:alist->plist)
  (:import-from #:reblocks/utils/uri
                #:remove-parameter-from-uri)
  (:import-from #:reblocks/page
                #:render-page-with-widgets)
  (:import-from #:reblocks/session-lock
                #:get-lock)
  (:import-from #:reblocks/widget
                #:render)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:import-from #:reblocks/html
                #:with-html-string
                #:*stream*)
  (:import-from #:reblocks/utils/warn
                #:style-warn
                #:non-idempotent-rendering)
  (:import-from #:reblocks/dependencies
                #:with-collected-dependencies
                #:push-dependencies
                #:get-dependencies
                #:get-collected-dependencies
                #:register-dependencies)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:reblocks/actions
                #:eval-action)
  (:import-from #:reblocks/commands
                #:get-collected-commands)
  (:import-from #:reblocks/error-handler
                #:with-handled-errors
                #:on-error)
  (:import-from #:reblocks/variables
                #:*backtrace-on-session-init-error*
                #:*action-string*)
  (:import-from #:reblocks/utils/timing
                #:*timing-level*
                #:*timing-report-fn*
                #:timing)
  (:import-from #:bordeaux-threads
                #:with-lock-held)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  (:import-from #:trivial-timeout
                #:timeout-error
                #:with-timeout)
  (:import-from #:reblocks/response
                #:get-response
                #:response
                #:immediate-response
                #:make-response
                #:redirect)
  ;; Just dependencies
  (:import-from #:reblocks/hooks)
  (:import-from #:log)
  ;; This package defines an :around method for reblocks/widgets:render
  ;; which adds a wrapper around widget body
  (:import-from #:reblocks/widgets/render-methods)
  (:import-from #:reblocks/widgets/root)
  (:import-from #:reblocks/session)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  
  (:export
   #:handle-request
   #:page-not-found-handler
   *request-timeout*
   #:handle-ajax-request))
(in-package reblocks/request-handler)


(defvar *request-timeout* 180
  "Seconds until we abort a request because it took too long.
  This prevents threads from hogging the CPU indefinitely.

  You can set this to NIL to disable timeouts (not recommended).")


(defgeneric handle-request (app)
  (:documentation
   "This method handles each request as it comes in from the
server. It is a hunchentoot handler and has access to all hunchentoot
dynamic variables. The default implementation executes a user
action (if any) and renders the root widget wrapped in HTML
provided by 'render-page'. If the request is an AJAX request, only the
dirty widgets are rendered into a JSON data structure. It also invokes
user supplied 'reblocks/session:init' method on the first request that has no
session setup.

'handle-request' immediately returns '+http-not-found+' if it
sees a mime type on the script name (it doesn't handle what could be
files because these mess with callback functions and break some
widgets that depend on them).

Additionally, on the first request a session is created and a client
is forced to redirect. At this point if the cookie is sent, session
information is removed from the URL, otherwise the URL is left in
tact. This is done so that session information appears on the URL for
clients that don't support cookies (this way AJAX requests followed by
a refresh will work).

This function also manages lists of callback functions and calls them
at different points before and after request. See 'request-hook'.

Override this method (along with :before and :after specifiers) to
customize behavior."))


(defmethod handle-request :around ((app app))
  "This wrapper sets current application and suppresses error output from Hunchentoot."
  (with-handled-errors
    (let ((*print-pretty* t)
          ;; Hunchentoot already displays warnings into log file, we just suppress output
          (*error-output* (make-string-output-stream)))
      (with-log-unhandled ()
        (let ((response (call-next-method)))
          (typecase response
            (response response)
            (string (make-response response))
            (t (error "Responses of type ~A are not supported"
                      (type-of response)))))))))


;; TODO: make-this method an optional application mixin
(defmethod handle-request :around (app)
  "This wrapper sets a timeout on the request and reports response timings."

  (log:debug "Handling client request for" app)


  ;; TODO: understand how to use it and write a doc.

  (handler-bind ((timeout-error
                   (lambda (c)
                     (declare (ignorable c))
                     ;; TODO: let the user customize this
                     (error "Your request timed out."))))
    ;; TRIVIAL-TIMEOUT seems to be broken on CCL and in yet another way on
    ;; Lispworks. For now let's only enable it on SBCL.
    (#-sbcl progn
     #+sbcl with-timeout #+sbcl (*request-timeout*)
     (unwind-protect
          (let* ((timings nil)
                 (*timing-level* 0)
                 (*timing-report-fn*
                   (lambda (name real cpu)
                     (setf timings (acons name
                                          (list real cpu
                                                *timing-level*)
                                          timings))))
                 (result (timing "handle-request"
                           (call-next-method))))
            (dolist (timing timings)
              (dotimes (i (cadddr timing))
                (write "  " :escape nil))
              (finish-output)
              (format t "~A time (real/cpu): ~F/~F~%" (car timing)
                      (cadr timing) (caddr timing)))
            result)))))


(defgeneric page-not-found-handler (app)
  (:documentation "This function is called when the current widget 
   heirarchy fails to parse a URL.  The default behavior simply sets the 
   404 return code")
  (:method ((app t))
    (declare (ignore app))
    
    (immediate-response "Not found"
                        :code 404)))


(defmethod handle-ajax-request ((app app))
  (log:debug "Handling AJAX request")
  
  (write
   (to-json
    (list :|commands| (get-collected-commands)))
   ;; Seems like a hack, because we have to know implementation details of reblocks/html here.
   ;; TODO: hide implementation details.
   :stream *stream*
   :escape nil))


(defmethod handle-normal-request ((app app))
  ;; we need to render widgets before the boilerplate HTML
  ;; that wraps them in order to collect a list of script and
  ;; stylesheet dependencies.
  (log:debug "Handling normal request")

  ;; TODO: make a macro reblocks/session-lock:with-lock
  (with-lock-held ((get-lock))
    (timing "widget tree rendering"
      (render (reblocks/widgets/root:get app))))

  ;; render page will wrap the HTML already rendered to
  ;; reblocks.html::*stream* with necessary boilerplate HTML
  (timing "page render"
    ;; Here we are using internal symbol, because we don't want to expose
    ;; this method for usage outside of the reblocks.
    (render-page-with-widgets app)))


(defun remove-action-from-uri (uri)
  "Removes the action info from a URI."
  (remove-parameter-from-uri uri *action-string*))


(defun handle-action-if-needed (app)
  (let ((action-name (get-action-name-from-request))
        (action-arguments
          (alist->plist (get-parameters))))

    (when action-name
      (log:debug "Processing action" action-name)

      ;; Remove :action key from action arguments
      (remf action-arguments (make-keyword (string-upcase *action-string*)))
      
      (when (pure-request-p)
        (log:debug "Request is pure, processing will be aborted.")
        ;; TODO: add with-action-hook ()
        (immediate-response
         (eval-action
          app
          action-name
          action-arguments)))

      (timing "action processing (w/ hooks)"
        (reblocks/hooks:with-action-hook (app action-name action-arguments)
          (eval-action
           app
           action-name
           action-arguments)))))


  ;; Remove "action" parameter for the GET parameters
  ;; it it is not an AJAX request
  (when (and (not (ajax-request-p))
             (get-parameter *action-string*))
    
    (let ((url (remove-action-from-uri
                (get-path :with-params t))))
      (log:debug "Redirecting to an URL without action parameter" url)
      (redirect url))))


(defmethod handle-request ((app app))
  (let ((path (get-path)))
    (log:debug "Handling client request" path)

    ;; TODO: write a test
    (when (null (reblocks/widgets/root:get app))
      (log:debug "Initializing session")
      (handler-bind ((error (lambda (c) 
                              (when *backtrace-on-session-init-error*
                                (let ((traceback))
                                  (log:error "Error during session initialization" traceback)))
                              (signal c))))
        (setf (reblocks/widgets/root:get app)
              (reblocks/session:init app))))
    
    ;; TODO: understand why there is coupling with Dialog here and
    ;;       how to move it into the Dialog's code.
    
    ;; (reblocks/hooks:on-session-hook-action
    ;;     update-dialog ()
    ;;   (reblocks::update-dialog-on-request)))

    (with-collected-dependencies
      ;; This variable will be set to HTML string after rendering
      (let ((content nil))

        
        (push-dependencies
         (get-dependencies app))
        
        (handle-action-if-needed app)

        (setf content
              (timing "rendering (w/ hooks)"
                (reblocks/hooks:with-render-hook (app)
                  (with-html-string
                    (if (ajax-request-p)
                        (handle-ajax-request app)
                        (handle-normal-request app))

                    ;; Now we'll add routes for each page dependency.
                    ;; This way, a dependency for widgets, created by action
                    ;; can be served when browser will follow up with next request.
                    (let ((dependencies (get-collected-dependencies)))
                      (log:debug "Collected dependencies"
                                 dependencies)

                      (register-dependencies
                       dependencies))))))

        content))))
