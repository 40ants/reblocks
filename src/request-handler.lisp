(uiop:define-package #:reblocks/request-handler
  (:use #:cl)
  (:import-from #:reblocks/request
                #:get-path
                #:get-action-name-from-request
                #:get-parameter
                #:get-parameters
                #:pure-request-p
                #:ajax-request-p)
  (:import-from #:reblocks/utils/list
                #:alist->plist)
  (:import-from #:reblocks/page
                #:in-page-context-p
                #:*current-page*
                #:current-page
                #:render-page-with-widgets)
  (:import-from #:reblocks/session)
  (:import-from #:40ants-routes/route)
  (:import-from #:reblocks/routes)
  (:import-from #:reblocks/widget
                #:render)
  (:import-from #:reblocks/html
                #:with-html-string
                #:*stream*)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks/routes/server
                #:register-dependencies)
  (:import-from #:reblocks/page-dependencies
                #:already-loaded-p
                #:with-collected-dependencies
                #:push-dependencies
                #:get-collected-dependencies)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:reblocks/actions
                #:remove-action-from-uri
                #:eval-action)
  (:import-from #:reblocks/commands
                #:get-collected-commands)
  (:import-from #:reblocks/variables
                #:*current-app*
                #:*backtrace-on-session-init-error*
                #:*action-string*)
  (:import-from #:reblocks/utils/timing
                #:*timing-level*
                #:*timing-report-fn*
                #:timing)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:reblocks/response
                #:get-response
                #:response
                #:immediate-response
                #:redirect)
  ;; Just dependencies
  (:import-from #:reblocks/hooks)
  (:import-from #:log)
  (:import-from #:alexandria
                #:curry
                #:make-keyword)
  (:import-from #:40ants-routes/handler
                #:call-handler)
  
  (:export #:page-not-found-handler
           #:*request-timeout*
           #:handle-ajax-request))
(in-package #:reblocks/request-handler)


(defvar *request-timeout* 180
  "Seconds until we abort a request because it took too long.
  This prevents threads from hogging the CPU indefinitely.

  You can set this to NIL to disable timeouts (not recommended).")


(defgeneric page-not-found-handler (server app)
  (:documentation "This function is called when the current widget 
   heirarchy fails to parse a URL.  The default behavior simply sets the 
   404 return code")
  (:method ((server t)
            (app t))
    (immediate-response "Not found"
                        :code 404)))


(defgeneric handle-ajax-request (app)
  (:documentation "This generic function is called when Reblocks processes an action called from Javascript."))


(defmethod handle-ajax-request ((app app))
  (log:debug "Handling AJAX request")
  
  ;; Generate code to embed new dependencies into the page on the fly.
  ;; This render will generate commands to include necessary pieces
  ;; of JS and CSS into the page.
  (mapc #'reblocks/dependencies:render-in-ajax-response
        (get-collected-dependencies))
  
  (let ((commands (get-collected-commands)))
    (write
     (to-json
      (list :|commands| commands))
     ;; Seems like a hack, because we have to know implementation details of reblocks/html here.
     ;; TODO: hide implementation details.
     :stream *stream*
     :escape nil)))


(defun handle-normal-request (app &key page)
  ;; we need to render widgets before the boilerplate HTML
  ;; that wraps them in order to collect a list of script and
  ;; stylesheet dependencies.
  (log:debug "Handling normal request")

  (reblocks/page::with-page-defaults (:page page)
    ;; TODO: make a macro reblocks/session-lock:with-lock
    (timing "widget tree rendering"
      (render (reblocks/page:page-root-widget
               (current-page))))

    ;; render page will wrap the HTML already rendered to
    ;; reblocks.html::*stream* with necessary boilerplate HTML
    (timing "page render"
      ;; Here we are using internal symbol, because we don't want to expose
      ;; this method for usage outside of the reblocks.
      (render-page-with-widgets app))))


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


(defmethod reblocks/routes:serve ((route reblocks/routes::page-route) env)
  "If a page route was found, then it's handler should return a page widget.

   It initializes the session if needed and if no page is matched to the current
   path in the session' cache, calls route's handler to create a page widget.

   Also, this method processes calls to action if it finds an action identifier
   in the URL params.
  "

  (let ((path (get-path))
        (app *current-app*))
    (log:debug "Handling client request" path)

    ;; TODO: write a test
    (unless (reblocks/session:get-value 'initialized)
      (log:debug "Initializing a new session")
      (handler-bind ((error (lambda (c) 
                              (when *backtrace-on-session-init-error*
                                (let ((traceback))
                                  (log:error "Error during session initialization" traceback)))
                              (signal c))))
        (reblocks/session:init-session app)
        (setf (reblocks/session:get-value 'initialized) t)))

    (with-collected-dependencies ()
      ;; This variable will be set to HTML string after rendering
      (multiple-value-bind (_ page-action-bound-to)
          (handle-action-if-needed app)
        (declare (ignore _))
        ;; We need this trick with PROGV to bind *CURRENT-PAGE*
        ;; only if PAGE-ACTION-BOUND-TO is not NIL. Otherwise,
        ;; we should leave *CURRENT-PAGE* be unbound.
        ;; 
        ;; TODO: current code of HANDLE-ACTION-IF-NEEDED
        ;; does not return two values, we need to check
        ;; if we really need this hassle with binding *current-page*:
        (let* ((symbols-to-bind (when page-action-bound-to
                                  (list '*current-page*)))
               (values-to-bind (when page-action-bound-to
                                 (list page-action-bound-to))))
          (progv symbols-to-bind values-to-bind
            ;; We should collect application dependencies here,
            ;; because this way we have a chance to bind the
            ;; to the current-page object, bound to the action.
            (push-dependencies
             (get-dependencies app))


            (reblocks/hooks:with-render-hook (app)
              (with-html-string ()
                (if (ajax-request-p)
                    (handle-ajax-request app)
                    (handle-normal-request app))

                ;; Now we'll add routes for each page dependency.
                ;; This way, a dependency for widgets, created by action
                ;; can be served when browser will follow up with next request.
                (let ((dependencies (get-collected-dependencies)))
                  (log:debug "Collected dependencies"
                             dependencies)

                  (register-dependencies dependencies))))))))))


(defmethod reblocks/routes:serve ((route 40ants-routes/route:route) env)
  "If a generic route was found, then it's handler should return a response of the Clack application."
  (call-handler))
