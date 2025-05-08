(uiop:define-package #:reblocks/server
  (:use #:cl)
  (:import-from #:40ants-routes/with-url
                #:with-partially-matched-url
                #:with-url)
  (:import-from #:reblocks/app
                #:app
                #:routes-app
                #:app-routes
                #:with-app)
  (:import-from #:reblocks/widget
                #:widget)
  (:import-from #:reblocks/page-dependencies
                #:get-collected-dependencies
                #:with-collected-dependencies)
  (:import-from #:reblocks/session
                #:make-session-middleware
                #:with-session)
  (:import-from #:reblocks/hooks
                #:with-render-hook
                #:prepare-hooks)
  (:import-from #:reblocks/routes
                #:find-route-by-class
                #:object-routes
                #:server-routes
                #:route
                #:get-route
                #:add-route
                #:add-routes)
  (:import-from #:reblocks/app
                #:get-prefix
                #:%reblocks-webapp-name
                #:get-autostarting-apps)
  (:import-from #:reblocks/request
                #:with-request)
  (:import-from #:reblocks/response
                #:not-found-error-app
                #:not-found-error-widget
                #:not-found-error
                #:with-response
                #:get-code
                #:get-headers
                #:get-content)
  (:import-from #:lack/request
                #:make-request)
  (:import-from #:clack
                #:clackup)
  (:import-from #:reblocks/request-handler
                #:handle-normal-request)
  (:import-from #:cl-strings
                #:starts-with)
  ;; Just dependencies
  (:import-from #:reblocks/debug)
  (:import-from #:log)
  (:import-from #:reblocks/welcome/app
                #:welcome-screen-app)
  (:import-from #:alexandria
                #:compose)
  (:import-from #:reblocks/utils/list
                #:insert-after
                #:insert-at)
  (:import-from #:reblocks/page
                #:page
                #:ensure-pages-cleaner-is-running)
  (:import-from #:40ants-routes/routes)
  (:import-from #:reblocks/variables
                #:*default-request-timeout*
                #:*current-app*
                #:*server*)
  (:import-from #:40ants-routes/matched-route
                #:original-route
                #:matched-route-p)
  (:import-from #:reblocks/error-handler
                #:with-immediate-response-handler
                #:with-handled-errors)
  (:import-from #:40ants-routes/route
                #:current-route-p
                #:current-route)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/routes/server
                #:register-dependencies)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:local-time
                #:adjust-timestamp!
                #:now)
  (:import-from #:40ants-routes/defroutes
                #:include)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:reblocks/deadline
                #:with-deadline)
  
  (:export
   #:handle-http-request
   #:stop
   #:start
   #:serve-static-file
   #:servers
   #:running-p
   #:*default-samesite-policy*
   #:server
   #:insert-middleware
   #:make-middlewares
   #:get-interface
   #:get-port
   #:get-server-type
   #:request-timeout
   #:server-apps))
(in-package #:reblocks/server)


(defvar *servers* (make-hash-table :test 'equal))


(defvar *clack-output* nil
  "Here we'll store all output from the Clack, because we don't want it to mix with reblocks own output.")


(defun make-initial-server-routes ()
  (40ants-routes/routes:routes ("server" :routes-class server-routes)))


(defclass server ()
  ((port :type integer
         :initarg :port
         :reader get-port)
   (interface :type string
              :initarg :interface
              :reader get-interface)
   (server-type :initarg :server-type
                :reader get-server-type)
   (handler :initform nil
            :accessor %get-handler)
   (routes :initform (make-initial-server-routes)
     :reader object-routes)
   (apps :initform nil
         :reader server-apps)
   (request-timeout
    :initarg :request-timeout
    :type (or null integer)
    :initform *default-request-timeout*
    :reader request-timeout
    :documentation
    "Seconds until we abort a request because it took too long.
     This prevents threads from hogging the CPU indefinitely.

     You can set this to NIL to disable timeouts (not recommended)."))
  (:documentation "Base class for all Reblocks servers. Redefine it if you want to add additional HTTP midlewares, etc."))


(defvar *default-samesite-policy* :lax
  "Default value for SameSite header.

   You will find more at [Mozilla docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite).")


(defgeneric make-middlewares (server &key samesite-policy)
  (:documentation "Returns an alist where keys are keywords and values are Lack middlewares or apps.

                   Default primary method returns alist with two keys :SESSION and :APP, where
                   :SESSION is a middleware and :APP is the main application.

                   To modify middlewares list, define method for a server subclass
                   and use INSERT-MIDDLEWARE function on the results of CALL-NEXT-METHOD.

                   SAMESITE-POLICY argument if given, should be a keyword. It's semantic
                   is described at [Mozilla docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite).
                   It's default value is taken from *DEFAULT-SAMESITE-POLICY* variable.

                   Here is an example, how this generic-function can be used to run
                   a HTTP API on URLs with path starting from /api/:

                   ```
                   (defmethod reblocks/server:make-middlewares ((server ultralisp-server) &rest rest)
                     (declare (ignore rest))
  
                     (flet ((test-app (env)
                              (declare (ignore env))
                              '(200
                                (:content-type \"text/plain\")
                                (\"Hello, World\"))))
                       (reblocks/server:insert-middleware
                        (call-next-method)
                        (cons
                         :some-api
                         (lambda (app)
                           (funcall (lack.util:find-middleware :mount)
                                    app
                                    \"/api/\"
                                    #'test-app)))
                        :before :app)))
                   ```

")
  (:method ((server server) &key (samesite-policy *default-samesite-policy*))
    (list (cons :session
                (make-session-middleware :samesite-policy samesite-policy))
          (cons :app
                (lambda (env)
                  (handle-http-request server env))))))


(defun insert-middleware (layers new-layer &key before after)
  "Returns a new stack of layers inserting NEW-LAYER before or after a layer with given name.

   You should not give both BEFORE and AFTER arguments. If layer with given name was not found,
   the function will signal error.

   Original alist LAYERS is not modified."
  (check-type new-layer cons)
  (check-type (car new-layer) keyword)
  (check-type before (or null keyword))
  (check-type after (or null keyword))
  
  (when (and before after)
    (error "Please specify either BEFORE nor AFTER keyword."))
  
  (unless (or before after)
    (error "Please specify BEFORE or AFTER keyword."))

  (flet ((search-position (key)
           (or (position key layers :key #'car)
               (error "Layer ~A was not found among ~{~A~^, ~}."
                      key
                      (mapcar #'car layers)))))
    (if before
        (insert-at new-layer
                   (copy-list layers)
                   (search-position before))
        (insert-after new-layer
                      (copy-list layers)
                      (search-position after)))))


(defgeneric handle-http-request (server env)
  (:documentation "Handles HTTP request, passed by Clack"))


(defun make-server (&key
                    (port 8080)
                    (interface "localhost")
                    (server-type :hunchentoot)
                    (server-class 'server)
                    (request-timeout *default-request-timeout*))
  "Makes a webserver instance.
Make instance, then start it with ``start`` method."
  (make-instance server-class
                 :port port
                 :interface interface
                 :server-type server-type
                 :request-timeout request-timeout))


(defmethod handle-http-request :around ((server server) env)
  (log4cl-extras/error:with-log-unhandled ()
    (let ((*server* server))
      (let ((result 
              (with-session (env)
                (with-request ((make-request env))
                  (with-response ()
                    (call-next-method))))))
        result))))


(-> make-404-page ((or null app)
                   string
                   widget)
    (values string &optional))

(defun make-404-page (app url-path error-widget)
  (cond
    (app
     (with-app (app)
       (with-collected-dependencies ()
         (with-render-hook (app)
           (with-html-string ()
             (handle-normal-request app
                                    :page
                                    ;; Application might define a common constructor
                                    ;; for all pages, to add such common elements like
                                    ;; header and footer. Here we need to apply this
                                    ;; application's handler:
                                    (let ((wrapped-result
                                            (funcall (reblocks/app::page-constructor app)
                                                     error-widget)))
                                      ;; TODO: probably extract common error page
                                      ;; making code with error-handler.lisp:
                                      (make-instance 'page
                                                     :root-widget wrapped-result
                                                     :path url-path
                                                     :expire-at (adjust-timestamp!
                                                                    (now)
                                                                  ;; It is no make sence to cache 404 pages
                                                                  (:offset :sec 5)))))
                  
             (register-dependencies
              (append (get-dependencies app)
                      (get-collected-dependencies))))))))
    (t
     (error "No app during 404 error handling"))))


(defmethod handle-http-request ((server server) env)
  "Reblocks HTTP dispatcher.
This function serves all started applications and their static files."

  (let ((url-path (getf env :path-info))
        ;; This "hack" is needed to allow widgets to change *random-state*
        ;; and don't interfere with other threads and requests
        (*random-state* *random-state*))
    ;; Dynamic hook :handle-http-request makes possible to write
    ;; some sort of middlewares, which change *request* and *session*
    ;; variables.
    (prepare-hooks
      (reblocks/hooks:with-handle-http-request-hook (env)
        (with-immediate-response-handler () 
          (with-partially-matched-url ((object-routes server)
                                       url-path)
            (handler-case 
                (let* ((app-route (find-route-by-class 'reblocks/app::app-routes))
                       (app (when app-route
                              (routes-app app-route))))
                  ;; In some cases app route might be not found and
                  ;; we need to handle this case. This might be if all
                  ;; apps are included into the server's routes using prefixes
                  ;; instead of /. In this case if route does not start with
                  ;; any app's prefix, APP will be NIL.
                  (macrolet ((with-optional-app ((app) &body body)
                               `(flet ((with-optional-app-thunk ()
                                         ,@body))
                                  (declare (dynamic-extent #'with-optional-app-thunk))
                                  (cond
                                    (,app (with-app (,app)
                                            (funcall #'with-optional-app-thunk)))
                                    (t
                                     (funcall #'with-optional-app-thunk))))))
                    (with-optional-app (app)
                      (cond
                        ((current-route-p)
                         (let* ((route (progn
                                         (unless (matched-route-p (current-route))
                                           (error "Unexpected class of route was found: ~S"
                                                  (class-of (current-route))))
                                         (original-route (current-route)))))
                           (log:debug "Processing request to" url-path)
                           
                           ;; This wrapper calls an interactive debugger
                           ;; if it is available or shows an error page.
                           (let ((resp (with-handled-errors ()
                                         (with-deadline (:seconds (request-timeout server))
                                           (reblocks/routes:serve route env)))))
                             (values resp))))
                        ;; No page was found matching the route
                        (t
                         (not-found-error (fmt "Route ~S not found."
                                               url-path)))))))
              (not-found-error (c)
                (list 404
                      (list :content-type "text/html")
                      (list
                       (make-404-page (not-found-error-app c)
                                      url-path
                                      (not-found-error-widget c))))))))))))


(defun start-server (server &key debug
                              (samesite-policy *default-samesite-policy*))
  "Starts a Clack webserver, returns this server as result.

If server is already started, then logs a warning and does nothing."

  (check-type server server)
  
  (cond ((%get-handler server)
         (log:warn "Webserver already started"))

        ;; Otherwise, starting a server
        (t
         (let* ((port (get-port server))
                (interface (get-interface server))
                (middlewares
                  (mapcar #'cdr
                          (make-middlewares server :samesite-policy samesite-policy)))
                (app
                  (reduce #'funcall
                          (remove-if #'null
                                     (butlast middlewares))
                          :initial-value (car (last middlewares))
                          :from-end t)))
           (log:info "Starting webserver on" interface port debug)

           ;; Suppressing output to stdout, because Clack writes message
           ;; about started server and we want to write into a log instead.
           (setf *clack-output* (make-string-output-stream))
           (let ((*standard-output* *clack-output*))
             (setf (%get-handler server)
                   (clackup app
                            :address interface
                            :server (get-server-type server)
                            :port port
                            :debug debug
                            ;; Here we are turning off :backtrace middleware from the Lack
                            ;; and probably all other middlewares which may become "default"
                            ;; in future.
                            :use-default-middlewares nil))))))
  server)


(defun stop-server (server)
  "Stops a Clack server, but does not deactivates active applications,
   use `stop' function for that."

  (if (%get-handler server)
      (progn (log:info "Stopping server" server)
             (clack:stop (%get-handler server))
             (setf (%get-handler server)
                   nil))
      (log:warn "Server wasn't started"))

  server)


(defun running-p (server)
  "Returns T if server is running and NIL otherwise."
  (when (%get-handler server)
    t))


(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "~A:~A "
            (get-interface server)
            (get-port server))
    (if (server-apps server)
        (format stream "(~{~A~^, ~})"
                (server-apps server))
        (format stream "(no apps)"))
    (format stream "~A"
            (if (running-p server)
                " running"
                " stopped"))))


(defun find-server (interface port)
  (gethash (cons interface port)
           *servers*))


(defun servers (&optional interface port)
  "Returns a list of Reblocks servers."
  (check-type interface (or null string))
  (check-type port (or null integer))
  
  (loop for (server-interface . server-port) being the hash-keys of *servers*
          using (hash-value server)
        when (cond
               ((and interface port)
                (and
                 (string-equal server-interface interface)
                 (= server-port port)))
               (interface
                (string-equal server-interface interface))
               (t t))
          collect server))


(defun start (&key
              (debug t)
              (port 8080)
              (interface "localhost")
              (server-type :hunchentoot)
              (samesite-policy :lax)
              apps
              (server-class 'server)
              (request-timeout *default-request-timeout*)
              (disable-welcome-app nil))
  "Starts reblocks framework hooked into Clack server.

   Set DEBUG to true in order for error messages and stack traces to be shown
   to the client (note: stack traces are temporarily not available due to changes
   in Hunchentoot 1.0.0).

   Server will start all apps declared having `autostart t` in their definition
   unless APPS argument is provided.

   Sometimes you might want your app respond on /some-uri and / return 404.
   In this case it is useful to set DISABLE-WELCOME-APP argument to T."

  (ensure-pages-cleaner-is-running)

  (let ((server (find-server interface port)))
    (reblocks/hooks:with-start-reblocks-hook ()
      (when (and server
                 (running-p server))
        (restart-case
            (error "Server already running on port ~A" port)
          (continue ()
            :report "Stop the old server and start a new one."
            (stop)
            (setf server nil))))
      
      (setf server
            (make-server :interface interface
                         :port port
                         :server-type server-type
                         :server-class server-class
                         :request-timeout request-timeout))
      (setf (gethash (cons interface port) *servers*)
            server)
      
      (log:info "Starting reblocks" port server-type debug)

      ;; TODO: move these settings to the server level
      (if debug
        (reblocks/debug:on)
        (reblocks/debug:off))

      (start-server server
                    :samesite-policy samesite-policy
                    :debug debug)

      ;; We need to set this bindings to allow apps to use
      ;; REBLOCKS/ROUTES:ADD-ROUTE without given a current
      ;; routes mapping.
      ;; 
      ;; TODO: Not sure if we need this *routes* var:
      (let* (;; (reblocks/routes::*routes* (object-routes server))
             (apps (or (uiop:ensure-list apps)
                       (get-autostarting-apps))))
        (loop for app-class in apps
              do (start-app server app-class))

        ;; If / prefix is not taken, start Welcome Screen app:
        (unless disable-welcome-app
          (loop with found-root = nil
                for app in (server-apps server)
                for prefix = (get-prefix app)
                when (string= prefix "/")
                  do (setf found-root t)
                finally (unless found-root
                          (start-app server 'welcome-screen-app)))))
      
      (values server))))


(defun register-app-routes (server app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
  (log:debug "Registering" app "routes for" server)
  
  ;; (loop with seen = (make-hash-table :test 'equal)
  ;;       for app in (cons app (server-apps server))
  ;;       for prefix = (get-prefix app)
  ;;       when (gethash prefix seen)
  ;;         do (error "Cannot have two defaults dispatchers with prefix \"~A\""
  ;;                   prefix)
  ;;       do (setf (gethash prefix seen)
  ;;                t))
  ;; Also, we should add app's routes to the mapper:
  (when (reblocks/app::app-routes app)
    (40ants-routes/generics:add-route (object-routes server)
                                      (include
                                       (reblocks/app::app-routes app)
                                       :path (get-prefix app))))
  ;; (add-routes app :routes (routes server))
  )


(defun start-app (server app-class &rest app-args)
  (cond
    ((member app-class (mapcar #'reblocks/app::webapp-name (server-apps server)))
     (log:warn "App ~A already started" app-class))
    (t
     (let* ((app (apply #'make-instance app-class app-args))
            (prefix (get-prefix app)))
       (cond
         ((member prefix (mapcar #'get-prefix (server-apps server))
                  :test #'string-equal)
          (loop for other-app in (server-apps server)
                for other-prefix = (get-prefix other-app)
                when (string-equal prefix other-prefix)
                  do (error "Unable to start app ~S because app ~S already uses prefix \"~A\""
                            app-class
                            other-app
                            other-prefix)))
         (t
          (reblocks/app:initialize-webapp app)
          (register-app-routes server app)
          ;; We need to keep apps sorted from longest prefix to shorters,
          ;; to find a correct app to serve the request:
          (setf (slot-value server 'apps)
                (sort (list* app
                             (server-apps server))
                      #'>
                      :key (compose #'length
                                    #'get-prefix)))
          
          (log:debug "App \"~A\" started at http://~A:~A~A"
                     (reblocks/app::webapp-name app)
                     (get-interface server)
                     (get-port server)
                     prefix))))))
  (values))


(defun stop (&optional interface port)
  "Stops Reblocks servers matching given INTERFACE and PORT.

   This function deactivates all applications bound to the server and stopps a Clack server.

   Returns stopped server objects."

  (loop for server in (servers interface port)
        when (running-p server)
          collect
          (reblocks/hooks:with-stop-reblocks-hook ()
            ;; TODO: maybe implement stop app generic function again?
            ;; (loop for app in (server-apps server)
            ;;       do (reblocks/app:stop (%reblocks-webapp-name app)))

            (setf (slot-value server 'app) nil
                  ;; (routes server) (reblocks/routes::make-routes)
                  )
            
            (stop-server server)
            (values server))))

;;;; Static files

(defclass static-route-from-file (route)
  ((path :initarg :path
         :reader get-path)
   (content-type :initarg :content-type
                 :reader get-content-type)))


(defmethod reblocks/routes:serve ((route static-route-from-file) env)
  "Returns a file's content"
  (declare (ignorable env))
  (list 200
        (list :content-type (get-content-type route))
        (get-path route)))
