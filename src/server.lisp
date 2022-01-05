(uiop:define-package #:reblocks/server
  (:use #:cl)
  (:import-from #:routes)
  (:import-from #:reblocks/app
                #:with-app)
  (:import-from #:reblocks/session
                #:make-session-middleware
                #:with-session)
  (:import-from #:reblocks/hooks
                #:prepare-hooks)
  (:import-from #:reblocks/routes
                #:route
                #:get-route
                #:add-route
                #:add-routes)
  (:import-from #:reblocks/app
                #:get-prefix
                #:app-serves-hostname-p
                #:reblocks-webapp-name
                #:get-autostarting-apps)
  (:import-from #:reblocks/request
                #:with-request)
  (:import-from #:reblocks/response
                #:get-code
                #:get-headers
                #:get-content)
  (:import-from #:reblocks/request-handler
                #:handle-request)
    
  (:import-from #:lack.request
                #:make-request)
  (:import-from #:lack
                #:builder)
  (:import-from #:clack
                #:clackup)
  (:import-from #:cl-strings
                #:starts-with)
  ;; Just dependencies
  (:import-from #:reblocks/debug)
  (:import-from #:log)
  
  (:export ;; #:get-server-type
   ;; #:get-port
   ;; #:make-server
   ;; #:handle-http-request
   #:stop
   #:start
   #:serve-static-file
   #:servers
   #:running-p))
(in-package #:reblocks/server)


(defvar *server*)
(setf (documentation '*server* 'variable)
      "Will be bound to a server currently processing the request.")

(defvar *servers* (make-hash-table :test 'equal))


(defvar *clack-output* nil
  "Here we'll store all output from the Clack, because we don't want it to mix with reblocks own output.")


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
            :accessor get-handler)
   (routes :initform (reblocks/routes::make-routes)
           :accessor routes)
   (apps :initform nil
         :accessor apps)))


(defgeneric handle-http-request (server env)
  (:documentation "Handles HTTP request, passed by Clack"))


(defun make-server (&key
                      (port 8080)
                      (interface "localhost")
                      (server-type :hunchentoot))
  "Makes a webserver instance.
Make instance, then start it with ``start`` method."
  (make-instance 'server
                 :port port
                 :interface interface
                 :server-type server-type))


(defun search-app-for-request-handling (path-info hostname)
  (dolist (app (apps *server*))
    (let ((app-prefix (get-prefix app))
          (app-works-for-this-hostname
            (app-serves-hostname-p app hostname)))
      (log:debug "Searching handler in" app app-prefix)

      (when (and app-works-for-this-hostname
                 (starts-with path-info
                              app-prefix))
        (return-from search-app-for-request-handling
          app)))))


(defun make-response-for-clack (response)
  (etypecase response
    (reblocks/response:response
     (list (get-code response)
           (get-headers response)
           ;; Here we use catch to allow to abort usual response
           ;; processing and to return data immediately
           (list (get-content response))))
    (list response)
    (function response)))


(defmethod handle-http-request :around ((server server) env)
  (log4cl-extras/error:with-log-unhandled ()
    (let ((*server* server)
          (reblocks/routes::*routes* (routes server)))
      (call-next-method))))


(defmethod handle-http-request ((server server) env)
  "Reblocks HTTP dispatcher.
This function serves all started applications and their static files."

  (let (;; This "hack" is needed to allow widgets to change *random-state*
        ;; and don't interfere with other threads and requests
        (*random-state* *random-state*))
    (with-session (env)
      (with-request ((make-request env))
        ;; Dynamic hook :handle-http-request makes possible to write
        ;; some sort of middlewares, which change *request* and *session*
        ;; variables.
        (prepare-hooks
          (reblocks/hooks:with-handle-http-request-hook (env)

            (let* ((path-info (getf env :path-info))
                   (hostname (getf env :server-name))
                   (route (get-route path-info))
                   (app (search-app-for-request-handling path-info hostname)))

              (log:debug "Processing request to" path-info)

              ;; If dependency found, then return it's content along with content-type
              (with-app app
                (make-response-for-clack
                 (cond
                   (route
                    (log:debug "Route was found" route)
                    (reblocks/routes:serve route env))
                   (app
                    (log:debug "App was found" route)
                    (handle-request app))
                   (t
                    (log:error "Application dispatch failed for" path-info)

                    (list 404
                          (list :content-type "text/html")
                          (list (format nil "File \"~A\" was not found.~%"
                                        path-info))))))))))))))


(defun start-server (server &key debug
                              (samesite-policy :lax))
  "Starts a Clack webserver, returns this server as result.

If server is already started, then logs a warning and does nothing."
  
  (cond ((get-handler server)
         (log:warn "Webserver already started"))

        ;; Otherwise, starting a server
        (t
         (let* ((port (get-port server))
                (interface (get-interface server))
                (app (builder
                      (make-session-middleware :samesite-policy samesite-policy)
                      (lambda (env)
                        (handle-http-request server env)
                        ;; Don't remember, why this code was commented
                        ;; TODO: check how 500 errors are handled and may be remove
                        ;;       this code, if everything is handled in some other place.
                        ;; (handler-case ()
                        ;;   (t (condition)
                        ;;     (let* ((traceback (with-output-to-string (stream)
                        ;;                         (trivial-backtrace:print-condition condition stream)))
                        ;;            (condition (describe condition))
                        ;;            (just-traceback (trivial-backtrace:backtrace-string)))
                        ;;       (log:error "Unhandled exception" condition traceback just-traceback))
                        ;;     '(500
                        ;;       ("Content-Type" "text/html")
                        ;;       ("Something went wrong!"))))
                        ))))
           (log:info "Starting webserver on" interface port debug)

           ;; Suppressing output to stdout, because Clack writes message
           ;; about started server and we want to write into a log instead.
           (setf *clack-output* (make-string-output-stream))
           (let ((*standard-output* *clack-output*))
             (setf (get-handler server)
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

  (if (get-handler server)
      (progn (log:info "Stopping server" server)
             (clack:stop (get-handler server))
             (setf (get-handler server)
                   nil))
      (log:warn "Server wasn't started"))

  server)


(defun running-p (server)
  "Returns T if server is running and NIL otherwise."
  (when (get-handler server)
    t))


(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "~A:~A "
            (get-interface server)
            (get-port server))
    (if (apps server)
        (format stream "(~{~A~^, ~})"
                (apps server))
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


(defun start (&key (debug t)
                (port 8080)
                (interface "localhost")
                (server-type :hunchentoot)
                (samesite-policy :lax)
                apps)
  "Starts reblocks framework hooked into Clack server.

   Set DEBUG to true in order for error messages and stack traces to be shown
   to the client (note: stack traces are temporarily not available due to changes
   in Hunchentoot 1.0.0).

   Server will start all apps declared having `autostart t` in their definition
   unless APPS argument is provided."

  (let ((server (find-server interface port)))
    (reblocks/hooks:with-start-reblocks-hook ()
      (cond
        (server
         (restart-case
             (error "Server already running on port ~A" port)
           (continue ()
             :report "Stop the old server and start a new one."
             (stop)
             (setf (routes server)
                   (reblocks/routes::make-routes)))))
        (t
         (setf server
               (make-server :interface interface
                            :port port
                            :server-type server-type))
         (setf (gethash (cons interface port) *servers*)
               server)))
      
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
      (let ((reblocks/routes::*routes* (routes server)))
        (loop for app-class in (uiop:ensure-list
                                (or apps
                                    (get-autostarting-apps)))
              do (start-app server app-class)))
      
      (values server))))


(defun register-app-routes (server app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
  (log:debug "Registering" app "routes for" server)
  
  (loop with seen = (make-hash-table :test 'equal)
        for app in (cons app (apps server))
        for prefix = (get-prefix app)
        when (gethash prefix seen)
          do (error "Cannot have two defaults dispatchers with prefix \"~A\""
                    prefix)
        do (setf (gethash prefix seen)
                 t))
  ;; Also, we should add app's routes to the mapper:
  (add-routes app :routes (routes server)))


(defun start-app (server app-class &rest app-args)
  (cond
    ((member app-class (mapcar #'reblocks/app::webapp-name (apps server)))
     (log:warn "App ~A already started" app-class))
    (t
     (let* ((app (apply #'make-instance app-class app-args))
            (prefix (get-prefix app)))
       (cond
         ((member prefix (mapcar #'get-prefix (apps server))
                  :test #'string-equal)
          (loop for other-app in (apps server)
                for other-prefix = (get-prefix other-app)
                when (string-equal prefix other-prefix)
                  do (error "App ~A already uses prefix \"~A\""
                            other-app other-prefix)))
         (t
          (reblocks/app:initialize-webapp app)
          (register-app-routes server app)
          (push app (apps server))
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
            ;; (loop for app in (apps server)
            ;;       do (reblocks/app:stop (reblocks-webapp-name app)))
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


(defgeneric serve-static-file (uri object &key content-type)
  (:documentation "Adds a route to serve given object by static URI."))


(defmethod serve-static-file (uri (path pathname) &key (content-type "text/plain"))
  (let* ((route (make-instance 'static-route-from-file
                               :template (routes:parse-template uri)
                               :path path
                               :content-type content-type)))
    (add-route route)))
