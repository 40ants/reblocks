(uiop:define-package #:reblocks/session
  (:use #:cl)
  (:import-from #:alexandria
                #:ensure-gethash)
  (:import-from #:log)
  (:import-from #:lack.util)
  (:import-from #:lack.middleware.session.store.memory
                #:memory-store-stash)
  (:import-from #:lack.session.state.cookie)
  (:import-from #:bordeaux-threads
                #:with-lock-held
                #:make-lock)
  (:import-from #:trivial-garbage
                #:make-weak-hash-table)
  (:import-from #:alexandria
                #:ensure-gethash)

  (:export
   #:with-session
   #:delete-value
   #:get-value
   #:gen-id
   #:in-session-p
   #:init
   ;; this function is defined in session-reset.lisp
   ;; to not introduce circular dependencies
   #:reset
   #:expire
   #:get-number-of-sessions
   #:make-session-middleware
   #:get-number-of-anonymous-sessions))
(in-package #:reblocks/session)


(defvar *session* nil
  "Stores current requests's session")

(defvar *env* nil
  "Stores current lack environment to configure session's behaviour.")


(defvar *session-locks* (make-weak-hash-table :test #'eql
                                              :weakness :key)
  "Per-session locks to avoid having unrelated threads
  waiting.")


(defvar *session-lock-table-lock* (make-lock "*session-lock-table-lock*"))


(defun in-session-p ()
  "Checks if session is active and data can be safely retrived or stored."
  (when *session*
    t))


;; previously webapp-session-value
(defun get-value (key &optional (default nil default-given-p))
  "Get a session value from the currently running webapp.
KEY is compared using EQUAL."

  (unless *session*
    (error "Session was not created for this request!"))
  ;; TODO: seems, previously keys were separated for different weblocks apps
  ;;       but I've simplified it for now
  (if default-given-p
      (ensure-gethash key *session* default)
      (gethash key *session*)))


(defun (setf get-value) (value key)
  "Set a session value for the currently running webapp.
KEY is compared using EQUAL."

  (setf (gethash key *session*)
        value))


;; Previously delete-webapp-session-value
(defun delete-value (key)
  "Clear the session value for the currently running app.

   KEY is compared using EQUAL."
  (remhash key *session*))


(defun gen-id (&optional (prefix "dom"))
  "Generates an ID unique accross the session. The generated ID can be
used to create IDs for html elements, widgets, etc."
  (let ((new-widget-id (1+ (or (get-value 'last-unique-id) -1))))
    (setf (get-value 'last-unique-id)
          new-widget-id)
    (apply #'concatenate 'string (mapcar #'princ-to-string (list prefix new-widget-id)))))


(defgeneric init (app)
  (:documentation "A method for this generic function should be defined to initialize application for a new user session.

                   It should return a widget which become a root widget."))


(defun expire ()
  "Deletes current session id for the browser.

   On the next HTTP request a new session will be created."
  (unless (or *session* *env*)
    (error "Expire should be called inside with-session call."))

  (setf (getf (getf *env* :lack.session.options)
              :expire)
        t)
  (values))


(defun get-lock (&optional session)
  (let ((session (or session
                     *session*)))
    (unless session
      (error "There is no current session."))
    
    (with-lock-held (*session-lock-table-lock*)
      (ensure-gethash session
                      *session-locks*
                      (make-lock
                       (format nil "Session lock for session ~S"
                               session))))))


(defun call-with-session-lock (session thunk)
  (with-lock-held ((get-lock session))
    (funcall thunk)))


(defmacro with-session-lock ((&optional session)
                             &body body)
  `(flet ((with-session-lock-thunk ()
            ,@body))
     (declare (dynamic-extent #'with-session-lock-thunk))
     (call-with-session-lock ,session
                             #'with-session-lock-thunk)))


(defun call-with-session (lack-env func)
  (let ((*session* (getf lack-env :lack.session))
        (*env* lack-env))
    (restart-case
        (with-session-lock (*session*)
          (funcall func))
      (reset-session ()
        :report "Reset current Weblocks session and return 500."
        (log:warn "Resetting current session.")
        (clrhash *session*)
        (list 500 nil (list "Please, reload page to start a new session."))))))


(defmacro with-session ((env) &body body)
  "Sets dynamic binding for *session* and *env*"
  `(call-with-session ,env
                      (lambda () ,@body)))


(defvar !get-number-of-sessions nil
  "This variable will contain a function after the session middleware will be created.")

(defvar !get-number-of-anonymous-sessions nil
  "This variable will contain a function after the session middleware will be created.")

(defvar !map-sessions nil
  "This variable will contain a function after the session middleware will be created.")

(defun get-number-of-sessions ()
  (unless !get-number-of-sessions
    (error "Please, call make-session-middleware first."))
  (funcall !get-number-of-sessions))


(defun get-number-of-anonymous-sessions ()
  (unless !get-number-of-anonymous-sessions
    (error "Please, call make-session-middleware first."))
  (funcall !get-number-of-anonymous-sessions))


(defun map-sessions (thunk &rest keys)
  (unless !map-sessions
    (error "Please, call make-session-middleware first."))
  (apply !map-sessions thunk keys))


(defvar *allowed-samesite-policies*
  (list :lax :strict :none))


(defun allowed-samesite-policy-p (value)
  (member value *allowed-samesite-policies*))


(deftype samesite-policy-type ()
  `(and keyword
        (satisfies allowed-samesite-policy-p)))


(defun make-session-middleware (&key (samesite-policy :lax) (public-session-keys (list :pages)))
  ;; We don't want to expose session store as a global variable,
  ;; that is why we use these closures to extract statistics.
  (check-type samesite-policy samesite-policy-type
              (format nil "one of ~{~S~#[~; or ~:;, ~]~}"
                      *allowed-samesite-policies*))
  
  (let* ((store (lack.session.store.memory:make-memory-store))
         (state (lack.session.state.cookie:make-cookie-state :samesite samesite-policy
                                                             ;; This is requirement
                                                             ;; to mark cookie as Secure
                                                             ;; when SameSite is None.
                                                             :secure (eql samesite-policy
                                                                          :none))))
    (flet ((get-number-of-sessions ()
             (let ((hash (memory-store-stash store)))
               (hash-table-count hash)))
           (get-number-of-anonymous-sessions ()
             (let ((hash (memory-store-stash store)))
               (loop for session-hash being the hash-values in hash
                     unless (gethash :user session-hash)
                     summing 1)))
           (map-sessions (thunk &rest keys)
             ;; For security reasons, we allow to access only
             ;; a few keys inside the session
             (loop for key in keys
                   unless (member key public-session-keys)
                   do (error "Unable to access ~A key in the session object."
                             key))
             
             (let ((hash (memory-store-stash store)))
               (loop for session-hash being the hash-values in hash
                     do (with-session-lock (session-hash)
                          (let* ((arguments
                                   (loop for key in keys
                                         collect (gethash key session-hash)))
                                 (results (apply thunk arguments)))
                            (loop for key in keys
                                  for result in results
                                  do (setf (gethash key session-hash)
                                           result)))))))
           (session-middleware (app)
             (funcall (lack.util:find-middleware :session) app
                      :store store
                      :state state)))
      (setf !get-number-of-sessions
            #'get-number-of-sessions)
    
      (setf !get-number-of-anonymous-sessions
            #'get-number-of-anonymous-sessions)
      
      (setf !map-sessions
            #'map-sessions)
    
      #'session-middleware)))
