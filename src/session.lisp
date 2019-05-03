(defpackage #:weblocks/session
  (:use #:cl)
  (:import-from #:alexandria
                #:ensure-gethash)
  (:import-from #:lack.middleware.session.store.memory
                #:memory-store-stash)
  (:export
   #:with-session
   #:delete-value
   #:get-value
   #:set-value
   #:gen-id
   #:in-session-p
   #:init
   #:get-session-id
   #:expire
   #:get-number-of-sessions
   #:make-session-middleware
   #:get-number-of-anonymous-sessions))
(in-package weblocks/session)


(defvar *session* nil
  "Stores current requests's session")

(defvar *env* nil
  "Stores current lack environment to configure session's behaviour.")


(defun in-session-p ()
  "Checks if session is active and data can be safely retrived or stored."
  (when *session*
    t))


;; previously webapp-session-value
(defun get-value (key &optional default)
  "Get a session value from the currently running webapp.
KEY is compared using EQUAL.

It was made as a macro to not evaluate 'default' on each call."

  (unless *session*
    (error "Session was not created for this request!"))
  ;; TODO: seems, previously keys were separated for different weblocks apps
  ;;       but I've simplified it for now
  
  (ensure-gethash key *session* default))


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
  (:documentation "This method should be defined for weblocks application.
                   it should return a widget which become a root widget."))

(defun get-session-id ()
  "Returns current session id or signals an error if no current session."
  ;; TODO: see if a id can be extracted from sesion
  *session*)


(defun expire ()
  "Deletes current session."
  (unless (or *session* *env*)
    (error "Expire should be called inside with-session call."))

  (setf (getf (getf *env* :lack.session.options)
              :expire)
        t)
  (values))


(defmacro with-session ((env) &body body)
  "Sets dynamic binding for *session* and *env*"
  `(let ((*session* (getf ,env :lack.session))
         (*env* ,env))
     ,@body))


(defvar !get-number-of-sessions nil
  "This variable will contain a function after the session middleware will be created.")

(defvar !get-number-of-anonymous-sessions nil
  "This variable will contain a function after the session middleware will be created.")

(defun get-number-of-sessions ()
  (unless !get-number-of-sessions
    (error "Please, call make-session-middleware first."))
  (funcall !get-number-of-sessions))


(defun get-number-of-anonymous-sessions ()
  (unless !get-number-of-anonymous-sessions
    (error "Please, call make-session-middleware first."))
  (funcall !get-number-of-anonymous-sessions))


(defun make-session-middleware ()
  ;; We don't want to expose session store as a global variable,
  ;; that is why we use these closures to extract statistics.
  (let* ((store (lack.session.store.memory:make-memory-store)))
    (setf !get-number-of-sessions
          (lambda ()
            (let ((hash (memory-store-stash store)))
              (hash-table-count hash))))
    
    (setf !get-number-of-anonymous-sessions
          (lambda ()
            (let ((hash (memory-store-stash store)))
              (loop for session-hash being the hash-values in hash
                    unless (gethash :user session-hash)
                      summing 1))))
    
    (lambda (app)
      (funcall (lack.util:find-middleware :session) app
               :store store))))
