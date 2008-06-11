
(in-package :weblocks)

(export '(do-login authenticatedp logout hash-password
	  *default-login-title* *default-login-failure-error*
	  *authentication-key* default-login-view email password login
	  login-view login-on-login login-quickform authenticated-server-users
	  anonymous-user-count print-user-stats))

;;; A wrapper function to quickly present a login dialog
(defun/cc do-login (on-login &key (view 'default-login-view) (title *default-login-title*))
  (do-dialog title (make-instance 'login :on-login on-login :view view)))

(defun authenticatedp ()
  "Returns nil if the current user is not authenticated. If the user
is authenticated, authentication information stored in the session is
returned."
  (car (multiple-value-list (session-value *authentication-key*))))

(defun logout ()
  "Removes any authentication information from the session."
  (setf (session-value *authentication-key*) nil))

(defun hash-password (password)
  "Returns a one way hash of a plain-text password."
  (hunchentoot::md5-hex (copy-seq password)))

(defparameter *default-login-title* "Please Log In"
  "Default title to be used in 'do-login'.")

(defparameter *default-login-failure-error* "Invalid credentials."
  "Default message in case of login failure.")

(defparameter *authentication-key* 'authentication-object
  "A key used to store the authentication object in the session.")

(defview default-login-view (:type form :persistp nil
			     :buttons '((:submit . "Login") :cancel)
			     :caption "Login"
			     :focusp t)
  (email :requiredp t)
  (password :requiredp t
	    :present-as password
	    :writer (lambda (pwd obj)
		      (setf (slot-value obj 'password)
			    (hash-password pwd)))))

(defwidget login ()
  ((view :accessor login-view
	 :initform 'default-login-view
	 :initarg :view
	 :documentation "A form view containing fields necessary to
	 collect credentials information. By default uses
	 'default-login-view' which requires an email address and a
	 password. Note that default view automatically hashes the
	 incoming password.")
   (on-login :accessor login-on-login
	     :initform nil
	     :initarg :on-login
	     :documentation "A function that must accept two
	     parameters (a login widget object, and a CLOS object that
	     contains information obtained from the user), and return
	     an authentication object if login is successful, or nil
	     if login failed. If the function returns a non-nil value,
	     it will be stored in the session under
	     *authentication-key*. If the function returns nil, it may
	     return a string as a second value that will be used as an
	     error message. Otherwise, *default-login-failure-error*
	     will be used. Note, the object passed to the specified
	     function is generated from the view provided in the
	     'login-view' slot.")
   (quickform :accessor login-quickform
	      :initform nil
	      :initarg :quickform
	      :documentation "A quickform widget used by the login to
	      provide UI."))
  (:documentation "A widget that provides basic login
  functionality. Based on a view to be rendered (or a default of email
  and password), and an authentication function, provides the UI for
  login, and stores login information the session on success."))

(defmethod initialize-instance :after ((obj login) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (login-quickform obj)
	(make-quickform (login-view obj)
			:on-success (lambda (w o)
				      (declare (ignore w o))
				      (answer obj (authenticatedp)))
			:on-cancel (lambda (w)
				     (declare (ignore w))
				     (answer obj))
			:satisfies (lambda (w o)
				     (declare (ignore w))
				     (multiple-value-bind (success error)
					 (funcall (login-on-login obj) obj o)
				       (if success
					   (setf (session-value *authentication-key*) success)
					   (values nil
						   (list
						    (cons nil (or error *default-login-failure-error*)))))))
			:answerp nil)))

(defmethod render-widget-body ((obj login) &rest args)
  (declare (ignore args))
  (render-widget (login-quickform obj)))

(defun authenticated-server-users ()
  "Returns authentication information found in each session on the
server. Sessions without authentication information are ignored."
  (remove nil
	  (mapcar (lambda (session)
		    (car (multiple-value-list (session-value *authentication-key* session))))
		  (active-sessions))))

(defun anonymous-user-count ()
  "Returns the number of anonymous users on the server."
  (- (length (active-sessions))
     (length (authenticated-server-users))))

(defun print-user-stats (&optional (stream t))
  "Prints user statistics to an optionally specified stream. If the
stream isn't specified, prints to standard output."
  (format stream "Total Users On Server: ~A (Authenticated: ~A, Anonymous: ~A)~%" 
	  (length (active-sessions))
	  (length (authenticated-server-users))
	  (anonymous-user-count)))
