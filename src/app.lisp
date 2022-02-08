(uiop:define-package #:reblocks/app
  (:use #:cl
        #:f-underscore)
  (:import-from #:cl-ppcre)
  (:import-from #:reblocks/app-mop
                #:get-autostarting-apps
                #:get-registered-apps
                #:app-class
                #:webapp-class-home-package)
  (:import-from #:reblocks/js/base
                #:make-js-backend)
  (:import-from #:reblocks/utils/string
                #:attributize-name
                #:remove-spurious-slashes
                #:strip-trailing-slashes
                #:ensure-starts-with-slash)
  (:import-from #:reblocks/utils/list
                #:remove-keyword-parameters)
  (:import-from #:reblocks/variables
                #:*current-app*)
  (:export #:defapp
           #:app
           #:get-autostarting-apps
           #:get-registered-apps
           #:get-prefix
           #:get-current
           #:with-app
           #:initialize-webapp))
(in-package #:reblocks/app)


(defclass app ()
  ((name :type (or symbol string)
         :accessor reblocks-webapp-name
         :initarg :name)
   (description :type (or null string)
                :accessor reblocks-webapp-description
                :initarg :description 
                :initform nil 
                :documentation "The name of the application.  This slot will be used 
                   by 'application-page-title' to generate the default title for each page.")
   (js-backend :accessor get-js-backend 
               :initarg :js-backend
               :initform :jquery
               :documentation "Specify a javascript backend for
               framework (default is :jquery)")
   (hostnames :type list
              :reader reblocks-webapp-hostnames
              :initarg :hostnames
              :initform nil
              :documentation "The hostnames (a list of strings) reserved for this webapp.
              See section 14.32 of RFC 2616.
         
              Example: '(\"foo.com\" \"www.foo.com\" \"shoo.bar.org\")
         
              Wildcard patterns are also allowed. Example: '(\"foo.*.com\")
         
              If NIL (the default), don't care about the hostname at all.
         
              TODO: support regex matching here.")
   (prefix :type string
           :reader get-prefix
           :initarg :prefix
           :documentation "The subtree of the URI space at this site that belongs to
           the webapp.")
   (session-key :type symbol :accessor reblocks-webapp-session-key :initarg :session-key)
   (debug :accessor reblocks-webapp-debug :initarg :debug :initform nil 
          :documentation "Responsible for debug mode, use WEBAPP-DEBUG function for getting slot value"))
  (:metaclass app-class)
  (:documentation 
   "A class that encapsulates a unique web application and all relevant rnesources.
A webapp is a unique set of dependencies and information that can be enabled or
disabled independently of others.  Multiple webapps can be active concurrently 
and incoming connections are dispatched to the root of the webapp according to a 
prefix parameter that defines the URLs parsed by that webapp.  The webapp does 
not see the prefix parameter in URLs that are provided to it.  You can, for 
instance, have different sites (e.g. mobile vs. desktop) with vastly different 
layout and dependencies running on the same server."))


;; Slash-normalizing accessors
;;
;; We use a "transform on write" approach for two reasons:
;;
;; 1. Sane values in slots all the time (except when someone messes around
;;    with SLOT-VALUE)
;;
;; 2. Increased performance
;;
(defmethod (setf get-prefix) (prefix (app app))
  "Set the prefix of the webapp. Ensures normalization by removing trailing slash."
  (unless (string= prefix "/") ;; XXX multiple slashes?
    (setf (slot-value app 'prefix)
          (strip-trailing-slashes prefix))))


;; abstraction macro
(defmacro defapp (name
                  &rest initargs
                  &key
                    prefix
                    subclasses
                    slots
                    description
                    documentation
                    (autostart t)
                  &allow-other-keys)
  "This macro defines the key parameters for a stand alone web application.  
It defines both a class with name 'name' and registers an instance of that class.
It also instantiates a defvar with an instance of this class.  This is intended
to be the primary way a web application is defined.

PREFIX - an URI from where this app should be available on the server. Read more
about this in the REBLOCKS/DOC/ROUTING:@ROUTING section.

SUBCLASSES - if you want to inherit subclass behavior from other webapps, you
can.  It's not likely to be needed much

SLOTS - webapps are class so slots are a list of definitions just as in defclass,
but as slots are likely to be rare on webapps, we make this a keyword argument.

All of the following, when present, are passed through as additional
initargs:

NAME - instantiates a username (and the default title for) a webapp.  use this
name to get and delete this webapp.  Multiple instances of a webapp class can
co-exist, so long as they have different prefixes

DESCRIPTION - A description of the application for the title page

DOCUMENTATION - Content of this argument will be added as (:documentation ...) form
to the class definition.

AUTOSTART - Whether this webapp is started automatically when start-reblocks is
called (primarily for backward compatibility"
  (declare (ignore prefix description))
  
  (let ((default-initargs
          (remove-keyword-parameters initargs
                                     :subclasses
                                     :slots
                                     :autostart
                                     :documentation))
        (documentation (when documentation
                         (list (list :documentation documentation)))))
    `(progn
       (defclass ,name (,@subclasses app)
         ,slots
         ,@documentation
         (:autostart . ,autostart)
         (:default-initargs ,@default-initargs)
         (:metaclass app-class)))))


(defmethod initialize-instance :after
    ((self app) &rest initargs)
  "Add some defaults to the slots."
  (declare (ignorable initargs))

  ;; Make an instance of js backend class from
  ;; a keyword name
  (setf (get-js-backend self)
        (make-js-backend
         (get-js-backend self)))
  
  
  ;; TODO: refactor this mess
  (macrolet ((slot-default (name initform)
               `(unless (slot-boundp self ',name)
                  (setf (slot-value self ',name) ,initform))))
    ;; special handling for prefix slots since initargs
    ;; bypass the normalizing axr
    (when (slot-boundp self 'prefix)
      (setf (get-prefix self)
            (slot-value self 'prefix)))

    (let ((class-name (class-name (class-of self))))
      (slot-default session-key class-name)
      (slot-default name (attributize-name class-name))
      (slot-default prefix
                    (concatenate 'string "/" (attributize-name class-name))))

    (setf (get-prefix self)
          (ensure-starts-with-slash (get-prefix self)))))


(defun find-app-by-name (active-apps name &key (signal-error t))
  "Get a running web application"
  (let ((app (find (if (symbolp name) (attributize-name name) name)
                   active-apps
                   :key #'reblocks-webapp-name :test #'equal)))
    (when (and (null app)
               signal-error)
      (error "Argument ~a is not a running reblocks application." name))
    app))


(defun check-if-valid-class-name (name)
  "Ensure that the we have a valid webapp class"
  (unless (find-class name nil)
    (error "~a is not a valid reblocks application class." name)))

(defun sort-webapps (webapps) 
  (let* ((webapps-sorted-by-prefix (sort webapps #'string> :key #'get-prefix))
         (webapps-sorted-by-hostname-and-prefix (stable-sort
                                                  webapps-sorted-by-prefix
                                                  (lambda (x y)
                                                    (and x (null y)))
                                                  :key #'reblocks-webapp-hostnames)))
    webapps-sorted-by-hostname-and-prefix))


(defgeneric initialize-webapp (app)
  (:documentation "A protocol for performing any special initialization on the creation of a webapp object.")
  (:method ((app t)) nil))


(defgeneric finalize-webapp (app)
  (:documentation "Called when the app has been pulled off the running list to perform any 
   webapp specific cleanup")
  (:method ((app t)) nil))


;;; Convenience accessors
;;; These procedures are relative to the current request's selected webapp


(defun call-in-webapp (app proc)
  "Helper for `in-webapp'."
  (let ((*current-app* app))
    (funcall proc)))


(defmacro with-app (app &body forms)
  "Bind variables that are both webapp-specific, or applicable to just
this app, and webapp-general, or not particular to some request to
this app, with regard to WEBAPP."
  `(call-in-webapp ,app (f0 . ,forms)))


(defun webapp-name (&optional (app *current-app*))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (reblocks-webapp-name app))

(defun webapp-description (&optional (app *current-app*))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (reblocks-webapp-description app))

(defun webapp-hostnames (&optional (app *current-app*))
  "Returns the hostnames this application will serve requests for."
  (reblocks-webapp-hostnames app))


(defun hostname-match-p (pattern hostname)
  (values (cl-ppcre:scan-to-strings
           (cl-ppcre:regex-replace-all "\\*" pattern ".*")
           hostname)))

(defun app-serves-hostname-p (app hostname)
  "Does APP serve requests for HOSTNAME?"
  (let ((hostname (car (cl-ppcre:split ":" hostname)))) ; ignore port
  (or (null (webapp-hostnames app))
        (member hostname (webapp-hostnames app) :test #'equalp)
        (loop for pattern in (webapp-hostnames app)
              thereis (hostname-match-p pattern hostname)))))


(defun webapp-debug (&optional (app *current-app*))
  "Whether APP is in debug mode."
  (reblocks-webapp-debug app))


(defun get-current ()
  *current-app*)
