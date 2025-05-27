(uiop:define-package #:reblocks/app
  (:use #:cl)
  (:import-from #:reblocks/widgets/default-page
                #:make-default-init-page-widget)
  (:import-from #:40ants-routes/routes)
  (:import-from #:40ants-routes/defroutes)
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
                #:ensure-starts-with-slash)
  (:import-from #:reblocks/variables
                #:*current-app*)
  (:import-from #:alexandria
                #:symbolicate
                #:remove-from-plist)
  (:import-from #:str
                #:ensure-prefix
                #:ensure-suffix)
  (:import-from #:reblocks/routes
                #:page)
  (:export #:defapp
           #:app
           #:get-autostarting-apps
           #:get-registered-apps
           #:get-prefix
           #:get-current
           #:with-app
           #:initialize-webapp
           #:page-constructor
           #:app-routes
           #:routes-app))
(in-package #:reblocks/app)


(defclass app-routes (40ants-routes/routes:routes)
  ((app :initform nil
        :type (or null reblocks/app:app)
        :documentation "App instance will be set during server initialization."
        :accessor routes-app)))


(defclass app ()
  ((name :type (or symbol string)
         :accessor %reblocks-webapp-name
         :initarg :name)
   (description :type (or null string)
                :accessor %reblocks-webapp-description
                :initarg :description 
                :initform nil 
                :documentation "The name of the application.  This slot will be used 
                   by 'application-page-title' to generate the default title for each page.")
   (js-backend :accessor %get-js-backend
               :initarg :js-backend
               :initform :jquery
               :documentation "Specify a javascript backend for
               framework (default is :jquery)")
   (prefix :type string
           :reader get-prefix
           :initarg :prefix
           :documentation "The subtree of the URI space at this site that belongs to
           the webapp.")
   (routes :type (or null
                     app-routes)
     :initarg :routes
     :initform nil
     :reader app-routes)
   (page-constructor :initarg :page-constructor
                     :initform #'identity
                     :type function
                     :reader page-constructor)
   (session-key :initarg :session-key
                :type symbol
                :accessor %reblocks-webapp-session-key)
   (debug :initarg :debug
          :accessor %reblocks-webapp-debug
          :initform nil 
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


;; abstraction macro
(defmacro defapp (name
                  &rest initargs
                  &key
                    prefix
                    routes
                    page-constructor
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

ROUTES - an object of 40ANTS-ROUTES/ROUTES:ROUTES class holding routes relative to the given
prefix.

PAGE-CONSTRUCTOR - a callable of one argument which accepts a widget bound to a route
and returns a root widget for the page.

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
  (declare (ignore prefix description page-constructor))
  
  (let* ((routes-var-name
           (symbolicate "*" name "-ROUTES*"))
         (routes-namespace (string-downcase name))
         (default-initargs
           (remove-from-plist initargs
                              :routes
                              :subclasses
                              :slots
                              :autostart
                              :documentation))
         (routes (or
                  routes
                  '((page ("/")
                      (make-default-init-page-widget)))))
         (documentation (when documentation
                          (list (list :documentation documentation)))))
    `(progn
       (40ants-routes/defroutes:defroutes (,routes-var-name
                                           :namespace ,routes-namespace
                                           :routes-class app-routes)
         ,@routes)
       
       (defclass ,name (,@subclasses app)
         ,slots
         ,@documentation
         (:autostart . ,autostart)
         (:default-initargs :routes ,routes-var-name
                            ,@default-initargs)
         (:metaclass app-class)))))


(defmethod initialize-instance :after
    ((self app) &rest initargs)
  "Add some defaults to the slots."
  (declare (ignorable initargs))

  ;; Make an instance of js backend class from
  ;; a keyword name
  (setf (%get-js-backend self)
        (make-js-backend
         (%get-js-backend self)))
  
  
  ;; TODO: refactor this mess
  (macrolet ((slot-default (name initform)
               `(unless (slot-boundp self ',name)
                  (setf (slot-value self ',name) ,initform))))
    (let ((class-name (class-name (class-of self))))
      (slot-default session-key class-name)
      (slot-default name (attributize-name class-name))
      (slot-default prefix
                    (attributize-name class-name)))

    ;; Normalizing prefix path, to make it work well
    ;; when app's routes are included into other routes.
    (setf (slot-value self 'prefix)
          (ensure-prefix "/"
                         (ensure-suffix "/"
                                        (get-prefix self))))

    ;; This way we'll be able to figure out to which application
    ;; current route belongs to:
    (setf (routes-app
           (slot-value self 'routes))
          self))
  (values))


(defun find-app-by-name (active-apps name &key (signal-error t))
  "Get a running web application"
  (let ((app (find (if (symbolp name) (attributize-name name) name)
                   active-apps
                   :key #'%reblocks-webapp-name :test #'equal)))
    (when (and (null app)
               signal-error)
      (error "Argument ~a is not a running reblocks application." name))
    app))


(defun check-if-valid-class-name (name)
  "Ensure that the we have a valid webapp class"
  (unless (find-class name nil)
    (error "~a is not a valid reblocks application class." name)))


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


(defmacro with-app ((app) &body forms)
  "Bind variable *CURRENT-APP* to the given APP argument."
  ` (flet ((with-app-thunk ()
             ,@forms))
      (declare (dynamic-extent #'with-app-thunk))
      (call-in-webapp ,app #'with-app-thunk)))


(defun webapp-name (&optional (app *current-app*))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (%reblocks-webapp-name app))

(defun webapp-description (&optional (app *current-app*))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (%reblocks-webapp-description app))


(defun webapp-debug (&optional (app *current-app*))
  "Whether APP is in debug mode."
  (%reblocks-webapp-debug app))


(defun get-current ()
  *current-app*)
