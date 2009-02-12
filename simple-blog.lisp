
(defpackage #:simple-blog
  (:use :cl :weblocks :cl-who
	:metabang.utilities)
  (:documentation
   "A web application based on Weblocks."))

(in-package :simple-blog)

(export '(start-simple-blog stop-simple-blog))

(defwebapp simple-blog
    :prefix "/"
    :description "simple-blog: An example application"
    :init-user-session 'simple-blog::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :slots ((debug :initform t)))

(defun start-blog (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'simple-blog))

(defun stop-blog ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'simple-blog)
  (stop-weblocks))

