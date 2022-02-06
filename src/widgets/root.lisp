(defpackage #:reblocks/widgets/root
  (:use #:cl)
  (:shadow #:get)
  ;; Just dependencies
  (:import-from #:reblocks/session)
  (:import-from #:alexandria
                #:symbolicate)
  (:export #:get
           #:root-widget-key))
(in-package #:reblocks/widgets/root)


(defgeneric root-widget-key (app)
  (:documentation "Creates a key to store root widget in a session.")
  (:method ((app t))
    (symbolicate "ROOT-WIDGET-"
                 (class-name
                  (class-of app)))))


(defun get (app)
  (reblocks/session:get-value (root-widget-key app)))


(defun (setf get) (value app)
  (setf (reblocks/session:get-value (root-widget-key app))
        value))
