(uiop:define-package #:reblocks/app-mop
  (:use #:cl)
  (:import-from #:metatilities
                #:removef)
  (:shadowing-import-from #:closer-mop
                          #:standard-class
                          #:validate-superclass
                          
                          ;; #:defclass
                          ;; #:defgeneric
                          ;; #:defmethod
                          ;; #:standard-generic-function
                          ;; #:ensure-generic-function
                          ;; #:typep
                          ;; #:subtypep
                          ;; #:standard-method
                          )
  (:export #:get-registered-apps
           #:get-autostarting-apps))
(in-package #:reblocks/app-mop)


;; (eval-when (:compile-toplevel :load-toplevel :execute))
(defvar *registered-apps* nil
  "A list of applications that the system knows about")


(defvar *autostarting-apps* nil
  "A list of webapps to start when start-reblocks is called")


(defclass app-class (standard-class)
  ((home-package
    :accessor webapp-class-home-package :initform *package*
    :documentation "The current package when I was defined."))
  (:documentation "The class of all webapp classes."))


(defmethod validate-superclass ((self app-class) (super standard-class))
  (typep (class-name (class-of super))
         '(member standard-class app-class)))


(defmethod shared-initialize :after
    ((self app-class) slots &key autostart &allow-other-keys)
  (declare (ignore slots))
  (let ((name (class-name self)))
    ;; (pushnew name (symbol-value '*registered-apps*))
    (pushnew name *registered-apps*)
    
    (if autostart
        (pushnew name *autostarting-apps*)
        (removef name *autostarting-apps*))))


(defun get-registered-apps ()
  *registered-apps*)


(defun get-autostarting-apps ()
  *autostarting-apps*)
