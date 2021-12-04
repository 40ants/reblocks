(defpackage #:weblocks/app-actions
  (:use #:cl)
  (:export
   #:get-app-actions
   #:get-action
   #:add-action
   #:remove-action
   #:define-action))
(in-package weblocks/app-actions)

;;
;; Permanent actions
;;

;; TODO: lock-protect this table since users may add actions at runtime
(defvar *apps-actions*
  (make-hash-table)
  "This hash maps app classes to hashes which store application's actions.")

(defun get-app-actions (app)
  (gethash (if (symbolp app)
               app
               (type-of app))
           *apps-actions*))

(defun normalize (action-name)
  (string-downcase
   (if (symbolp action-name)
       (symbol-name action-name)
       action-name)))

(defun get-action (app action-name)
  "Returns the action function associated with this symbol in the current app"
  (let ((action-table (get-app-actions app)))
    (when action-table
      (gethash (normalize action-name)
               action-table))))

(defun add-action (app-class action-name function-or-name)
  "Remove an action from a webapp.  action-name should be a string, or it
   will be converted to one (to work with the macro).  function-or-name is
   a symbol or a function object (valid object for funcall)"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  (macrolet ((action-table (appname)
               `(gethash ,appname *apps-actions*)))
    (unless (action-table app-class)
      (setf (action-table app-class)
            (make-hash-table :test 'equal)))
    (setf (gethash (normalize action-name)
                   (action-table app-class)) 
          function-or-name)))
      
(defun remove-action (app-class action-name)
  "Removes a permanent action from a webapp"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  
  (let ((table (gethash app-class *apps-actions*)))
    (when table
      (remhash action-name table))))


(defmacro define-action (name app-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (find-class app-class))
  `(flet ((,name ,action-params
            ,@body))
     (add-action ',app-class ',name
                 #',name)))

