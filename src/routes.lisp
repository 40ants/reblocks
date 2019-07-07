(defpackage #:weblocks/routes
  (:use #:cl)
  (:import-from #:routes
                #:route)
  (:import-from #:weblocks/error-handler
                #:with-handled-errors)
  (:export
   #:add-route
   #:route
   #:reset-routes
   #:serve
   #:get-route))
(in-package weblocks/routes)


(defvar *routes* (make-instance 'routes:mapper)
  "We will store mapping from URL to dependency here.")


(defun get-route (path)
  "Returns a route, matched on given path.
   If none matched, then returns nil.
 
   Path should be a string."
  (check-type path string)
  (routes:match *routes* path))


(defun add-route (route)
  "Inserts a new route into the routing table."
  (unless (routes:match *routes* route)
    (routes:connect *routes* route)))


(defun reset-routes ()
  "Resets routes before starting Weblocks server."
  (setf *routes* (make-instance 'routes:mapper)))



(defgeneric serve (route env) 
  (:documentation "Methods should return a list like that:
\(list 200                                 ;; status-code
       \(list :content-type content-type\) ;; headers
       content\)                           ;; content
"))


(defmethod serve :around (route env)
  "This method handles unhandled exceptions, because this code is common for all routes."
  (with-handled-errors
    (call-next-method)))
