(defpackage #:weblocks/routes
  (:use #:cl)
  (:import-from #:routes)
  (:import-from #:weblocks/error-handler
                #:with-handled-errors)
  (:export
   #:add-route
   #:route
   #:reset-routes
   #:serve
   #:get-route
   #:defroute
   #:add-routes))
(in-package weblocks/routes)


(defclass route (routes:route)
  ((handler :initform nil
            :initarg :handler
            :reader get-handler)
   (content-type :initform "application/json"
                 :initarg :content-type
                 :reader get-content-type))
  (:documentation "Inherit from this class to add a custom routes."))


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


(defun add-routes (app)
  "Inserts all routes bound to the app into the routing table."
  (let ((app-class-name (typecase app
                          (symbol app)
                          (t (type-of app)))))
    (dolist (handler (get app-class-name :route-handlers))
      (add-route (get handler :route)))))


(defun reset-routes ()
  "Resets routes before starting Weblocks server."
  (setf *routes* (make-instance 'routes:mapper)))



(defgeneric serve (route env) 
  (:documentation "Methods should return a list like that:
\(list 200                                 ;; status-code
       \(list :content-type content-type\) ;; headers
       content\)                           ;; content
")
  (:method ((route route) env)
    (let ((handler (get-handler route)))
      (cond
        (handler (let ((response (funcall handler)))
                   (etypecase response
                     (list response)
                     (string (list 200
                                   (list :content-type (get-content-type route))
                                   (list response))))))
        (t (list 400
                 (list :content-type "text/plain")
                 "No handler for this route"))))))


(defmethod serve :around (route env)
  "This method handles unhandled exceptions, because this code is common for all routes."
  (with-handled-errors
    (call-next-method)))


(defmacro defroute ((app route &key (content-type "application/json")) &body body)
  "Defines a handler for a given route. By default route should return
   a serialized JSON:

   (defroute (app /api/data)
       \"{\"my-data\": [1, 2, 3]}\")

   but you can redefine the content type:

   (defroute (app /api/data :content-type \"application/xml\")
       \"<my-data><item>1</item><item>2</item></my-data>\")
   "
  (let* ((uri (string-downcase (symbol-name route)))
         (route-var (gensym "ROUTE")))
    `(progn
       ;; First, we'll define a handler function
       (defun ,route ()
         ,@body)

       (let ((,route-var (make-instance 'route
                                   :template (routes:parse-template ,uri)
                                   :handler ',route
                                   :content-type ,content-type)))
         ;; next, we'll attach a route object to a function name
         (setf (get ',route :route)
               ,route-var)
         ;; and finally, will store this function into the list of
         ;; app's routes.
         ;; Later, these list of routes will be added to the routes mapper
         ;; on app's start.
         ;; We need all of this because routes mapper can be recreated on
         ;; app's restart and we need an other place to store all routes,
         ;; bound to the app.
         (pushnew ',route
                  (get ',app :route-handlers))

         ;; Also, if app is currently active, then we should add this route to
         ;; the active mapping
         (when (weblocks/app:app-active-p ',app)
           (add-route ,route-var))))))
