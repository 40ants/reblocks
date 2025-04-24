(uiop:define-package #:reblocks/routes
  (:use #:cl)
  (:import-from #:40ants-routes/defroutes)
  (:import-from #:40ants-routes/routes)
  (:import-from #:40ants-routes/vars)
  (:import-from #:40ants-routes/included-routes)
  (:export #:route
           #:serve
           #:get-route
           #:defroute
           #:server-routes
           #:static-route
           #:page))
(in-package #:reblocks/routes)


(defclass server-routes (40ants-routes/routes:routes)
  ())


(defclass static-route (40ants-routes/route:route)
  ())


;; (defclass route (routes:route)
;;   ((handler :initform nil
;;             :initarg :handler
;;             :reader get-handler)
;;    (content-type :initform "application/json"
;;                  :initarg :content-type
;;                  :reader get-content-type))
;;   (:documentation "Inherit from this class to add a custom routes."))


;; (defvar *routes*)
;; (setf (documentation '*routes* 'variable)
;;       "This variable will be bound to the server's routes during request processing.
;;        Most functions in the REBLOCKS/ROUTES package will work only when this variable is bound.")


;; (defun make-routes ()
;;   (make-instance 'routes:mapper))


;; (defun get-route (path)
;;   "Returns a route, matched on given path.
;;    If none matched, then returns nil.
 
;;    Path should be a string.

;;    This function could be useful for customizing widget rendering
;;    depending on the URL in the browser."
;;   (check-type path string)
;;   (routes:match *routes* path))


;; (defun add-route (route &key (routes *routes*))
;;   "Inserts a new route into the routing table."
;;   (routes:connect routes route))


(defclass page-route (40ants-routes/route:route)
  ())


(defmacro page ((path &key name title) &body handler-body)
  `(40ants-routes/defroutes:get (,path :name ,name :title ,title :route-class page-route)
     ,@handler-body))


(defgeneric object-routes (obj)
  (:documentation "Returns a routes object bound to a server or application."))


;; (defun add-routes (app &key (routes *routes*))
;;   "Inserts all routes bound to the app into the routing table."
;;   (let ((app-class-name (typecase app
;;                           (symbol app)
;;                           (t (type-of app)))))
;;     (dolist (handler (get app-class-name :route-handlers))
;;       (add-route (get handler :route)
;;                  :routes routes))))


(defgeneric serve (route env) 
  (:documentation "Methods should return a list like that:

                   ```lisp
                   (list 200                                ;; status-code
                         (list :content-type content-type)  ;; headers
                         content)                           ;; content
                   ```")
  ;; (:method ((route route) env)
  ;;   (let ((handler (get-handler route)))
  ;;     (cond
  ;;       (handler (let ((response (funcall handler)))
  ;;                  (etypecase response
  ;;                    (list response)
  ;;                    (string (list 200
  ;;                                  (list :content-type (get-content-type route))
  ;;                                  (list response))))))
  ;;       (t (list 400
  ;;                (list :content-type "text/plain")
  ;;                "No handler for this route")))))
  )


;; (defmacro defroute ((app route &key (content-type "application/json")) &body body)
;;   "Defines a handler for a given route. By default route should return
;;    a serialized JSON:

;;    ```lisp
;;    (defroute (app /api/data)
;;        \"{\"my-data\": [1, 2, 3]}\")
;;    ```

;;    but you can redefine the content type:

;;    ```lisp
;;    (defroute (app /api/data :content-type \"application/xml\")
;;        \"<my-data><item>1</item><item>2</item></my-data>\")
;;    ```

;;    or to serve `robots.txt`:

;;    ```lisp
;;    (defroute (app /robots.txt :content-type \"text/plain\")
;;      (alexandria:read-file-into-string
;;       (asdf:system-relative-pathname \"frontend\"
;;                                      \"robots.txt\")))
;;    ```
;;    "
;;   (let* ((uri (string-downcase (symbol-name route)))
;;          (route-var (gensym "ROUTE")))
;;     `(progn
;;        ;; First, we'll define a handler function
;;        (defun ,route ()
;;          ,@body)

;;        (let ((,route-var (make-instance 'route
;;                                         :template (routes:parse-template ,uri)
;;                                         :handler ',route
;;                                         :content-type ,content-type)))
;;          ;; next, we'll attach a route object to a function name
;;          (setf (get ',route :route)
;;                ,route-var)
;;          ;; and finally, will store this function into the list of
;;          ;; app's routes.
;;          ;; Later, these list of routes will be added to the routes mapper
;;          ;; on app's start.
;;          ;; We need all of this because routes mapper can be recreated on
;;          ;; app's restart and we need an other place to store all routes,
;;          ;; bound to the app.
;;          (pushnew ',route
;;                   (get ',app :route-handlers))

;;          ;; TODO: load all routes when server gets started

;;          ;; Also, if app is currently active, then we should add this route to
;;          ;; the active mapping
;;          ;; (when (reblocks/app:app-active-p ',app)
;;          ;;   (add-route ,route-var))
;;          ))))


(defun find-route-by-class (route-class)
  (loop for node in 40ants-routes/vars::*routes-path*
        when (and (typep node '40ants-routes/included-routes:included-routes)
                  (typep (40ants-routes/included-routes:original-routes node)
                         route-class))
          do (return (40ants-routes/included-routes:original-routes node))))
