(uiop:define-package #:reblocks/routes
  (:use #:cl)
  (:import-from #:40ants-routes/defroutes)
  (:import-from #:40ants-routes/routes)
  (:import-from #:40ants-routes/vars)
  (:import-from #:40ants-routes/included-routes)
  (:import-from #:40ants-routes/url-pattern
                #:parse-url-pattern)
  (:import-from #:serapeum
                #:->)
  (:import-from #:40ants-routes/handler
                #:call-handler)
  (:export #:serve
           #:server-routes
           #:static-route
           #:page
           #:find-route-by-class
           #:page-route
           #:object-routes
           #:static-file-route
           #:file-pathname
           #:file-content-type
           #:static-file))
(in-package #:reblocks/routes)


(defclass server-routes (40ants-routes/routes:routes)
  ())


(defclass page-route (40ants-routes/route:route)
  ())


(defmacro page ((path &key name title) &body handler-body)
  `(40ants-routes/defroutes:get (,path :name ,name :title ,title :route-class page-route)
     ,@handler-body))


(defclass static-route (40ants-routes/route:route)
  ()
  (:documentation "The class of route for serving static file. Content of this file could be on a local disk or can be fetched from some other place."))


(defclass static-file-route (static-route)
  ((pathname :initarg :pathname
             :type pathname
             :reader file-pathname)
   (content-type :initarg :content-type
                 :type string
                 :reader file-content-type))
  (:documentation "The class of route for serving static file from the local disk."))


(-> static-file (string pathname
                 &key
                 (:content-type string)
                 (:name string)
                 (:title string))
    (values static-file-route &optional))


(defun static-file (path pathname &key (content-type "text/plain") name title)
  "Creates a route item for serving a static file from the local disk."
  (let ((name (or name
                  (string-downcase
                   (gensym "UNNAMED-ROUTE-"))))
        (pattern (parse-url-pattern path)))
    (make-instance 'static-file-route
                   :name name
                   :title title
                   :method :get
                   :pattern pattern
                   :pathname pathname
                   :content-type content-type)))


(defgeneric object-routes (obj)
  (:documentation "Returns a routes object bound to a server or application."))


(defgeneric serve (route env) 
  (:documentation "Methods should return a list like that:

                   ```lisp
                   (list 200                                ;; status-code
                         (list :content-type content-type)  ;; headers
                         content)                           ;; content
                   ```"))


(defun find-route-by-class (route-class &key (routes-path 40ants-routes/vars::*routes-path*))
  (loop for node in routes-path
        when (and (typep node '40ants-routes/included-routes:included-routes)
                  (typep (40ants-routes/included-routes:original-routes node)
                         route-class))
          do (return (40ants-routes/included-routes:original-routes node))))


(defmethod reblocks/routes:serve ((route 40ants-routes/route:route) env)
  "Generic handler for any kind of route."
  (declare (ignorable env))
  (call-handler))


(defmethod reblocks/routes:serve ((route static-file-route) env)
  "Returns content of a file's from local disk."
  (declare (ignorable env))
  (list 200
        (list :content-type (file-content-type route))
        (file-pathname route)))
