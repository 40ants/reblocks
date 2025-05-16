(uiop:define-package #:reblocks/routes
  (:use #:cl)
  (:import-from #:40ants-routes/defroutes)
  (:import-from #:40ants-routes/routes)
  (:import-from #:40ants-routes/vars)
  (:import-from #:40ants-routes/included-routes)
  (:export #:serve
           #:server-routes
           #:static-route
           #:page
           #:find-route-by-class
           #:page-route
           #:object-routes))
(in-package #:reblocks/routes)


(defclass server-routes (40ants-routes/routes:routes)
  ())


(defclass static-route (40ants-routes/route:route)
  ())


(defclass page-route (40ants-routes/route:route)
  ())


(defmacro page ((path &key name title) &body handler-body)
  `(40ants-routes/defroutes:get (,path :name ,name :title ,title :route-class page-route)
     ,@handler-body))


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
