(uiop:define-package #:reblocks/routes/server
  (:use #:cl)
  (:import-from #:reblocks/variables
                #:*server*)
  (:import-from #:reblocks/routes
                #:object-routes)
  (:import-from #:reblocks/dependencies
                #:get-route)
  (:import-from #:40ants-routes/generics
                #:add-route))
(in-package #:reblocks/routes/server)


;; TODO: rename to register-routes or may be move this code to push-dependency?
(defun register-dependencies (dependencies)
  "Adds dependencies to the router to make HTTP server handle them."
  (let ((server-routes (object-routes
                        *server*)))
    (dolist (dependency dependencies)
      (let ((route (get-route dependency)))
        (when route
          (add-route server-routes
                     route
                     :override t))))))
