(uiop:define-package #:reblocks/routes-error-handler
  (:use #:cl)
  (:import-from #:reblocks/error-handler
                #:with-handled-errors)
  (:import-from #:reblocks/routes
                #:serve))
(in-package #:reblocks/routes-error-handler)


(defmethod serve :around (route env)
  "This method handles unhandled exceptions, because this code is common for all routes."
  (with-handled-errors
    (call-next-method)))

