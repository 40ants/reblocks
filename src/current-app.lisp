(defpackage #:reblocks/current-app
  (:use #:cl)
  (:import-from #:reblocks/app
                #:*current-app*
                #:get-prefix)
  (:export
   #:make-uri
   #:get-prefix))
(in-package reblocks/current-app)


(defun get-prefix ()
  "Returns the URL prefix of the application."
  (get-prefix *current-app*))



