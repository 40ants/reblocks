(defpackage #:reblocks/widgets/root
  (:use #:cl)
  (:shadow #:get)
  ;; Just dependencies
  (:import-from #:reblocks/session)
  
  (:export #:get))
(in-package reblocks/widgets/root)


(defun get ()
  (reblocks/session:get-value 'root-widget))


(defun (setf get) (value)
    (setf (reblocks/session:get-value 'root-widget)
     value))
