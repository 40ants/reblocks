(uiop:define-package #:reblocks/deadline
  (:use #:cl))
(in-package #:reblocks/deadline)


;; SB-SYS:DEADLINE-TIMEOUT

(defmacro with-deadline ((&key seconds) &body body)
  `(flet ((with-deadline-thunk ()
            ,@body))
     (declare (dynamic-extent #'with-deadline-thunk))

     #+sbcl
     (sb-sys:with-deadline (:seconds ,seconds)
       (funcall #'with-deadline-thunk))
     #-sbcl
     (funcall #'with-deadline-thunk)))
