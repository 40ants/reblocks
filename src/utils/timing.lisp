(defpackage #:reblocks/utils/timing
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:log)
  (:export #:*enable-timings*
           #:*timing-report-fn*
           #:timing))
(in-package #:reblocks/utils/timing)


;; TODO: rebuild this functionality, making
;;       global variables hidden.
;;       Add a macro lile with-collected-timings
;;       and use it in the request-handler.lisp

(defvar *enable-timings* nil
  "When this var is T then some parts of request will log timings.")

(defvar *timing-report-fn*
  (lambda (name real cpu)
    (log:debug (format nil "Time spent for ~S, real: ~F, cpu: ~F"
                       name real cpu)))
  "A function of (name real and cpu) to log timing results. By defaults, logs with debug level using log4cl.")

(defun report-timing (name real cpu)
  (funcall *timing-report-fn* name real cpu))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *timing-level* 0))

(declaim (fixnum *timing-level*))

(defmethod on-timing-start (level name))
(defmethod on-timing-end (level name))

(defmacro timing (name &body body)
  (with-gensyms (start/real start/cpu
                 end/real end/cpu
                 spent/real spent/cpu)
    `(let ((thunk (lambda () ,@body)))
       (if *enable-timings*
         (let ((,start/real (get-internal-real-time))
               (,start/cpu (get-internal-run-time)))
           (declare (optimize (speed 3)(safety 3))
                    ((integer 0) ,start/real ,start/cpu))
           (incf *timing-level*)
           (on-timing-start *timing-level* ,name)
           (prog1
             (funcall thunk)
             (let* ((,end/real (get-internal-real-time))
                    (,end/cpu (get-internal-run-time))
                    (,spent/real (/ (- ,end/real ,start/real)
                                    internal-time-units-per-second))

                    (,spent/cpu (/ (- ,end/cpu ,start/cpu)
                                   internal-time-units-per-second)))
               (declare ((integer 0) ,end/real ,end/cpu)
                        ((rational 0) ,spent/real ,spent/cpu))
               ;; Strange place, actually we need on call here, not
               ;; two to report-timing and on-timing-end
               (report-timing ,name ,spent/real ,spent/cpu)
               (on-timing-end *timing-level* ,name)
               
               (decf *timing-level*))))
         (funcall thunk)))))
