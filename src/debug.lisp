(uiop:define-package #:reblocks/debug
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:reblocks/hooks
                #:on-application-hook-handle-http-request)
  (:import-from #:reblocks/session
                #:*session*)
  (:import-from #:reblocks/request
                #:*request*)
  (:import-from #:reblocks/variables
                #:*invoke-debugger-on-error*
                #:*ignore-missing-actions*)
  (:import-from #:reblocks/session-reset
                #:reset-session)
  (:export #:*latest-session*
           #:*latest-request*
           #:reset-latest-session
           #:on
           #:off
           #:status
           #:get-session-value))
(in-package #:reblocks/debug)


;; TODO: move useful staff from debug-mode.lisp, to this package


(defvar *on* nil
  "When true, then Reblocks will be saving addional information useful
for debugging.")


(defvar *config* nil
  "Stores options of last call to (on) or nil if it newer called.

The value of this variable is returned by (status) call as a second value.")


(defvar *latest-session* nil
  "Stores last session, to be able to clear it during development.

To clear, use function \(reset-last-session\).")


(defvar *latest-request* nil
  "Stores last request if debug mode was enabled.")


(defun on (&key
             (track-latest-session t)
             (debug-actions t)
             (track-latest-request t)
             (invoke-debugger-on-error t))
  ;; TODO: think about pluggable switchers to not hardcode them into this function
  (setf *on* t)
  
  (when track-latest-session
    ;; This piece will store latest session in a variable
    (on-application-hook-handle-http-request
        track-latest-session (env)
      
      (setf *latest-session*
            *session*))
    
    ;; Remember that we turned this on
    (setf (getf *config* :track-latest-session)
          t))

  (when track-latest-request
    (when (boundp '*request*)
      (setf *latest-request*
            *request*))
    (setf (getf *config* :track-latest-request)
          t))

  (when debug-actions
    (setf *ignore-missing-actions*
          nil
          (getf *config* :debug-actions)
          t))

  (when invoke-debugger-on-error
    (setf *invoke-debugger-on-error*
          t
          (getf *config* :invoke-debugger-on-error)
          t))

  (log:config :debug)

  (values))


(defun off ()
  (setf *on* nil)

  (when (getf *config* :track-latest-session)
    ;; TODO: implement hook removal
    ;; (reblocks/hooks:remove-application-hook :handle-request
    ;;                                         track-latest-session)
    )
  
  (when (getf *config* :debug-actions)
    (setf *ignore-missing-actions*
          t))
  
  (when (getf *config* :invoke-debugger-on-error)
    (setf *invoke-debugger-on-error*
          nil))
  
  (setf *config* nil)

  (log:config :warn)

  (values))


(defun status ()
  (values *on* *config*))


(defun reset-latest-session ()
  "Resets the latest session when debug mode is on."
  (restart-case
      (progn
        (unless *on*
          (error "Debugging wasn't turned on and I know nothing about latest session."))
        (reset-session *latest-session*))
    (enable-debugging ()
      :report "Enable debug mode (you'll need to refresh page and call RESET-LATEST-SESSION again)."
      (on))))


(defun get-session-value (key)
  "Returns a value from the latest session."
  (unless *latest-session*
    (error "Please, turn on debug mode with (reblocks/debug:on) call and refresh the page."))
  (gethash key *latest-session*))
