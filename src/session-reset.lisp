(defpackage #:reblocks/session-reset
  (:use #:cl)
  (:import-from #:reblocks/session
                #:*session*)
  (:import-from #:reblocks/hooks))
(in-package reblocks/session-reset)


(defun reset-session (session)
  "Internal function for session reset."
  (when session
    (reblocks/hooks:with-reset-session-hook (session)
      (clrhash session))))


;; This function whould be external for reblocks/session package
;; to make it easier for end user
(defun reblocks/session:reset ()
  "Reset current session."
  (reset-session *session*))
