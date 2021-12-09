(uiop:define-package #:reblocks/variables
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export #:*default-content-type*
           #:*ignore-missing-actions*
           #:*invoke-debugger-on-error*
           #:*backtrace-on-session-init-error*
           #:*action-string*
           #:*current-app*))

(in-package reblocks/variables)

(defvar-unbound *current-app*
  "A currently active web application.")

;;; Set outgoing encoding to utf-8
(defvar *default-content-type* "text/html; charset=utf-8")

(defvar *ignore-missing-actions* t)

(defvar *invoke-debugger-on-error* nil
  "If this variable is t, then Weblocks will start lisp's debugger on unhandled conditions.")

(defvar *backtrace-on-session-init-error* t)

(defvar *action-string* "action"
  "A string used to pass actions from a client to the server. See
  'reblocks/request:get-request-action'.")
