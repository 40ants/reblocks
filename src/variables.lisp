(uiop:define-package #:reblocks/variables
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export #:*default-content-type*
           #:*ignore-missing-actions*
           #:*invoke-debugger-on-error*
           #:*backtrace-on-session-init-error*
           #:*action-string*
           #:*current-app*
           #:*max-pages-per-session*
           #:*pages-expire-in*
           #:*extend-page-expiration-by*
           #:*delay-between-pages-cleanup*))
(in-package #:reblocks/variables)

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


(defvar *pages-expire-in* nil
  "A number of seconds before page and it's actions expiration.
   If NIL, then actions will live in memory forever.")


(defvar-unbound *extend-page-expiration-by*
  "A number of seconds to be added to current page expiration time when a new action
   is processed.

   After an action processing page expiration time either will remain the same or
   become equal to (+ (now) *extend-page-expiration-by*).

   By default, the variable is unbound and the value of *PAGES-EXPIRE-IN* is used.")


(defvar *max-pages-per-session* nil
  "A number of pages per session. When it is reached, oldest pages and their actions will be expired.
   If NIL, then actions will live in memory forever.")


(defvar *delay-between-pages-cleanup* 15
  "A number of seconds between expired pages cleanup.")
