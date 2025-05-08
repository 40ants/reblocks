(uiop:define-package #:reblocks/doc/session
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/doc/example
                #:defexample)
  (:import-from #:reblocks/session
                #:with-session
                #:get-value
                #:delete-value
                #:gen-id
                #:in-session-p
                #:init-session
                #:reset
                #:expire
                #:get-number-of-sessions
                #:make-session-middleware
                                         
                #:get-number-of-anonymous-sessions))
(in-package #:reblocks/doc/session)


(defsection @session (:title "Using the Session"
                      :ignore-words ("REBLOCKS"
                                     "SESSION"
                                     "HTML"
                                     "HTTP"
                                     "ID"))

  "
The functions and macros to manipulate the session are defined in the
REBLOCKS/SESSION package.

# Session initialization

When a new user opens the site in the browser, Reblocks does these steps:

* Creates a new session object. You can consider it a hash table where some
  data could be stored.
* Generates an unique id and put's this session object to the hash map of all
  sessions.
* Calls the INIT-SESSION generic function to determine what widget should be shown to the user.
* Renders the root widget to HTML and sends it to the browser.
* Along with this HTML response, sends `Set-Cookie` header to save session id
  in the browser.

The method you define for INIT-SESSION generic-function should return a root widget.
This widget may include children and render them in it's
REBLOCKS/WIDGET:RENDER generic-function implementation.

You will find an example of [INIT-SESSION][generic-function] in the REBLOCKS/DOC/QUICKSTART::@QUICKSTART section.

# Storing data

You can store any kind of data in a session.

To set a value, use `(SETF GET-VALUE)` function.

For example, you might want to store information about the current user if he logged in:

"
  (login-example reblocks-example)

  "# API"
  ;; (with-session macro)
  ;; (get-value function)
  ;; (delete-value function)
  ;; (gen-id function)
  ;; (in-session-p function)
  ;; (init-session generic-function)
  ;; (reset function)
  ;; (expire function)
  ;; (get-number-of-sessions function)
  ;; (make-session-middleware function)
  ;; (get-number-of-anonymous-sessions function)
  )


(defexample login-example ()
  (reblocks/widget:defwidget login-example ()
    ())

  (defmethod reblocks/widget:render ((widget login-example))
    (reblocks/html:with-html
      (flet ((login-callback (&key login &allow-other-keys)
               (setf (reblocks/session:get-value :user) login)
               (reblocks/widget:update widget))
             (reset-callback (&rest args)
               (declare (ignore args))
               (reblocks/session:reset)
               (reblocks/widget:update widget)))
        (let* ((login-code (reblocks/actions:make-js-form-action #'login-callback))
               (reset-code (reblocks/actions:make-js-form-action #'reset-callback))
               (user (reblocks/session:get-value :user)))

          (:h3 "Session management example")
          
          (:form :method "POST"
                 :onsubmit (if user
                               reset-code
                               login-code)
                 (:table
                  (:tr
                   (cond
                     (user
                      (:td (format nil "Hello ~A" user))
                      (:td (:input :type "submit"
                                   :class "button"
                                   :value "Logout")))
                     (t
                      (:td (:input :type "text"
                                   :name "login"
                                   :placeholder "Enter login"))
                      (:td (:input :type "submit"
                                   :class "button"
                                   :value "Login"))))))))))))
