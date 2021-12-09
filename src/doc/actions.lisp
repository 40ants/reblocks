(uiop:define-package #:reblocks/doc/actions
  (:use #:cl)
  (:import-from #:reblocks/actions
                #:eval-action
                #:make-action
                #:make-js-action
                #:make-js-form-action
                #:make-action-url
                #:on-missing-action)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:reblocks/variables
                #:*action-string*
                #:*ignore-missing-actions*)
  (:import-from #:reblocks/doc/example
                #:defexample))
(in-package reblocks/doc/actions)


(in-readtable pythonic-string-syntax)


(defsection @actions (:title "Actions"
                      :ignore-words ("AJAX"
                                     "JS"
                                     "URL"
                                     "POST"
                                     "GET"
                                     "HTML"
                                     "API"
                                     "USER")
                      :external-links (("sticky" . "https://www.haproxy.com/blog/client-ip-persistence-or-source-ip-hash-load-balancing/")))
  """
  Actions is the core component allowing interactivity in Reblocks applications.

  Actions are callbacks that are stored in a session and can be called from the browser
  using AJAX, POST or GET requests.

  There are two types of actions:

  * Application-wide.
  * Session-bound.

  ## Application actions

  This kind of actions is good when you want to build a scalable application,
  because amount of memory they are taking does not grow with number of user
  sessions.

  Application action should be a function which only depends on it's argument.
  They should not closeup any variables because the same function will be called
  for every application's user.

  Here is an example, how to define such action which will create a new user:

  ```lisp
  (reblocks/app:defapp demo-actions
    :autostart nil)

  (reblocks/app-actions:define-action create-user demo-actions (name email)
    (format t "User ~A with email ~A was created.~%"
            name email))
  ```

  After that, this function can be called by name

  ```lisp
  (let ((app (make-instance 'demo-actions)))
    (reblocks/actions:eval-action app
                                  "create-user"
                                  (list "Bob"
                                        "bob@40ants.com"))))
  => "User Bob with email bob@40ants.com was created."
  ```

  ## Session actions

  Session actions can be a stand-alone functions or closures. This makes
  it easier to catch some variables and to use them when user interacts
  with application.

  However, such closures are stored in a hash table bound to the each session.
  This makes a few consequences:

  - it impossible to use database as a session storage and after application
    restart all session actions are expire.
  - it makes harder to scale application, because now you need to
    redirect each user to a single backend, using [sticky sessions][sticky] on the
    balancer.
  - session actions tend to accumulate over time and eat more memory.

  In the next example, we can see how it is easy to catch USER variable
  into the action's closure and then use it when email will be provided:

  ```lisp
  (let ((user "Bob"))
    (reblocks-test/utils:with-session
      (let ((action-id
              (reblocks/actions:make-action
               (lambda (email)
                 (format t "Changing email for user ~A to ~A"
                         user email)))))
        (reblocks/actions:eval-action nil
                                      action-id
                                      (list "bob@40ants.com")))))
  .. <DEBUG> [12:11:44] reblocks/actions actions.lisp (eval-action) -
  ..   Calling REBLOCKS/ACTIONS::ACTION: #<FUNCTION (LAMBDA
  ..                                                    (
  ..                                                     EMAIL)) {7009B6328B}>
  ..   with REBLOCKS/ACTIONS::ARGUMENTS: ("bob@40ants.com")
  ..   and REBLOCKS/ACTIONS::ACTION-NAME: "11626:ca7db51f4b0ffb082dcc560eed1b6121707d9677"
  ..   
  .. Changing email for user Bob to bob@40ants.com
  => NIL
  ```

  With application-wide action you'll have to render user's id in the hidden
  field of HTML form and then to retrieve the user object from the database
  when action get called.

  Now lets see how to use actions from the frontend!

  ## Using at the Frontend

  At the fronted you might want to call actions as the result of user's click,
  form submit or some other act, for example, by timer.

  Reblocks provides a thin JS layer which includes following functions:

  - `initiateAction(actionCode, options)`
  - `initiateFormAction(actionCode, form, options)`

  **`initiateAction(actionCode, options)`**

  This function calls given action using AJAX. You have to pass optional
  arguments as JS object `options`:

  ```json
  {
      "method": "POST",
      "args": {},
      "url": undefined,
      "on_success": undefined,
      "on_failure": undefined,
  }
  ```

  Here is an example which uses MAKE-ACTION and renders usual HTML link as a button
  bound to an anonymous lisp function:

  """
  (button-click reblocks-example)

  """
  Also, you might generate a link using MAKE-ACTION-URL function and use it as HREF
  argument of any link on a page. Click on such link will trigger the action.

  Here I intentionally didn't add a "button" class to show that this is just a link.

  Pay attention how this example flickers when you click the link. This is because
  the whole iframe's content is reloaded when you click the link. In the previous
  example this is not happening because click leads to an AJAX requst and only a
  piece of page get updated.
  """

  (just-href reblocks-example)

  """
  In the next example, we'll pass arguments to our action. To do this, we have
  to call MAKE-ACTION and render JS code manually:
  """
  (many-buttons-click reblocks-example)

  """
  **`initiateFormAction(actionCode, form, options)`**

  This version of function accepts a form jquery object as the second argument.
  Options argument has the same meaning as for `initiateAction` function,
  but `args` attrbute is formed from the serialized form fields and default
  `method` is taken from the form's `method` attribute.

  Next example shows how to process a form submit, using a callback,
  registered with MAKE-JS-FORM-ACTION:
  """
  
  (form-example reblocks-example)

  """
  ## API
  """
  
  (eval-action generic-function)
  (on-missing-action generic-function)
  (make-action function)
  (make-js-action function)
  (make-js-form-action function)
  (make-action-url function)
  (*action-string* variable)
  (*ignore-missing-actions* variable))


(defexample button-click ()
  (reblocks/widget:defwidget button-with-counter ()
    ((counter :initform 0
              :accessor counter)))

  (defmethod reblocks/widget:render ((widget button-with-counter))
    (let ((action (reblocks/actions:make-js-action
                   (lambda (&rest args)
                     (declare (ignore args))
                     (incf (counter widget))
                     (reblocks/widget:update widget)))))
      (reblocks/html:with-html
        (:a :class "button"
            :onclick action
            (format nil "Clicked ~A time~:P"
                    (counter widget)))))))


(defexample many-buttons-click ()
  (reblocks/widget:defwidget two-buttons ()
    ((first-counter :initform 0
                    :accessor first-counter)
     (second-counter :initform 0
                     :accessor second-counter)))
  
  (defmethod reblocks/widget:render ((widget two-buttons))
    (let ((action-code (reblocks/actions:make-action
                        (lambda (&key button-id &allow-other-keys)
                          (case button-id
                            (0 (incf (first-counter widget)))
                            (1 (incf (second-counter widget))))
                          (reblocks/widget:update widget)))))
      (reblocks/html:with-html
        (:a :class "button"
            :onclick (format nil "initiateAction(\"~A\", {\"args\": {\"button-id\": 0}}) ; return false;"
  action-code)
            (format nil "Clicked ~A time~:P"
                    (first-counter widget)))
        (:a :class "button secondary"
            :onclick (format nil "initiateAction(\"~A\", {\"args\": {\"button-id\": 1}}); return false;"
                             action-code)
            (format nil "Clicked ~A time~:P"
                    (second-counter widget)))))))


(defexample form-example ()
  (reblocks/widget:defwidget registration-form ()
    ((name :initform nil
           :accessor name)
     (email :initform nil
            :accessor email)
     (sent :initform nil
           :accessor sent)))

  (defmethod reblocks/widget:render ((widget registration-form))
    (reblocks/html:with-html
      (cond
        ((sent widget)
         (let ((action (reblocks/actions:make-js-action
                        (lambda (&rest args)
                          (declare (ignore args))
                          (setf (sent widget) nil)
                          (reblocks/widget:update widget)))))
           (reblocks/html:with-html
             (:p (format nil "Congratulations, ~A!"
                         (name widget)))
             (:p (:a :class "button" :onclick action "Reset")))))
        (t
         (let ((action (reblocks/actions:make-js-form-action
                        (lambda (&key name email &allow-other-keys)
                          (setf (name widget) name
                                (email widget) email
                                (sent widget) t)
                          (reblocks/widget:update widget)))))
           (:form :method :post
                  :onsubmit action
                  (:input :name "name"
                          :type "text"
                          :placeholder "Your name")
                  (:input :name "email"
                          :type "text"
                          :placeholder "Your email")
                  (:input :type "submit"
                          :class "button"
                          :value "Register"))))))))


(defexample just-href ()
  (reblocks/widget:defwidget button-with-counter ()
    ((counter :initform 0
              :accessor counter)))

  (defmethod reblocks/widget:render ((widget button-with-counter))
    (let ((action-url (reblocks/actions:make-action-url
                       (lambda (&rest args)
                         (declare (ignore args))
                         (incf (counter widget))
                         (reblocks/widget:update widget)))))
      (reblocks/html:with-html
        (:a :href action-url
            (format nil "Clicked ~A time~:P"
                    (counter widget)))))))
