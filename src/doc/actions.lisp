(uiop:define-package #:weblocks/doc/actions
  (:use #:cl)
  (:import-from #:weblocks/actions
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
  (:import-from #:weblocks/variables
                #:*action-string*))
(in-package weblocks/doc/actions)


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
Actions is the core component allowing interactivity in Weblocks applications.

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
(weblocks/app:defapp demo-actions
  :autostart nil)

(weblocks/app-actions:define-action create-user demo-actions (name email)
  (format t "User ~A with email ~A was created.~%"
          name email))
```

After that, this function can be called by name

```lisp
(let ((app (make-instance 'demo-actions)))
  (weblocks/actions:eval-action app
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
  (weblocks-test/utils:with-session
    (let ((action-id
            (weblocks/actions:make-action
             (lambda (email)
               (format t "Changing email for user ~A to ~A"
                       user email)))))
      (weblocks/actions:eval-action nil
                                    action-id
                                    (list "bob@40ants.com")))))
.. <DEBUG> [12:11:44] weblocks/actions actions.lisp (eval-action) -
..   Calling WEBLOCKS/ACTIONS::ACTION: #<FUNCTION (LAMBDA
..                                                    (
..                                                     EMAIL)) {7009B6328B}>
..   with WEBLOCKS/ACTIONS::ARGUMENTS: ("bob@40ants.com")
..   and WEBLOCKS/ACTIONS::ACTION-NAME: "11626:ca7db51f4b0ffb082dcc560eed1b6121707d9677"
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

### initiateAction(actionCode, options)

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

### initiateFormAction(actionCode, form, options)

This version of function accepts a form jquery object as the second argument.
Options argument has the same meaning as for `initiateAction` function,
but `args` attrbute is formed from the serialized form fields and default
`method` is taken from the form's `method` attribute.
  
 
## API
"""

  
  (eval-action generic-function)
  (on-missing-action generic-function)
  (make-action function)
  (make-js-action function)
  (make-js-form-action function)
  (make-action-url function)
  (*action-string* variable))
