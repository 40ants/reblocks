(defpackage #:reblocks/doc/routing
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/routes
                #:route
                #:serve
                #:page)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get)
  (:export #:@routing))
(in-package #:reblocks/doc/routing)


(in-readtable pythonic-string-syntax)


(defsection @routing (:title "Routing"
                      :ignore-words ("URL"
                                     "DEFAPP"
                                     "ASDF"
                                     "TODO"
                                     "HTML"
                                     "TASK-PAGE"
                                     "MAKE"
                                     "CL-PPCRE")
                      ;; :external-docs ("https://40ants.com/reblocks-navigation-widget/")
                      )
  """
In the quickstart tutorial, we saw how to create and render widgets,
and how to react to user events. We also learned that by default the
app name defined its base url, and how to change it.

Here, we will extend the example and make each task accessible under
`/tasks/<task id>`.
  
Before we start, here's the summary on how to handle routing in
Reblocks:
  
* use REBLOCKS/APP:DEFAPP with a ROUTES argument.
  
* The ROUTES argument contains a list of route definitions:
  
    ```
    (defapp tasks
      :prefix "/"
      :routes ((page ("/tasks/\\d+" :name "task-details")
                 (make-task-page))
               (page ("/tasks/" :name "task-list")
                 (make-task-list))))
    ```
  
* Each route is defined using functions like `page`, `get`, `file-server`, etc.
  
  Let's start. Note that you can see the full code
  [on Github](https://github.com/40ants/reblocks/blob/reblocks/docs/source/routing.lisp).
  
  
# Getting started
  
You can install the [Ultralisp][Ultralisp] Quicklisp distribution where all Reblocks-related libraries are present and up to date.
  
The package definition becomes:
  
```
(defpackage #:todo
  (:use #:cl
        #:reblocks-ui/form
        #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/routes
                #:page)
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get))
(in-package #:todo)
```
  

# Extending the example: to each task an id

We want each task to have an id, so we add a slot to the `task` widget:

```
(defwidget task ()
  ((title
    :initarg :title
    :initform nil
    :accessor title)
   (done
    :initarg :done
    :initform nil
    :accessor done)
   (id
    :initarg :id
    :initform nil
    :accessor id
    :type integer)))
```

We also need a simple in-memory "database". We'll use a hash-table to
save the tasks. It associates an id to the task:

```
(defvar *store* (make-hash-table) "Dummy store for tasks: id -> task.")
```

Our task constructor will give them an incremental id:

```
(defvar *counter* 0 "Simple counter for the hash table store.")


(defun make-task (title &key done)
  "Create a task and store it by its id."
  (let* ((id (incf *counter*))
         (task (make-instance 'task :title title :done done :id id)))
    (setf (gethash id *store*) task)
    task))
```

So we create a utility function to find a task by its id. All this
could just be an interface to a database.

```
(defun get-task (id)
  (gethash id *store*))
```

And we need a function to toggle task's state:

```
(defun toggle (task)
  (setf (done task)
        (not (done task))))
```

When we render the tasks list, we add an href on the task, so we can go to `/tasks/<id>`:

TODO: replace format with route-url task-details

```
(defmethod render ((task task))
  (with-html ()
    (:p (:input :type "checkbox"
                :checked (done task)
                :onclick (make-js-action
                          (lambda (&key &allow-other-keys)
                            (toggle task))))
        (:span (if (done task)
                   (with-html ()
                     (:s (title task)))
                   (:a :href (format nil "/tasks/~a" (id task)) ;; <-- only addition.
                       (title task)))))))
```

# The task-page widget

In Reblocks, an HTML block that we want to display, and possibly update
independently, is a widget. Here, we want to show a task's details on
their own page, it is then a widget.

```
(defwidget task-page ()
  ((task
    :initarg :task
    :initform nil
    :accessor task)))

(defmethod render ((task-page task-page))
  (let ((task (task task-page)))
    (with-html ()
      (:div "Task " (id task))
      (:h1 (title task))
      (:div (if (done task) "Done!" "To Do."))
      (:div "Lorem ipsum…"))))
```


# Defining application with routes

At this point we can think of our routes like this:

```
;; TODO Change
(defapp tasks
  :prefix "/"
  :routes ((page ("/tasks/\\d+" :name "task-details")
             (make-task-page))
           (page ("/tasks/" :name "task-list")
             (make-task-list))))
```

The text `<int:task-id>` says to Reblocks that it need to  capture and integer parameter from the URL. Parameter's name is `task-id` and it will be bound during the route's body execution. In the previous Reblocks version, this parameter should be extracted from the URL manuall, but after the 0.62.0 version it can be done automatically.

# Path and URL parameters

To get the current path, use `(reblocks/request:get-path)`. Then,
you can find the matching parameters with [CL-PPCRE][CL-PPCRE].

Our TASK-PAGE constructor becomes:

;; TODO: Remove
```
(defun make-task-page ()
  (let* ((path (reblocks/request:get-path))
         (id (first (ppcre:all-matches-as-strings "\\d+" path)))
         (task (get-task (parse-integer id))))
    (if task
        (make-instance 'task-page :task task)
        (not-found))))


(defun not-found ()
  "Show a 404 not found page."
  (with-html ()
    (:div "Task not found.")))
```

And our app definition with routes is simply:

```
(defapp tasks
  :prefix "/"
  :routes ((page ("/tasks/\\d+" :name "task-details")
             (make-task-page))
           (page ("/tasks/" :name "task-list")
             (make-task-list "Make my first Reblocks app"
                           "Deploy it somewhere"
                           "Have a profit"))))
```

The route definitions are processed when the app is defined, and each route is registered with the routing system.


# Redirections

To perform redirections, use `(reblocks/response:redirect "/url")`:


```
(defapp tasks
  :prefix "/"
  :routes ((page ("/tasks/\\d+" :name "task-details")
             (make-task-page))
           (get ("/tasks/list/?" :name "task-list-redirect")
             (reblocks/response:redirect "/tasks/"))  ;; <-- redirection
           (page ("/tasks/" :name "task-list")
             (make-task-list "Make my first Reblocks app"
                           "Deploy it somewhere"
                           "Have a profit"))))
```

Here the trailing `/?` allows to catch `/tasks/list` and `/tasks/list/`.

Note that for simple redirections, we use the `get` route type instead of `page`.
REBLOCKS/RESPONSE:REDIRECT does not return a widget but signals a special condition.


# Final steps

With the new routing system, you don't need to explicitly set up the router in the session initialization. The app definition with its routes is automatically registered when the app is defined.

Reset the session if needed:

```
(defun reset ()
  (setf *counter* 0)
  (reblocks/debug:reset-latest-session))
(reset)
```

And access the app at <http://localhost:40000/tasks/>.

[Ultralisp]: https://ultralisp.org/
[CL-PPCRE]: https://edicl.github.io/cl-ppcre/
""")


;; (defsection @lowlevel-api (:title "Lowlevel API"
;;                            :ignore-words ("JSON"
;;                                           "URL"))
;;   (route class)
;;   (serve generic-function))
