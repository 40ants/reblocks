(defpackage #:reblocks/doc/routing
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/routes
                #:route
                #:serve)
  (:export #:@routing))
(in-package #:reblocks/doc/routing)


(defsection @routing (:title "Routing")
  (@quickstart section)
  ;; (@lowlevel-api section)
  )


;; TODO: Rewrite this section using new routing
(defsection @quickstart (:title "Quickstart"
                         :ignore-words ("URL"
                                        "DEFROUTES"
                                        "MAKE-TASKS-ROUTES"
                                        "ASDF"
                                        "HTML"
                                        "TASK-PAGE"
                                        "MAKE"
                                        "CL-PPCRE"
                                        ;; TODO: make an external-link
                                        "REBLOCKS-NAVIGATION-WIDGET"
                                        "REBLOCKS-NAVIGATION-WIDGET:DEFROUTES")
                         ;; :external-docs ("https://40ants.com/reblocks-navigation-widget/") 
                         )
  "
In the quickstart tutorial, we saw how to create and render widgets,
and how to react to user events. We also learned that by default the
app name defined its base url, and how to change it.

Here, we will extend the example and make each task accessible under
`/tasks/<task id>`.

Before we start, here's the summary on how to handle routing in
Reblocks:

* use REBLOCKS-NAVIGATION-WIDGET:DEFROUTES.

* DEFROUTES associates URLs (as strings) to a widget:

```
(defroutes tasks-routes
  (\"/tasks/\\d+\" (make-task-page))
  (\"/tasks/\" (make-task-list)))
```

* REBLOCKS/SESSION:INIT must return an instance of the route
  widget, using the MAKE-TASKS-ROUTES constructor created by DEFROUTES macro.

Let's start. Note that you can see the full code
[on Github](https://github.com/40ants/reblocks/blob/reblocks/docs/source/routing.lisp).


# Getting started

> **Warning!**
>
> As for this version of Reblocks,
> REBLOCKS-NAVIGATION-WIDGET system is not in Quicklisp
> yet. To install it you need to clone the repository
> somewhere where ASDF will find it, for example, to the
> `~/common-lisp/` or `~/quicklisp/local-projects/`
> directories.

You can also install the [Ultralisp][Ultralisp] Quicklisp distribution where all Reblocks-related libraries are present and up to date.

Load and import the routing library:

```
TODO> (ql:quickload '(:reblocks-navigation-widget))
```

The package definition becomes::

```
TODO> (defpackage todo
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
        (:import-from #:reblocks-navigation-widget
                      #:defroutes))
```


# Extending the example: to each task an id

We want each task to have an id, so we add a slot to the `task` widget:

```
TODO> (defwidget task ()
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

We also need a simple in-memory \"database\". We'll use a hash-table to
save the tasks. It associates an id to the task:

```
TODO> (defparameter *store* (make-hash-table) \"Dummy store for tasks: id -> task.\")
```

Our task constructor will give them an incremental id:

```
TODO> (defparameter *counter* 0 \"Simple counter for the hash table store.\")
TODO> (defun make-task (title &key done)
        \"Create a task and store it by its id.\"
        (let* ((id (incf *counter*))
               (task (make-instance 'task :title title :done done :id id)))
          (setf (gethash id *store*) task)
          task))
```

So we create a utility function to find a task by its id. All this
could just be an interface to a database.

```
TODO> (defun get-task (id)
        (gethash id *store*))
```

When we render the tasks list, we add an href on the task, so we can go to `/tasks/<id>`:

```
TODO> (defmethod render ((task task))
        (with-html ()
          (:p (:input :type \"checkbox\"
                      :checked (done task)
                      :onclick (make-js-action
                                (lambda (&key &allow-other-keys)
                                  (toggle task))))
              (:span (if (done task)
                         (with-html ()
                           (:s (title task)))
                         (:a :href (format nil \"/tasks/~a\" (id task)) ;; <-- only addition.
                             (title task)))))))
```

# The task-page widget

In Reblocks, an HTML block that we want to display, and possibly update
independently, is a widget. Here, we want to show a task's details on
their own page, it is then a widget.

```
TODO> (defwidget task-page ()
        ((task
          :initarg :task
          :initform nil
          :accessor task)))

TODO> (defmethod render ((task-page task-page))
        (let ((task (task task-page)))
          (with-html ()
            (:div \"Task \" (id task))
            (:h1 (title task))
            (:div (if (done task) \"Done!\" \"To Do.\"))
            (:div \"Lorem ipsum…\"))))
```


# Defining routes

At this point we can think of our routes like this:

```
(defroutes tasks-routes
  (\"/tasks/\\d+\" <create the task-page widget>)
  (\"/tasks/\" (make-task-list)))
```

The regexp `\\d+` will capture any URL that is formed of digits and
contains at least one.

As we see, the TASK-PAGE constructor will need to get the id
matched by the route.


# Path and URL parameters

To get the current path, use `(reblocks/request:get-path)`. Then,
you can find the matching parameters with [CL-PPCRE][CL-PPCRE].

Our TASK-PAGE constructor becomes:

```
TODO> (defun make-task-page ()
        (let* ((path (reblocks/request:get-path))
               (id (first (ppcre:all-matches-as-strings \"\\d+\" path)))
               (task (get-task (parse-integer id))))
          (if task
              (make-instance 'task-page :task task)
              (not-found))))
TODO> (defun not-found ()
        \"Show a 404 not found page.\"
        (with-html ()
          (:div \"Task not found.\")))
```

And our router is simply:

```
TODO> (defroutes tasks-routes
        (\"/tasks/\\d+\" (make-task-page))
        (\"/tasks/\" (make-task-list \"Make my first Reblocks app\"
                                   \"Deploy it somewhere\"
                                   \"Have a profit\")))
```

The DEFROUTES macro creates a new class and its constructor, named
`MAKE-<CLASS-NAME>`.

> **Note**
>
> It is important to use the constructor instead of MAKE-INSTANCE, as it defines properties on the fly.


# Redirections

To perform redirections, use `(reblocks/response:redirect \"/url\")`:


```
TODO> (defroutes tasks-routes
        (\"/tasks/\\d+\" (make-task-page))
        (\"/tasks/list/?\" (reblocks/response:redirect \"/tasks/\"))  ;; <-- redirection
        (\"/tasks/\" (make-task-list \"Make my first Reblocks app\"
                                   \"Deploy it somewhere\"
                                   \"Have a profit\")))
```

Here the trailing `/?` allows to catch `/tasks/list` and `/tasks/list/`.

And indeed, contrary to what we stated in the introduction,
REBLOCKS/RESPONSE:REDIRECT does not return a widget but signals a specital condition.


# Final steps

Make our router the main widget for this session:

```
TODO> (defmethod reblocks/session:init ((app tasks))
        (declare (ignorable app))
        (make-tasks-routes))
```

Reset the session:

```
TODO> (defun reset ()
        (setf *counter* 0)
        (reblocks/debug:reset-latest-session))
TODO> (reset)
```

And access the app at <http://localhost:40000/tasks/>.

[Ultralisp]: https://ultralisp.org/
[CL-PPCRE]: https://edicl.github.io/cl-ppcre/
")


;; (defsection @lowlevel-api (:title "Lowlevel API"
;;                            :ignore-words ("JSON"
;;                                           "URL"))
;;   (route class)
;;   (serve generic-function))
