============
 Routing
============

In the quickstart tutorial, we saw how to create and render widgets,
and how to react to user events. We also learned that by default the
app name defined its base url, and how to change it.

Here, we will extend the example and make each task accessible under
``/tasks/<task id>``.

Before we start, here's the summary on how to handle routing in
Weblocks:

* use ``weblocks-navigation-widget:defroutes``

* ``defroutes`` associates URLs (as strings) to a widget::

.. code-block:: common-lisp

   (defroutes tasks-routes
     ("/tasks/\\d+" (make-task-page))
     ("/tasks/" (make-task-list)))

* ``weblocks/session:init`` must return an instance of the route
  widget, using the ``make-tasks-routes`` constructor created by ``defroutes``.

Let's start. Note that you can see the full code `on Github
<https://github.com/40ants/weblocks/blob/reblocks/docs/source/routing.lisp>`_.


Getting started
===============

.. warning:: As for this version of Weblocks,
             ``weblocks-navigation-widget`` is not in Quicklisp
             yet. To install it you need to clone the repository
             somewhere where ASDF will find it, for example, to the
             ``~/common-lisp/`` or ``~/quicklisp/local-projects/``
             directories.

.. seealso:: You can also install the `Ultralisp`_ Quicklisp distribution where all Weblocks-related libraries are present and up to date.

Load and import the routing library:

.. code-block:: common-lisp-repl

   TODO> (ql:quickload '(:weblocks-navigation-widget))

The package definition becomes::

   TODO> (defpackage todo
           (:use #:cl
                 #:weblocks-ui/form
                 #:weblocks/html)
           (:import-from #:weblocks/widget
                         #:render
                         #:update
                         #:defwidget)
           (:import-from #:weblocks/actions
                         #:make-js-action)
           (:import-from #:weblocks/app
                         #:defapp)
           (:import-from #:weblocks-navigation-widget
                         #:defroutes))


Extending the example: to each task an id
=========================================

We want each task to have an id, so we add a slot to the ``task`` widget:

.. code-block:: common-lisp-repl

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


We also need a simple in-memory "database". We'll use a hash-table to
save the tasks. It associates an id to the task:

.. code-block:: common-lisp-repl

   TODO> (defparameter *store* (make-hash-table) "Dummy store for tasks: id -> task.")


Our task constructor will give them an incremental id:

.. code-block:: common-lisp-repl

   TODO> (defparameter *counter* 0 "Simple counter for the hash table store.")
   TODO> (defun make-task (title &key done)
           "Create a task and store it by its id."
           (let* ((id (incf *counter*))
                  (task (make-instance 'task :title title :done done :id id)))
             (setf (gethash id *store*) task)
             task))

So we create a utility function to find a task by its id. All this
could just be an interface to a database.

.. code-block:: common-lisp-repl

   TODO> (defun get-task (id)
           (gethash id *store*))

When we render the tasks list, we add an href on the task, so we can go to ``/tasks/<id>``:

.. code-block:: common-lisp-repl

   TODO> (defmethod render ((task task))
           (with-html
             (:p (:input :type "checkbox"
                         :checked (done task)
                         :onclick (make-js-action
                                   (lambda (&key &allow-other-keys)
                                     (toggle task))))
                 (:span (if (done task)
                            (with-html
                              (:s (title task)))
                            (:a :href (format nil "/tasks/~a" (id task)) ;; <-- only addition.
                                (title task)))))))


The task-page widget
====================

In Weblocks, an HTML block that we want to display, and possibly update
independently, is a widget. Here, we want to show a task's details on
their own page, it is then a widget.

.. code-block:: common-lisp-repl

   TODO> (defwidget task-page ()
           ((task
             :initarg :task
             :initform nil
             :accessor task)))

   TODO> (defmethod render ((task-page task-page))
           (let ((task (task task-page)))
             (with-html
               (:div "Task " (id task))
               (:h1 (title task))
               (:div (if (done task) "Done!" "To Do."))
               (:div "Lorem ipsum…"))))


Defining routes
===============

At this point we can think of our routes like this:

.. code-block:: common-lisp

    (defroutes tasks-routes
      ("/tasks/\\d+" <create the task-page widget>)
      ("/tasks/" (make-task-list)))

The regexp ``\\d+`` will capture any URL that is formed of digits and
contains at least one.

As we see, the ``task-page`` constructor will need to get the id
matched by the route.


Path and URL parameters
=======================

To get the current path, use ``(weblocks/request:get-path)``. Then,
you can find the matching parameters with ``cl-ppcre``.

Our ``task-page`` constructor becomes:

.. code-block:: common-lisp-repl

   TODO> (defun make-task-page ()
           (let* ((path (weblocks/request:get-path))
                  (id (first (ppcre:all-matches-as-strings "\\d+" path)))
                  (task (get-task (parse-integer id))))
             (if task
                 (make-instance 'task-page :task task)
                 (not-found))))
   TODO> (defun not-found ()
           "Show a 404 not found page."
           (with-html
             (:div "Task not found.")))

And our router is simply::

   TODO> (defroutes tasks-routes
           ("/tasks/\\d+" (make-task-page))
           ("/tasks/" (make-task-list "Make my first Weblocks app"
                                      "Deploy it somewhere"
                                      "Have a profit")))

The ``defroutes`` macro creates a new class and its constructor, named
``make-<class-name>``.

.. note:: It is important to use the constructor instead of ``make-instance``, as it defines properties on the fly.

Redirections
============

To perform redirections, use ``weblocks/response:redirect "/url"``:

.. code-block:: common-lisp-repl

   TODO> (defroutes tasks-routes
           ("/tasks/\\d+" (make-task-page))
           ("/tasks/list/?" (weblocks/response:redirect "/tasks/"))  ;; <-- redirection
           ("/tasks/" (make-task-list "Make my first Weblocks app"
                                      "Deploy it somewhere"
                                      "Have a profit")))

Here the trailing ``/?`` allows to catch ``/tasks/list`` and ``/tasks/list/``.

And indeed, contrary to what we stated in the introduction,
``redirect`` is not a widget.

Final steps
===========

Make our router the main widget for this session:

.. code-block:: common-lisp-repl

   TODO> (defmethod weblocks/session:init ((app tasks))
           (declare (ignorable app))
           (make-tasks-routes))

Reset the session:

.. code-block:: common-lisp-repl

   TODO> (defun reset ()
           (setf *counter* 0)
           (weblocks/debug:reset-latest-session))
   TODO> (reset)

And access the app at ``http://localhost:40000/tasks/``.

.. _Ultralisp: https://ultralisp.org/
