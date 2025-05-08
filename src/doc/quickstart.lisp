(defpackage #:reblocks/doc/quickstart
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/doc/routing
                #:@routing)
  (:import-from #:reblocks/doc/example
                #:defexample)
  (:import-from #:reblocks-ui/form)
  (:import-from #:reblocks/html)
  (:export #:@quickstart))
(in-package #:reblocks/doc/quickstart)


(defsection @quickstart (:title "Quickstart"
                         :ignore-words ("ASDF"
                                        "TODO"
                                        "CLOS"
                                        "REPL"
                                        "POST"
                                        "HTML"
                                        "DOM"
                                        "UI"
                                        "DONE"
                                        "ADD-TASK"
                                        "ONCLICK"
                                        "TOGGLE"
                                        "TASK-LIST"
                                        "RENDER"
                                        "AJAX")
                         :external-links (("Webinspector" . "https://developers.google.com/web/tools/chrome-devtools/inspect-styles/")
                                          ("Ultralisp" . "https://ultralisp.org/")
                                          ("Reblocks-ui" . "https://github.com/40ants/reblocks-ui/")
                                          ("PCL" . "http://www.gigamonkeys.com/book/object-reorientation-classes.html")
                                          ("CLOS-Cookbook" . "https://lispcookbook.github.io/cl-cookbook/clos.html")
                                          ("DB-Cookbook" . "https://lispcookbook.github.io/cl-cookbook/databases.html")
                                          ("Spinneret" . "https://github.com/ruricolist/spinneret/")))
  "
> This version of Reblocks is not in Quicklisp yet. To
> install it you need to clone the repository somewhere where
> ASDF will find it, for example, to the `~/common-lisp/` directory.
> You also need to clone [reblocks-ui][reblocks-ui].

> You can also install the [Ultralisp][Ultralisp] Quicklisp distribution where all Reblocks-related libraries are present and up to date.


Load reblocks and create a package for a sandbox:

```
CL-USER> (ql-dist:install-dist \"http://dist.ultralisp.org/\"
                               :prompt nil)
CL-USER> (ql:quickload '(:reblocks :reblocks-ui :find-port))
CL-USER> (defpackage todo
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
                    #:defapp))
#<PACKAGE \"TODO\">
CL-USER> (in-package todo)
#<PACKAGE \"TODO\">
```

Now, create an application:

```
TODO> (defapp tasks)
```

By default, the name of the app defines the url where it is
accessible. Here, the \"tasks\" app will be accessible under
<http://localhost:40000/tasks>. We can change it with the
PREFIX argument of REBLOCKS/APP:DEFAPP:

```
TODO> (defapp tasks
         :prefix \"/\")
```

Now our app runs under the root url.

```
TODO> (reblocks/debug:on)
TODO> (defvar *port* (find-port:find-port))
TODO> (reblocks/server:start :port *port*)
 <INFO> [19:41:00] reblocks/server server.lisp (start) -
  Starting reblocks REBLOCKS/SERVER::PORT: 40000
  REBLOCKS/SERVER::SERVER-TYPE: :HUNCHENTOOT DEBUG: T
 <INFO> [19:41:00] reblocks/server server.lisp (start-server) -
  Starting webserver on REBLOCKS/SERVER::INTERFACE: \"localhost\"
  REBLOCKS/SERVER::PORT: 40000 DEBUG: T
 #<SERVER port=40000 running>
 (NIL)
```

Open <http://localhost:40000/tasks/> in your browser (double check the port) and you'll see a
text like that:

```
No reblocks/page:init-page method defined.
Please define a method reblocks/page:init-page to initialize a page.

It could be something simple, like this one:

(defmethod reblocks/page:init-page ((app tasks) url-path expire-at)
            \"Hello world!\")

Read more in the documentaion.
```

It means that you didn't write any code for your application. Let's do
it now and make an application which outputs a list of tasks.

In the end, we'll build the mandatory TODO-list app:

![The TODO-list app in Reblocks](docs/images/quickstart-check-task.gif)

# The Task widget

```
TODO> (defwidget task ()
        ((title
          :initarg :title
          :accessor title)
         (done
          :initarg :done
          :initform nil
          :accessor done)))
```

This code defines a task widget, the building block of our
application. REBLOCKS/WIDGET:DEFWIDGET is similar to Common Lisp's DEFCLASS,
in fact it is only a wrapper around it. It takes a name, a list of
super-classes (here `()`) and a list of slot definitions.

We can create a task with MAKE-INSTANCE:

```
TODO> (defvar *task-1* (make-instance 'task :title \"Make my first Reblocks app\"))
TODO> *task-1*
#<TASK {1005406F33}>
```

Above, we provide only a TITLE argument, and since we didn't give a DONE argument,
it will be instanciated to its initform, which is NIL.

We defined accessors for both slots, so we can read and set them easily:

```
TODO> (title *task-1*)
\"Make my first Reblocks app\"
TODO> (done *TASK-1*)
NIL
TODO> (setf (done *TASK-1*) t)
T
```

We define a constructor for our task:

```
TODO> (defun make-task (title &key done)
        (make-instance 'task :title title :done done))
```

It isn't mandatory, but it is good practice to do so.


If you are not familiar with the Common Lisp Object System (CLOS), you
can have a look at [Practical Common Lisp][PCL]
and the [Common Lisp Cookbook][CLOS-Cookbook].

Now let's carry on with our application.


# The Tasks-list widget

Below we define a more general widget that contains a list of tasks,
and we tell Reblocks how to display them by *specializing* the
REBLOCKS/WIDGET:RENDER generic-function for our newly defined classes:

```
TODO> (defwidget task-list ()
        ((tasks
          :initarg :tasks
          :accessor tasks)))

TODO> (defmethod render ((task task))
        \"Render a task.\"
        (with-html ()
              (:span (if (done task)
                         (with-html ()
                               (:s (title task)))
                       (title task)))))

TODO> (defmethod render ((widget task-list))
        \"Render a list of tasks.\"
        (with-html ()
              (:h1 \"Tasks\")
              (:ul
                (loop for task in (tasks widget) do
                      (:li (render task))))))
```

The REBLOCKS/HTML:WITH-HTML macro uses
[Spinneret][Spinneret] under the hood,
but you can use anything that outputs html.

We can check how the generated html looks like by calling
REBLOCKS/WIDGET:RENDER generic-function in the REPL:


```
TODO> (render *task-1*)
<div class=\"widget task\"><span>Make my first Reblocks app</span>
</div>
NIL
```

But we still don't get anything in the browser.


```
TODO> (defun make-task-list (&rest rest)
        (let ((tasks (loop for title in rest
                        collect (make-task title))))
          (make-instance 'task-list :tasks tasks)))

TODO> (defmethod reblocks/page:init-page ((app tasks) (url-path string) expire-at)
         (declare (ignorable app url-path expire-at))
         (make-task-list \"Make my first Reblocks app\"
                         \"Deploy it somewhere\"
                         \"Have a profit\"))
```

This defines a list of tasks (for simplicity, they are defined as a
list in memory) and returns what will be our session's root widget..

Restart the application:

```
TODO> (reblocks/debug:reset-latest-session)
```

Right now it should look like this:"

  (example1 reblocks-example)

  "# Adding tasks

Now, we'll add some ability to interact with a list – to add some tasks
into it, like so:

![Adding tasks in our TODO-list interactively.](docs/images/quickstart-add-task.gif)

Import a new module, [reblocks-ui][reblocks-ui] to help in creating forms and other UI elements:

```
TODO> (ql:quickload \"reblocks-ui\")
TODO> (use-package :reblocks-ui/form)
```

Write a new ADD-TASK method and modify the RENDER method of a
task-list to call ADD-TASK in response to POST method:

```
TODO> (defmethod add-task ((task-list task-list) title)
        (push (make-task title)
              (tasks task-list))
        (update task-list))
            
TODO> (defmethod render ((task-list task-list))
        (with-html ()
          (:h1 \"Tasks\")
          (loop for task in (tasks task-list) do
            (render task))
          (with-html-form (:POST (lambda (&key title &allow-other-keys)
                                         (add-task task-list title)))
            (:input :type \"text\"
                    :name \"title\"
                    :placeholder \"Task's title\")
            (:input :type \"submit\"
                    :value \"Add\"))))

TODO> (reblocks/debug:reset-latest-session)
```

The method ADD-TASK does only two simple things:

- it adds a task into a list;
- it tells Reblocks that our task list should be redrawn.

This second point is really important because it allows Reblocks to render
necessary parts of the page on the server and to inject it into the HTML DOM
in the browser. Here it rerenders the task-list widget, but we can as well [REBLOCKS/WIDGET:UPDATE][generic-function]
a specific task widget, as we'll do soon.

We are calling ADD-TASK from a lambda function to catch a
TASK-LIST in a closure and make it availabe when reblocks will
process AJAX request with POST parameters later.

Another block in our new version of RENDER of a TASK-LIST is the form:

```
(with-html-form (:POST #'add-task)
   (:input :type \"text\"
    :name \"task\"
    :placeholder \"Task's title\")
   (:input :type \"submit\"
    :value \"Add\"))
```

It defines a text field, a submit button and an action to perform on
form submit.

Go, try it! This demo is interative:"

  (example2 reblocks-example)

  
  "
> **This is really amazing!**
> 
> With Reblocks, you can handle all the business logic
> server-side, because an action can be any lisp function, even an
> anonymous lambda, closuring all necessary variables.

Restart the application and reload the page. Test your form now and see in a
[Webinspector][Webinspector] how Reblocks sends requests to the server and receives
HTML code with rendered HTML block.

Now we'll make our application really useful – we'll add code to toggle the tasks' status.


# Toggle tasks

```
TODO> (defmethod toggle ((task task))
        (setf (done task)
              (if (done task)
                  nil
                  t))
        (update task))

TODO> (defmethod render ((task task))
        (with-html ()
          (:p (:input :type \"checkbox\"
            :checked (done task)
            :onclick (make-js-action
                      (lambda (&key &allow-other-keys)
                        (toggle task))))
              (:span (if (done task)
                   (with-html ()
                         ;; strike
                         (:s (title task)))
                 (title task))))))
```

We defined a small helper to toggle the DONE attribute, and we've
modified our task rendering function by adding a code to render a
checkbox with an anonymous lisp function, attached to its
ONCLICK attribute.

The REBLOCKS/ACTIONS:MAKE-JS-ACTION function returns a Javascript code,
which calls back a lisp lambda function when evaluated in the browser.
And because TOGGLE updates a Task widget, Reblocks returns on this
callback a new prerendered HTML for this one task only.

Here is how our app will work now:"

  (example3 reblocks-example)

  "# What is next?

As a homework:

1. Play with lambdas and add a \"Delete\" button next after
   each task.
2. Add the ability to sort tasks by name or by completion flag.
3. Save tasks in a database (this [Cookbook chapter][DB-Cookbook] might help).
4. Read the REBLOCKS/DOC/ROUTING:@ROUTING section.
5. Read the rest of the documentation and make a real application, using the full
   power of Common Lisp.")


(defexample example1 ()
  (reblocks/widget:defwidget task ()
    ((title
      :initarg :title
      :accessor title)
     (done
      :initarg :done
      :initform nil
      :accessor done)))
  
  (reblocks/widget:defwidget task-list ()
    ((tasks
      :initarg :tasks
      :accessor tasks)))

  (defmethod reblocks/widget:render ((task task))
    "Render a task."
    (reblocks/html:with-html ()
      (:span (if (done task)
                 (:s (title task))
                 (title task)))))

  (defmethod reblocks/widget:render ((widget task-list))
    "Render a list of tasks."
    (reblocks/html:with-html ()
      (:h1 "Tasks")
      (:ul
       (loop for task in (tasks widget) do
         (:li (reblocks/widget:render task))))))

  (defun make-task (title &key done)
    (make-instance 'task :title title :done done))

  (defun make-example ()
    (make-instance 'task-list
                   :tasks (list (make-task "Make my first Reblocks app")
                                (make-task "Deploy it somewhere")
                                (make-task "Have a profit")))))


(defexample example2 (:inherits example1 :height "15em")
  (defmethod add-task ((task-list task-list) title)
    (push (make-task title)
          (tasks task-list))
    (reblocks/widget:update task-list))
  
  (defmethod reblocks/widget:render ((task-list task-list))
    (reblocks/html:with-html ()
      (:h1 "Tasks")
      (loop for task in (tasks task-list) do
        (reblocks/widget:render task))
      (reblocks-ui/form:with-html-form (:POST (lambda (&key title &allow-other-keys)
                                                (add-task task-list title)))
        (:input :type "text"
                :name "title"
                :placeholder "Task's title")
        (:input :type "submit"
                :class "button"
                :value "Add")))))


(defexample example3 (:inherits example2 :height "15em")
  (defmethod toggle ((task task))
    (setf (done task)
          (if (done task)
              nil
              t))
    (reblocks/widget:update task))

  (defmethod reblocks/widget:render ((task task))
    (reblocks/html:with-html ()
      ;; (:a :href "http://localhost:40000/examples/reblocks/doc/quickstart/example3?some=arg"
      ;;     "Click me to test POST cookies")
      (:p (:input :type "checkbox"
                  :checked (done task)
                  :onclick (reblocks/actions:make-js-action
                            (lambda (&key &allow-other-keys)
                              (toggle task))))
          (:span (if (done task)
                     (reblocks/html:with-html ()
                       ;; strike
                       (:s (title task)))
                     (title task)))))))
