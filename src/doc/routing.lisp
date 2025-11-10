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
  (:export #:@routing))
(in-package #:reblocks/doc/routing)


(in-readtable pythonic-string-syntax)


(defsection @routing (:title "Routing"
                      :ignore-words ("URL"
                                     "DEFAPP"
                                     "ASDF"
                                     "TODO"
                                     "HTML"
                                     "HTTP"
                                     "ID"
                                     "TASK-PAGE"
                                     "MAKE"
                                     "CL-PPCRE"))
  """
Previously the recommended way for processing different URL paths was the use of REBLOCKS-NAVIGATION-WIDGET system.
But now Reblocks integrates with 40ANTS-ROUTES and it is possible to specify which widgets to show for the URL path.
To do this, use ROUTES argument of DEFAPP macro, like this:

```
(defapp tasks
  :prefix "/"
  :routes ((page ("/<int:task-id>" :name "task-details")
             (make-task-page task-id))
           (page ("/" :name "tasks-list")
             (make-task-list "First"
                             "Second"
                             "Third"))))
```

In this example we define an application with two routes. Route `/` is served by rendering a list of tasks.
Routes like `/100500` are served by rendering `task-page` widgets.

# How does Routing Work

In reblocks, when you start a server, the server holds a root routes collection.
Routes of each application included in the server are included in this root routes collection using a prefix, specified
for the DEFAPP form.

For example, you can define three apps like this:

```
(defapp site
  :prefix "/"
  :routes ((page ("/" :name "landing")
             (make-landing-page))))

(defapp blog
  :prefix "/blog/"
  :routes ((page ("/<int:post-id>" :name "post")
             (make-post-page post-id))))

(defapp admin
  :prefix "/admin/"
  :routes ((page ("/" :name "dashboard")
             (make-dashboard-page))
           (page ("/users/" :name "users")
             (make-users-page))
           (page ("/users/<int:user-id>" :name "user")
             (make-user-page user-id))
           (page ("/posts/" :name "posts")
             (make-posts-page))
           (page ("/posts/<int:post-id>" :name "post")
             (make-post-page user-id))))
```

then the server will combine their routes into the routes hierarchy.

Somebody could also create a library which returns 40ANTS-ROUTES/ROUTES:ROUTES class instance. For example,
such library might provide admin views to control some class of entities. In this case the above
code could be rewritten like this:


```
(defapp admin
  :prefix "/admin/"
  :routes ((page ("/" :name "dashboard")
             (make-dashboard-page))
           (include (crud 'user)
                    :path \"users\")
           (include (crud 'post)
                    :path \"posts\")))
```

Here we've used the `crud` function which returns a 40ANTS-ROUTES/ROUTES:ROUTES class collection to show the list
of entities of a given type and allows creating, editing, and deleting them.

# Wrapping the Page

Often it is desirable to render the same header and footer around each page. In this case, you might use
argument PAGE-CONSTRUCTOR of the DEFAPP macro. If specified, the PAGE-CONSTRUCTOR argument should be
a function of one argument - the widget returned by a route's handler. In the simplest form,
such a page constructor just creates a widget which wraps the page content:

```
(defun wrap-with-frame (widget)
  (make-instance 'page-frame-widget
                 :content widget))
```


# Serving the Static

Another case when defining a route might be desired is serving static files.
The most common cases are serving `robots.txt` and `favicon.ico`. In such cases,
the route handler should return a list instead of a widget:


```
(defapp app
  :prefix "/"
  :routes ((page ("/" :name "index")
             (make-landing-page))

           ;; First way of serving the favicon:
           (static-file "/favicon.ico"
             (asdf:system-relative-pathname :my-app "favicon.ico")
             :content-type "image/x-icon")

           ;; Alternative would be:
           (get ("/favicon.ico")
             (list 200
                   (list :content-type "image/x-icon")
                   (asdf:system-relative-pathname :my-app
                                                  "favicon.ico")))
           ;; Also we can return just a string and it will become
           ;; a page's content:
           (get ("/robots.txt")
             "User-agent: *")))
```

Here we see two alternative ways to serve a favicon file:

1. The first way is to use the REBLOCKS/ROUTES:STATIC-FILE function for constructing the route.
2. The second way is more generic - it uses 40ANTS-ROUTES/DEFROUTES:GET with
   the body returning a list in the format defined by [Clack](https://github.com/fukamachi/clack)
   web server.

As can be seen, when using 40ANTS-ROUTES/DEFROUTES:GET, the route's handler might
return a list of three items: HTTP code, HTTP headers and a pathname. Or just a string.
Actually, this response is passed to Clack as is and anything supported by Clack is supported.
""")
