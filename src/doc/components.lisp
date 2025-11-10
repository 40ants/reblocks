(defpackage #:reblocks/doc/components
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/server)
  (:import-from #:reblocks/preview)
  (:export #:@components))
(in-package #:reblocks/doc/components)


(defsection @components (:title "Components"
                         :ignore-words ("AJAX"
                                        "URI"
                                        "HTML"
                                        "CLOS"
                                        "HTTP"
                                        "LAYER"
                                        "API"
                                        "URL"
                                        "TCP"
                                        "MAKE-WIDGET"
                                        ;; TODO: make an external-link
                                        "REBLOCKS-NAVIGATION-WIDGET"
                                        ))
  "
# Widgets

Widgets are the main building blocks of Reblocks (hence the name).

At the heart of Reblocks is a tree of widgets that is manipulated by the clients
requests. When a client sends its first request to a Reblocks application then
a new session is started for it and a widget tree is associated with this session.

This initial widget tree <sup>[1](#tree)</sup> is computed as defined by the application developer.
The generic function REBLOCKS/SESSION:INIT is called by Reblocks
to initialize a new session. This function should return a single widget which becomes
the root of a tree:

```
TODO> (defmethod reblocks/session:init ((app tasks))
         (declare (ignorable app))
         (make-task-list \"Make my first Reblocks app\"
                         \"Deploy it somewhere\"
                         \"Have a profit\"))
```

The initial content and structure of the widget tree may depend on arbitrary
factors; it is conceivable (although probably not very sensible) to generate
different widget trees depending on the time of day.

A client's request for a specific URI modifies the widget tree: widgets
called **dispatchers** choose their one child based on the current request
URI.

The old version of Weblocks had a MAKE-WIDGET function. It was possible to use strings
and function designators <sup>[3](#function-designators) instead of widgets in this context.
Probably, we will return this functionality some day.


# Actions

Apart from session initialization the widget tree may be modified by **actions**.

Actions are plain functions that are stored in a session hash table the keys
of which are unique identifiers. This way they can be called by the browser.

When Javascript is available actions will be called via AJAX, otherwise
a normal request will be initiated.

It is often useful to set up closures as actions.

Example of typical action usage:

```
(reblocks/widget:defwidget counter ()
  ((count :accessor count-of :initform 0)))

(defmethod reblocks/widget:render ((widget counter))
  (with-html ()
    (:p (format nil \"The counter is at ~D.\" (count-of widget)))
    (:p (:button :onclick (reblocks/actions:make-js-action
                            (lambda (&rest args)
                              ;; closes over WIDGET.
                              (incf (count-of widget))))
         \"Count up!\"))))
```

# Navigations and dispatchers

Dispatchers are widgets that configure themselves and their children
(i.e. broadly speaking the widget tree) based on the URI of the request.

Navigations are dispatchers that maintain a simple one-to-one association between
an URI token <sup>[2](#uri-tokens)</sup> a widget.

Old versions of Weblocks supported such dispatchers out of the box,
but during refactoring this functionality was moved into a separate
REBLOCKS-NAVIGATION-WIDGET system. Read its documentation to learn more.

# Servers

To make everything work, you need to start one or more webservers on some TCP ports.

When you are starting a server, usually you specify an interface and port to listen on
and a list of Reblocks apps to serve.

You might define your own server class and inherit it from REBLOCKS/SERVER:SERVER class.
This will allow you to customize a list of HTTP middlewares.
"
  ;; Here is the list of functions useful when working with Reblocks servers:
  ;; (reblocks/session:init generic-function)
  ;; (reblocks/server:server class)
  ;; (reblocks/server:start function)
  ;; (reblocks/server:stop function)
  ;; (reblocks/server:servers function)
  ;; (reblocks/server:running-p function)
  ;; (reblocks/preview:preview function)
  ;; (reblocks/preview:stop function)
  ;; (reblocks/server:insert-middleware function)
  ;; (reblocks/server:make-middlewares generic-function)
  ;; (reblocks/server:*default-samesite-policy* variable)
  )
