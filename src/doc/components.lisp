(defpackage #:weblocks/doc/components
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export #:@components))
(in-package weblocks/doc/components)


(defsection @components (:title "Components"
                         :ignore-words ("AJAX"
                                        "URI"
                                        "HTML"
                                        "CLOS"
                                        "MAKE-WIDGET")
                         ;; :external-docs ("https://40ants.com/weblocks-navigation-widget/")
                         )
  "
# Widgets

Widgets are the main building blocks of Weblocks (hence the name).

At the heart of Weblocks is a tree of widgets that is manipulated by the clients
requests. When a client sends its first request to a Weblocks application then
a new session is started for it and a widget tree is associated with this session.

This initial widget tree <sup>[1](#tree)</sup> is computed as defined by the application developer.
A generic-function WEBLOCKS/SESSION:INIT is called by Weblocks
to initialize a new session. This function should return a single widget which become
a root of a tree:

```
TODO> (defmethod weblocks/session:init ((app tasks))
         (declare (ignorable app))
         (make-task-list \"Make my first Weblocks app\"
                         \"Deploy it somewhere\"
                         \"Have a profit\"))
```

The initial content and structure of the widget tree may depend on arbitrary
factors; it is conceivable (although probably not very sensible) to generate
different widget trees depending on the time of day.

A client's request for a specific URI modifies the widget tree: widgets
called **dispatchers** choose their one child based on the current request
URI.

Old version of Weblocks had a MAKE-WIDGET function. You was able to use strings
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
(weblocks/widget:defwidget counter ()
  ((count :accessor count-of :initform 0)))

(defmethod weblocks/widget:render ((widget counter))
  (with-html
    (:p (format nil \"The counter is at ~D.\" (count-of widget)))
    (:p (:button :onclick (weblocks/actions:make-js-action
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
WEBLOCKS-NAVIGATION-WIDGET system. Read its documentation to learn more.
")
