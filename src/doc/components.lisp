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
                         :external-docs ("https://40ants.com/weblocks-navigation-widget/"))
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

# Removed Conceptions

During refactoring, I've removed some other interesting conceptions from
the core of the framework. They could be reborn as a separate libraries
as well as navigation widget.

## Views

Views provided convenient ways to define how data object are to be rendered.

Weblocks supported three types of views:

- data views;
- table views;
- form views.

Form views were especially useful because they let you build forms
in a declarative manner, error checking and reporting included. 

Views mainly consist of a rendering framework unique to the
view and a series of view fields. The table view for example knows
how to render a HTML table.

View fields usually map to the slots of your data class but they were
flexible enough to be used in any other way. Associated with each view field
regardless of the view type are things like a human-readable label and
information on how to render the field (presentation).

Form views included additional parameters like how to translate user
input to a proper model value (parser) and constraints on the data
(satisfies).


## Templates

Template were something different from view.  It is another layer which
allows to customize views look.  Most of weblocks code supported templates
and this gave us ability to change widget html without changing widget
logic.

Templates brought theming functionality into Weblocks, and intended to
improve widgets reusing.

I think it is better to split you render method into a smaller ones
and allow user to override these methods. But anyway, here is a short example
of how did templated work in old version of the Weblocks.

**Note!** Following subsections contains only examples and such code is
not supported in the latest version of the Weblocks. However you might want
to implement something like that on your own or use [djula](https://github.com/mmontone/djula),
[mustache][mustache] or other template engine.


### Template function

Template function received some key parameters and returned html string.
You was able to use any html generation library or template library for templates. 

You was encouraged to 

* use naming convention and call template `*-wt` (with \"-wt\" suffix
  ). \"wt\" means either \"web template\" or \"Weblocks template\". Since
  template should be overriden often, name convention will made easier
  to find what we need.
* use `&allow-other-keys` in every template.
* use no complex logic and no computations in templates. Just simple
  iteration over lists and if statements. The best
  thing would be to use no more logic then in [mustache][mustache] templates.

Here is a template example.

```          
(defun my-wt (&key (content \"I'm template\") &allow-other-keys)
    (with-html-to-string 
        (:p (str content))))
```


### Template definition

Template definition is a `deftemplate` call. 

```          
(deftemplate :my-wt 'my-wt)
```

Here we just connected `:my-wt` template name with template function `'my-wt`.

And here comes most important templates part.
We can connect many template functions to template name and only one of them - effective template - will be called.
Effectiveness determined by priority which is received from context matchers. 
Context matcher just receives context and returns priority number.

```
(defun my-other-wt(&key (content \"I'm template\") &allow-other-keys)
  (with-html-to-string 
    (:div :class \"other-template\" (str content))))

(deftemplate :my-wt 'my-other-wt 
  :context-matches (lambda(&rest context)
    100))
```

`my-other-wt` has more priority than `my-wt` so it will be called. 
And this is how template overriding is done.

There is also `:application-class` parameter which gives template 10
more priority points.

```          
(deftemplate :page-wt 'my-customized-page-wt 
             :application-class 'my-webapp)
```

Here `'my-customized-page-wt` function will have more priority than
one defined in Weblocks.

**Note.** This context matching logic was too complex and at some places it was intersected with CLOS and generic functions
that is why I've decided to exclude templates from the core of the Weblocks.

As an example for the oldschool templating you may see
[Weblocks Twitter Bootstrap theme](https://github.com/html/weblocks-twitter-bootstrap-application).

**Footnotes**

<a name=\"tree\"></a>
\\[1\\] An acyclic graph with exactly one parent per node.

<a name=\"uri-tokens\"></a>
\\[2\\] Еach path component of an URI is a token; for example
        in `\"/foo/bar/quux\"` there are three tokens `foo`
        `bar` and `quux`.

<a name=\"function-designators\"></a>
\\[3\\] I.e. symbols naming a global function and
        function objects themselves.

[mustache]: https://github.com/kanru/cl-mustache
")
