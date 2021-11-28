(uiop:define-package #:weblocks/doc/templates
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package weblocks/doc/templates)


(defsection @templates (:title "Templates"
                        :ignore-words ("CLOS"
                                       "URI"))
  "
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


# Template function

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


# Template definition

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
        in `\"/foo/bar/quux\"` there are three tokens `foo`,
        `bar` and `quux`.

<a name=\"function-designators\"></a>
\\[3\\] I.e. symbols naming a global function and
        function objects themselves.

[mustache]: https://github.com/kanru/cl-mustache
")
