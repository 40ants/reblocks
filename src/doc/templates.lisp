(uiop:define-package #:reblocks/doc/templates
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/templates)


(defsection @templates (:title "Templates"
                        :ignore-words ("CLOS"
                                       "URI"))
  "
Templates were something different from views. They are another layer which
allows customizing the views' look. Most of the Weblocks code supported templates
and this gave us the ability to change widget HTML without changing widget
logic.

Templates brought theming functionality into the original Weblocks and were intended to
improve widget reuse.

I think it is better to split your render method into smaller ones
and allow users to override these methods. But anyway, here is a short example
of how templates worked in the old version of Weblocks.

**Note!** The following subsections contain only examples and such code is
not supported in the latest version of Weblocks. However, you might want
to implement something like that on your own or use [djula](https://github.com/mmontone/djula),
[mustache][mustache] or other template engine.


# Template function

Template functions received some key parameters and returned HTML strings.
You were able to use any HTML generation library or template library for templates. 

You were encouraged to 

* use a naming convention and call templates `*-wt` (with the \"-wt\" suffix
  ). \"wt\" means either \"web template\" or \"Weblocks template\". Since
  templates should be overridden often, naming conventions will make it easier
  to find what you need.
* use `&allow-other-keys` in every template.
* use no complex logic and no computations in templates. Just simple
  iteration over lists and if statements. The best
  thing would be to use no more logic than in [mustache][mustache] templates.

Here is a template example.

```          
(defun my-wt (&key (content \"I'm template\") &allow-other-keys)
    (with-html-to-string 
        (:p (str content))))
```


# Template definition

A template definition is a `deftemplate` call. 

```          
(deftemplate :my-wt 'my-wt)
```

Here we just connected the `:my-wt` template name with the template function `'my-wt`.

And here comes the most important template part.
We can connect many template functions to a template name and only one of them - the effective template - will be called.
Effectiveness is determined by priority which is received from context matchers. 
A context matcher just receives context and returns a priority number.

```
(defun my-other-wt(&key (content \"I'm template\") &allow-other-keys)
  (with-html-to-string 
    (:div :class \"other-template\" (str content))))

(deftemplate :my-wt 'my-other-wt 
  :context-matches (lambda(&rest context)
    100))
```

`my-other-wt` has higher priority than `my-wt` so it will be called. 
And this is how template overriding is done.

There is also an `:application-class` parameter which gives the template 10
more priority points.

```          
(deftemplate :page-wt 'my-customized-page-wt 
             :application-class 'my-webapp)
```

Here the `'my-customized-page-wt` function will have higher priority than
the one defined in Weblocks.

**Note.** This context matching logic was too complex and in some places it intersected with CLOS and generic functions
which is why I decided to exclude templates from the Weblocks core.

As an example of old-school templating, you may see
[Weblocks Twitter Bootstrap theme](https://github.com/html/weblocks-twitter-bootstrap-application).

**Footnotes**

<a name=\"tree\"></a>
\\[1\\] An acyclic graph with exactly one parent per node.

<a name=\"uri-tokens\"></a>
\\[2\\] Each path component of a URI is a token; for example
        in `\"/foo/bar/quux\"` there are three tokens `foo`,
        `bar` and `quux`.

<a name=\"function-designators\"></a>
\\[3\\] I.e. symbols naming a global function and
        function objects themselves.

[mustache]: https://github.com/kanru/cl-mustache
")
