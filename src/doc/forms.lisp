(uiop:define-package #:reblocks/doc/forms
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks/doc/forms)


(defsection @forms (:title "Forms"
                    :ignore-words ("CRUD"
                                   "DATAFORM"
                                   "FORM-VIEW"
                                   "DEFVIEW"
                                   "DSL"
                                   "SUMMARY"
                                   "BODY"
                                   "DATA"
                                   "MAKE-QUICKFORM")
                    :external-links
                    (("DATAFORM" . "https://quickref.common-lisp.net/weblocks.html#go-to-the-WEBLOCKS_2236_2236DATAFORM-class")
                     ("MAKE-QUICKFORM" . "https://quickref.common-lisp.net/weblocks.html#go-to-the-WEBLOCKS_2236_2236DATAFORM-class")
                     ("DEFVIEW" . "https://quickref.common-lisp.net/weblocks.html#go-to-the-WEBLOCKS_2236_2236DEFVIEW-macro")
                     ("FORM-VIEW" . "https://quickref.common-lisp.net/weblocks.html#go-to-the-WEBLOCKS_2236_2236FORM_002dVIEW-class")))
  "

> **Warning!** This section is outdated and belongs to the old Weblocks documention.
> All widgets, described below, were removed from the core framework.
> Probably we need only to link to the [reblocks-ui](https://github.com/40ants/reblocks-ui) here.
>
> Below you'll find a description how did forms work in the
> original Weblocks. It is given to here to show who one might
> handle data-models changes. Probably someone will want to
> resurrect this method in a Weblocks extension library.

Forms enable the user to communicate with a web application.

Usually the server side action boils down to selecting, modifying, creating or
deleting data sets (this is sometimes abbreviated as CRUD: Create/Read/Update/Delete)

Building web forms is usually a cumbersome process. Elaborate but
complicated solutions have been devised
[php-quickform](https://pear.php.net/manual/en/package.html.html-quickform.tutorial.php),
but so far we haven't found any of them to match the ease of use and flexibility of
Weblocks' declarative view DSL.


Introduction
============

The form mechanism consists of two parts, the [DATAFORM][DATAFORM] widget and
the [FORM-VIEW][FORM-VIEW] view type.

Forms are usually built by defining form views using the [DEFVIEW][DEFVIEW] macro
and instantiating a [DATAFORM][DATAFORM] object with this view.


Simple example
==============

Let's define a view for creating and editing bug reports.

Let the model be defined thus:

```
(defclass bug-report ()
  ((id :type integer :initform (create-unique-id))
   (status :type (member :new :open :resolved) :initform :new)
   (summary :type string)
   (body :type string)))
```

This view should apply to users that are not developers: they may
enter a summary and body but that's it.

```          
(defview bug-report-form-view
    (:type form :inherit-from '(:scaffold bug-report)
     :caption \"Enter a bug report\")
  (id :hidep t)
  (status :hidep t))
```

The `SUMMARY` and `BODY` fields will default to
strings; every form field is presented as a text input and parsed as a
string by default.

Let's use this view to derive the view for developers:

```          
(defview bug-report-form-view/developer
    (:type form :inherit-from 'bug-report-form-view )
  (status :hidep nil))
```

The status field will automatically be displayed as a dropdown control
since the scaffold inspector has decided this upon the slot's type.

You can define scaffolding rules for your own types.

As part of the validation process Weblocks will also check whether a
user input matches the slot's type regardless of whether you use
scaffolding or not.

But let's assume that we want custom labels for the dropdown:


```
(defview bug-report-form-view/developer
    (:type form :inherit-from 'bug-report-form-view )
  (status :hidep nil
          :present-as (dropdown :choices '((\"This report is totally new, don't trust it!\" . :new)
                                           (\"Yeah okay, we're working on it.\" . :open)
                                           (\"We've solved that problem already...\" . :resolved)))))
```

Quickforms
==========

Quickforms are specialized Dataforms.

They provide a way to build forms based entirely on a view; they are handy
for operations where you don't have the user working on an actual model instance.

Let's dive right into it:

```
(make-quickform 
  (defview nil 
    (:caption \"A Quickform\" :type form :persistp nil)
    (some-text  :present-as input))
  :on-success (lambda (form data)
    (with-html ()
      \"Submitted data - \"
      (str (slot-value data 'some-text)))))
```

This will display form with single field. After form submit we'll see text with value submitted.
`DATA` object here is a class created dynamically from view fields.


```
:persistp nil
```

in view definition is necessary, we don't want dynamic class to persist.

There are options in [MAKE-QUICKFORM][MAKE-QUICKFORM] for validation, control flow and other things.
See [MAKE-QUICKFORM][MAKE-QUICKFORM] documentation.
")
