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

> **Warning!** This section is outdated and belongs to the old Weblocks documentation.
> All widgets described below were removed from the core framework.
> Probably only a link to [reblocks-ui](https://github.com/40ants/reblocks-ui) is needed here.
>
> Below is a description of how forms worked in the
> original Weblocks. It is given here to show how one might
> handle data model changes. Probably someone will want to
> resurrect this method in a Weblocks extension library.

Forms enable users to communicate with a web application.

Usually the server-side action boils down to selecting, modifying, creating or
deleting data sets (this is sometimes abbreviated as CRUD: Create/Read/Update/Delete).

Building web forms is usually a cumbersome process. Elaborate but
complicated solutions have been devised such as
[php-quickform](https://pear.php.net/manual/en/package.html.html-quickform.tutorial.php),
but so far none of them have been found to match the ease of use and flexibility of
Weblocks' declarative view DSL.


Introduction
============

The form mechanism consists of two parts: the [DATAFORM][DATAFORM] widget and
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

This view should apply to users who are not developers: they may
enter a summary and body, but that's it.

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
since the scaffold inspector has decided this based on the slot's type.

Scaffolding rules can be defined for custom types.

As part of the validation process, Weblocks will also check whether
user input matches the slot's type regardless of whether
scaffolding is used or not.

But let's assume that custom labels are wanted for the dropdown:


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
for operations where the user is not working on an actual model instance.

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

This will display a form with a single field. After form submission, text with the submitted value will be seen.
The `DATA` object here is a class created dynamically from view fields.


```
:persistp nil
```

in the view definition is necessary, as the dynamic class should not persist.

There are options in [MAKE-QUICKFORM][MAKE-QUICKFORM] for validation, control flow and other things.
See the [MAKE-QUICKFORM][MAKE-QUICKFORM] documentation.
")
