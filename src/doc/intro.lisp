(defpackage #:reblocks/doc/intro
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:@intro))
(in-package #:reblocks/doc/intro)


(defsection @intro (:title "Introduction"
                           :ignore-words ("ASDF"
                                          "API"
                                          "HTTP"
                                          "URL"
                                          "HTTP"
                                          "LAYER"
                                          "RSS"))
  "

[![](https://ultralisp.org/projects/40ants/reblocks.svg)](https://ultralisp.org/projects/40ants/reblocks)

Reblocks is a fork of the Weblocks web framework written by Slava Akhmechet
and maintained by Scott L. Burson and Olexiy Zamkoviy.

Some time ago I (Alexander Artemenko) found this old Weblocks and liked its idea
of building web applications out of widgets. But the framework's code was complex
and hard to change. Thus I decided to refactor it into a smaller core plus
separate libraries implementing more advanced features.

For a few years this refactored code lived in the repository https://github.com/40ants/weblocks and
it was possible to install it from Ultralisp only. I didn't publish it
to Quicklisp to not break sites using the old version of Weblocks. But in January 2022
I decided to rename the ASDF system to \"reblocks\" to make it installable from
Quicklisp under this name.

Core Features of Reblocks
=========================

* Application pages are built from \"widgets\".
* All business logic can be implemented in Common Lisp on the server-side.
* The framework's code is separated by functionality into separate packages.
* The project now has [hyperlinked documentation](https://40ants.com/reblocks/)
  with code snippets and embedded interactive demos.

The framework's API is still under active development. All significant changes are
described in the REBLOCKS/DOC/CHANGELOG:@CHANGELOG. You can add the ChangeLog page
to your RSS reader to be notified of new releases.

Removed Features
================

These features are present in the old-school Weblocks version but were removed
from the core of Reblocks. Some of them might be implemented as separate ASDF
systems.

* Views
* Templates
* Forms
* Continuation based dialogs

You can read about these tools in the REBLOCKS/DOC/INDEX::@REMOVED-FEATURES section.

Learning Reblocks
=================

To learn how to use Reblocks to build a simple single-page application, proceed to the
REBLOCKS/DOC/QUICKSTART:@QUICKSTART section.

To learn how a real-world application can be built,
inspect [Ultralisp's](https://ultralisp.org) code:

<https://github.com/ultralisp/ultralisp>

Getting Support
===============

* In case of any bugs, please [create an issue](https://github.com/40ants/reblocks/issues) on GitHub.
* If you have some ideas to share or want to discuss a feature request, use [GitHub Discussions](https://github.com/40ants/reblocks/discussions).

")
