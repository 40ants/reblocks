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
Reblocks is the fork of the Weblocks web frameworks written by Slava Akhmechet
and maintained by Scott L. Burson and Olexiy Zamkoviy.

Some time ago I (Alexander Artemenko) found this old Weblocks and liked its idea
of building web application out of widgets. But framework's code was complex
and hard to change. Thus I decided to refactor it into a smaller core plus
separate libraries implementing more advanced features.

A few years this refactored code lived in the repository https://github.com/40ants/weblocks and
it was possible to install it from Ultralisp only. I didn't publish it
to Quicklisp to not break sites using old version of the Weblocks. But at January 2022
I decided to rename ASDF system into the \"reblocks\" to make it installable from
the Quicklisp under this name.

Core Features of Reblocks
=========================

* Application pages are built from \"widgets\".
* All business logic could be implemented in Common Lisp in server-side.
* Framework's code is separated by functionality into a separate packages.
* Project now has a [hyperlinked documentation](https://40ants.com/reblocks/)
  with code snippets and embedded interactive demos.

Framework's API is still under active development. All significant changes are
described in the REBLOCKS/DOC/CHANGELOG:@CHANGELOG. You can add ChangeLog page
into your RSS reader to be notified on new releases.

Removed Features
================

These features are present in the oldshool Weblocks version but were removed
from the core of the Reblocks. Some of them might be implemented as a separate ASDF
systems.

* Views
* Templates
* Forms
* Continuation based dialogs

You can read about these tools in the REBLOCKS/DOC/INDEX::@REMOVED-FEATURES section.

Learning Reblocks
=================

To learn, how to use Reblocks to build a simple single-page application, proceed to the
REBLOCKS/DOC/QUICKSTART:@QUICKSTART section.

To learn how a real-world application can be built,
inspect [Ultralisp's](https://ultralisp.org) code:

<https://github.com/ultralisp/ultralisp>

Getting Support
===============

* In case any bugs, please, [create an issue](https://github.com/40ants/reblocks/issues) on the GitHub.
* If you have some ideas to share or want to discuss a feature request, use [GitHub Discussions](https://github.com/40ants/reblocks/discussions).

")
