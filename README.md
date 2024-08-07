<a id="x-28REBLOCKS-2FDOC-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Introduction

Reblocks is the fork of the Weblocks web frameworks written by Slava Akhmechet
and maintained by Scott L. Burson and Olexiy Zamkoviy.

Some time ago I (Alexander Artemenko) found this old Weblocks and liked its idea
of building web application out of widgets. But framework's code was complex
and hard to change. Thus I decided to refactor it into a smaller core plus
separate libraries implementing more advanced features.

A few years this refactored code lived in the repository https://github.com/40ants/weblocks and
it was possible to install it from Ultralisp only. I didn't publish it
to Quicklisp to not break sites using old version of the Weblocks. But at January 2022
I decided to rename `ASDF` system into the "reblocks" to make it installable from
the Quicklisp under this name.

<a id="core-features-of-reblocks"></a>

## Core Features of Reblocks

* Application pages are built from "widgets".
* All business logic could be implemented in Common Lisp in server-side.
* Framework's code is separated by functionality into a separate packages.
* Project now has a [hyperlinked documentation][22e2]
  with code snippets and embedded interactive demos.

Framework's `API` is still under active development. All significant changes are
described in the [`ChangeLog`][c52e]. You can add ChangeLog page
into your `RSS` reader to be notified on new releases.

<a id="removed-features"></a>

## Removed Features

These features are present in the oldshool Weblocks version but were removed
from the core of the Reblocks. Some of them might be implemented as a separate `ASDF`
systems.

* Views
* Templates
* Forms
* Continuation based dialogs

You can read about these tools in the [`Removed Features`][1505] section.

<a id="learning-reblocks"></a>

## Learning Reblocks

To learn, how to use Reblocks to build a simple single-page application, proceed to the
[`Quickstart`][4851] section.

To learn how a real-world application can be built,
inspect [Ultralisp's][2a0d] code:

[https://github.com/ultralisp/ultralisp][8e2a]

<a id="getting-support"></a>

## Getting Support

* In case any bugs, please, [create an issue][f11d] on the GitHub.
* If you have some ideas to share or want to discuss a feature request, use [GitHub Discussions][562e].


[22e2]: https://40ants.com/reblocks/
[c52e]: https://40ants.com/reblocks/changelog/#x-28REBLOCKS-2FDOC-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[4851]: https://40ants.com/reblocks/quickstart/#x-28REBLOCKS-2FDOC-2FQUICKSTART-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[1505]: https://40ants.com/reblocks/removed-features/#x-28REBLOCKS-2FDOC-2FINDEX-3A-3A-40REMOVED-FEATURES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[562e]: https://github.com/40ants/reblocks/discussions
[f11d]: https://github.com/40ants/reblocks/issues
[8e2a]: https://github.com/ultralisp/ultralisp
[2a0d]: https://ultralisp.org

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
