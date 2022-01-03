(defpackage #:reblocks/doc/installation
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:@installation))
(in-package reblocks/doc/installation)


(defsection @installation (:title "Installation"
                           :ignore-words ("SBCL"
                                          "REPL"
                                          "Qlot"
                                          "UNIX"
                                          "ASDF"
                                          "SEXP"
                                          "CL"
                                          "IDE"
                                          "S-E"))
  "
# Setup your Common Lisp environment

You can skip this section if you already have a comfortable Common Lisp environment.


## Common Lisp implementation

First you must choose a Common Lisp implementation.

Reblocks is designed and implemented to be portable and should run on the most popular
Common Lisp implementations. It is currently tested best on SBCL and Clozure CL, though.


## Development environment setup

There are at least two fundamentally different development approaches for using
Common Lisp:

* Editor-centric development: you access all Lisp functions from within your editor.

  Example: Emacs/Slime, Vim/Slimv, Lispworks IDE.
    
  Incremental development happens mainly on the S-Expression level. This means that you
  edit a SEXP and send it with the help of editor directly to your Lisp image, which
  evaluates it, thus affecting the current Lisp environment.
  
* UNIX-style development: one tool for each job. The editor is not all that important
  here (as long as you're comfortable with it and it supports at least a basic level
  of paren highlighting.
    
  Example: Vim and your favorite terminal emulator. You start Vim in one window and
  your Lisp in another. Interaction happens by reloading your applications ASDF system
  and simple copy/paste of snippets.

We will try to be largely agnostic of the development approach in this manual which actually
means that we tend towards the second approach: Lisp calls are referred to by what you'd
type in your REPL, not by Emacs shortcut as it is often the case.

For a basic comfortable SBCL setup, see :ref:`Basic SBCL setup`.
 

# Installation

> **Note**
>
> The official Quicklisp distribution contains an old Weblocks
> version.
>
> This documentation is about newer Reblocks fork.
> It is only available on the Ultralisp.org for a while.

The easiest way to install Reblocks and additional libraries is to use
<https://ultralisp.org> distribution. It is Quicklisp compatible. To
install Reblocks from Ultralisp, do eval these commands in your REPL:

```
CL-USER> (ql-dist:install-dist \"http://dist.ultralisp.org/\"
                               :prompt nil)
CL-USER> (ql:quickload :reblocks)
```

If you are using [Qlot][Qlot] to pin versions, you can add such lines into your
`qlfile`

```
dist ultralisp http://dist.ultralisp.org/
ql :all :latest
ultralisp :all :latest
```

Run `qlot update` after these changes, and new Reblocks will be
available to install.


[Qlot]: https://github.com/fukamachi/qlot

")
