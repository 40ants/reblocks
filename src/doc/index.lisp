(defpackage #:reblocks/doc/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:reblocks/doc/rendering
                #:@rendering)
  (:import-from #:reblocks/doc/intro
                #:@intro)
  (:import-from #:reblocks/doc/installation
                #:@installation)
  (:import-from #:reblocks/doc/components
                #:@components)
  (:import-from #:reblocks/doc/quickstart
                #:@quickstart)
  (:import-from #:reblocks/doc/routing
                #:@routing)
  (:import-from #:reblocks/doc/api
                #:@api)
  (:import-from #:reblocks/doc/widgets
                #:@widgets)
  (:import-from #:reblocks/doc/dependencies
                #:@dependencies)
  (:import-from #:reblocks/doc/hooks
                #:@hooks)
  (:import-from #:reblocks/doc/forms
                #:@forms)
  (:import-from #:reblocks/doc/continuations
                #:@continuations)
  (:import-from #:reblocks/doc/views
                #:@views)
  (:import-from #:reblocks/doc/templates
                #:@templates)
  (:import-from #:reblocks/doc/contribute
                #:@contribute)
  (:import-from #:reblocks/doc/session
                #:@session)
  (:import-from #:reblocks/doc/actions
                #:@actions)
  (:import-from #:reblocks/doc/app
                #:@apps)
  (:import-from #:reblocks/doc/response
                #:@response)
  (:import-from #:reblocks/doc/request
                #:@request)
  (:import-from #:reblocks/doc/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme))
(in-package #:reblocks/doc/index)


(defsection-copy @index @intro)
(defsection-copy @readme @intro)


(defmethod docs-config ((system (eql (asdf:find-system "reblocks-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)

  (let ((server-url
          ;; When local examples server is running,
          ;; we'll be using it instead of production:
          (unless reblocks/doc/example::*port*
            "https://com-40ants-reblocks-examples.herokuapp.com/examples")))
    (list :theme
          (find-symbol "40ANTS-THEME"
                       (find-package "40ANTS-DOC-THEME-40ANTS"))
          :dynamic-bindings (list (cons 'reblocks/doc/example:*server-url*
                                        server-url))
          :root-sections '(@index
                           @installation
                           @quickstart
                           @components
                           @apps
                           @widgets
                           @rendering
                           @actions
                           @routing
                           @dependencies
                           @request
                           @response
                           @hooks
                           @session
                           @removed-features
                           @api
                           @contribute
                           @changelog
                           @readme))))


(defsection @removed-features (:title "Removed Features")
  "During refactoring, I've removed some other interesting conceptions from
   the core of the framework. They could be reborn as a separate libraries
   as well as navigation widget."
  
  (@views section)
  (@templates section)
  (@forms section)
  (@continuations section))
