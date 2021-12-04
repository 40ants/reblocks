(defpackage #:weblocks/doc/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:weblocks/html
                #:@html)
  (:import-from #:weblocks/doc/intro
                #:@intro)
  (:import-from #:weblocks/doc/installation
                #:@installation)
  (:import-from #:weblocks/doc/components
                #:@components)
  (:import-from #:weblocks/doc/quickstart
                #:@quickstart)
  (:import-from #:weblocks/doc/routing
                #:@routing)
  (:import-from #:weblocks/doc/api
                #:@api)
  (:import-from #:weblocks/doc/widgets
                #:@widgets)
  (:import-from #:weblocks/doc/dependencies
                #:@dependencies)
  (:import-from #:weblocks/doc/hooks
                #:@hooks)
  (:import-from #:weblocks/doc/forms
                #:@forms)
  (:import-from #:weblocks/doc/continuations
                #:@continuations)
  (:import-from #:weblocks/doc/views
                #:@views)
  (:import-from #:weblocks/doc/templates
                #:@templates)
  (:import-from #:weblocks/doc/contribute
                #:@contribute)
  (:import-from #:weblocks/doc/session
                #:@session)
  (:import-from #:weblocks/doc/actions
                #:@actions)
  (:import-from #:weblocks/doc/app
                #:@apps)
  (:import-from #:weblocks/doc/response
                #:@response)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme))
(in-package weblocks/doc/index)


(defsection @index (:title "Contents")
  "
- @INTRO
- @INSTALLATION
- @QUICKSTART
- @COMPONENTS
- @APPS
- @WIDGETS
- @HTML
- @ACTIONS
- @ROUTING
- @DEPENDENCIES
- @RESPONSE
- @HOOKS
- @SESSION
- @REMOVED-FEATURES
- @API
- @CONTRIBUTE
")

(defsection-copy @readme @quickstart)


(defmethod docs-config ((system (eql (asdf:find-system "weblocks-docs"))))
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
          (unless weblocks/doc/example::*port*
            "https://com-40ants-weblocks-examples.herokuapp.com/examples")))
    (list :theme
          (find-symbol "40ANTS-THEME"
                       (find-package "40ANTS-DOC-THEME-40ANTS"))
          :dynamic-bindings (list (cons 'weblocks/doc/example:*server-url*
                                        server-url))
          :root-sections '(@index
                           @intro
                           @installation
                           @quickstart
                           @components
                           @apps
                           @widgets
                           @html
                           @actions
                           @routing
                           @dependencies
                           @response
                           @hooks
                           @session
                           @removed-features
                           @api
                           @contribute
                           @readme))))


(defsection @removed-features (:title "Removed Features")
  "During refactoring, I've removed some other interesting conceptions from
   the core of the framework. They could be reborn as a separate libraries
   as well as navigation widget."
  
  (@views section)
  (@templates section)
  (@forms section)
  (@continuations section))
