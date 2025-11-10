(defpackage #:reblocks/doc/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:reblocks/doc/rendering
                #:@rendering)
  (:import-from #:reblocks/doc/error-handler
                #:@error-handling)
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
  (:import-from #:reblocks/doc/page
                #:@page)
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
  (:import-from #:reblocks/doc/commands
                #:@commands)
  (:import-from #:reblocks/doc/debug
                #:@debug)
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
                           @extensions
                           @components
                           @apps
                           @widgets
                           @rendering
                           @actions
                           @routing
                           @dependencies
                           @request
                           @response
                           @error-handling
                           @hooks
                           @page
                           @session
                           @debug
                           @commands
                           @removed-features
                           @api
                           @contribute
                           @changelog
                           @readme))))


(defsection @removed-features (:title "Removed Features")
  "During refactoring, I removed some other interesting concepts from
   the framework core. They could be reborn as separate libraries
   as well as the navigation widget."
  
  (@views section)
  (@templates section)
  (@forms section)
  (@continuations section))


(defsection @extensions (:title "Reblocks Extensions"
                         :ignore-words ("CSS"
                                        "LASS"
                                        "URL"
                                        "UI"))
  "There are a number of addons for Reblocks implementing additional widgets and features.

   Here is a list of these addons. If you know of another extension, just make a pull request and add it to the list!

   - [reblocks-auth](https://40ants.com/reblocks-auth/) - A system to add authentication. Stores information about users and lets them log in using different identity providers.
   - [reblocks-file-server](https://40ants.com/reblocks-file-server/) - A file browser for static.
   - [reblocks-lass](https://40ants.com/reblocks-lass/) - A helper to define CSS dependencies in LASS syntax.
   - [reblocks-navigation-widget](https://40ants.com/reblocks-navigation-widget/) - A widget which changes its children when a user goes to another URL.
   - [reblocks-parenscript](https://40ants.com/reblocks-parenscript/) - A utility to define JavaScript dependencies.
   - [reblocks-typeahead](https://40ants.com/reblocks-typeahead/) - A Typeahead widget.
   - [reblocks-ui](https://40ants.com/reblocks-ui/) - A set of UI widgets.
   - [reblocks-ui2](https://40ants.com/reblocks-ui2/) - The second version of UI toolkit (work in progress).
   - [reblocks-websocket](https://40ants.com/reblocks-websocket/) - Websocket support for Reblocks.

")
