(uiop:define-package #:reblocks/doc/error-handler
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/error-handler
                #:on-error))
(in-package #:reblocks/doc/error-handler)


(defsection @error-handling (:title "Error Handling"
                             :ignore-words ())
  "
When error happens, Reblocks either renders an error page or opens an interactive debugger.

Interactive debugger is opened only if server is running in debug mode. Also, in this mode,
error page contains a backtrace.

If you want to customize how does error page look like, define a method for
ON-ERROR generic-function.
"
  ;; (on-error generic-function)
  )
