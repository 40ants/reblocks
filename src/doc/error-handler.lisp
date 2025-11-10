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
When an error happens, Reblocks either renders an error page or opens an interactive debugger.

The interactive debugger is opened only if the server is running in debug mode. Also, in this mode,
the error page contains a backtrace.

If customizing how the error page looks is desired, define a method for
the ON-ERROR generic function.
"
  ;; (on-error generic-function)
  )
