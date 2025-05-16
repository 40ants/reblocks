(uiop:define-package #:reblocks/doc/debug
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/debug
                #:reset-latest-session
                #:*latest-request*
                #:*latest-session*
                #:get-session-value
                #:off
                #:on
                #:status))
(in-package #:reblocks/doc/debug)


(defsection @debug (:title "Debugging Reblocks"
                    :ignore-words ("REBLOCKS"
                                   "HTTP"))
  "# Performance

   Reblocks return response timing in the Server-Timing HTTP header.
   This performance information can be viewed in the browser.
   Read more about it at [Mozilla's docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server-Timing).
"

  ;; "# API"
  ;; (status function)
  ;; (on function)
  ;; (off function)
  ;; (reset-latest-session function)
  ;; (get-session-value function)
  
  ;; (*latest-request* variable)
  ;; (*latest-session* variable)
  )

