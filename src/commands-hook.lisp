(uiop:define-package #:reblocks/commands-hook
  (:use #:cl)
  (:import-from #:reblocks/hooks
                #:on-application-hook-handle-http-request
                #:call-next-hook)
  (:import-from #:reblocks/commands
                #:with-collected-commands))
(in-package #:reblocks/commands-hook)


(on-application-hook-handle-http-request
    reset-commands-list (env)
  (with-collected-commands ()
    (call-next-hook)))
