(defpackage #:weblocks/commands-hook
  (:use #:cl)
  (:import-from #:weblocks/hooks
                #:on-application-hook-handle-http-request
                #:call-next-hook)
  (:import-from #:weblocks/commands
                #:*commands*))
(in-package weblocks/commands-hook)


(on-application-hook-handle-http-request
  reset-commands-list (env)
  (let (*commands*)
    (call-next-hook)))
