(defpackage #:reblocks/commands-hook
  (:use #:cl)
  (:import-from #:reblocks/hooks
                #:on-application-hook-handle-http-request
                #:call-next-hook)
  (:import-from #:reblocks/commands
                #:*commands*))
(in-package #:reblocks/commands-hook)


(on-application-hook-handle-http-request
  reset-commands-list (env)
  (let (*commands*)
    (call-next-hook)))
