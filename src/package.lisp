(defpackage #:weblocks-cont
    (:documentation "Operators for continuation-based web development
    with Weblocks."))

(defpackage #:weblocks
  (:use #:cl
        #:c2mop
        #:metabang.utilities
        #:json
        #:optima
        #:cont
        #:parenscript
        #:anaphora
        #:f-underscore
        #:trivial-timeout 
        ;; #:weblocks-stores 
        ;; #:weblocks-util
        )
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
                          #:standard-generic-function #:ensure-generic-function
                          #:standard-class #:typep #:subtypep #:standard-method)
  (:shadowing-import-from :f-underscore #:f #:_)
  (:shadowing-import-from :optima #:match)
  (:shadowing-import-from :metabang.utilities #:with-array #:size #:bind)
  (:shadowing-import-from :json #:prototype)
  ;; (:shadowing-import-from :weblocks-util #:find-all)
  (:shadow #:redirect #:reset-sessions #:errors #:create-regex-dispatcher #:create-prefix-dispatcher #:create-folder-dispatcher-and-handler #:create-static-file-dispatcher-and-handler)
  (:export #:defstore #:asdf-system-directory #:id #:persist-object
           #:initialize-webapp)
  (:documentation
    "Weblocks is a Common Lisp framework that eases the pain of web
    application development. It achieves its goals by standardizing on
    various libraries, providing flexible and extensible generic views,
    and exposing a unique widget-based approach to maintaining UI
    state."))

  ;; the following are export-only; see `wexport'

  
