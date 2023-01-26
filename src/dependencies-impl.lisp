(uiop:define-package #:reblocks/dependencies-impl
  (:use #:cl)
  (:import-from #:reblocks/response
                #:send-script)
  (:import-from #:parenscript
                #:ps*)
  (:import-from #:reblocks/dependencies
                #:dependency
                #:get-url
                #:get-type))
(in-package #:reblocks/dependencies-impl)


(defmethod reblocks/dependencies:render-in-ajax-response ((dependency dependency))
  (let ((url (get-url dependency)))
    (case (get-type dependency)
      (:js
       (let ((script (ps* `(include_dom ,url))))
         (log:debug "Rendering js dependency in ajax response" dependency)
         (send-script script :before-load)))

      (:css
       (let ((script (ps* `(include_css ,url))))
         (log:debug "Rendering css dependency in ajax response" dependency)
         (send-script script :before-load))))))

