(defpackage #:weblocks-test/default-init
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:weblocks-test/utils)
  (:import-from #:reblocks/app
                #:defapp))
(in-package weblocks-test/default-init)


(deftest default-init-method-should-return-string-widget
  (defapp app)
  
  (with-session
    (with-request ("/" :app app)
      (let ((root (reblocks/session:init reblocks/app::*current-app*)))
        (ok (typep root
                   'reblocks/widget:widget)
            "Default init method should return a widget")))))
