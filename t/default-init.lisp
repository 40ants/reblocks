(defpackage #:reblocks-tests/default-init
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:reblocks-tests/utils)
  (:import-from #:reblocks/app
                #:defapp))
(in-package #:reblocks-tests/default-init)


(deftest default-init-method-should-return-string-widget
  (defapp app)
  
  (with-session
    (with-request ("/" :app app)
      (let ((root (reblocks/session:init reblocks/app::*current-app*)))
        (ok (typep root
                   'reblocks/widget:widget)
            "Default init method should return a widget")))))
