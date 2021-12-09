(defpackage #:reblocks-test/request-handler
  (:use #:cl
        #:rove
        #:hamcrest/rove
        ;;        #:weblocks
        #:reblocks-test/utils)
  (:import-from #:reblocks/app
                #:defapp
                #:*current-app*)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks/request-handler
                #:handle-request)
  (:import-from #:reblocks/response
                #:get-code
                #:get-content
                #:get-custom-headers)
  
  ;; Just dependencies
  (:import-from #:reblocks/session))
(in-package reblocks-test/request-handler)


(defapp app-with-init
  :prefix "/test/my-app"
  :autostart nil)


(defmethod reblocks/session:init ((app app-with-init))
  (make-string-widget
   "Hello world"))


(deftest process-first-request
  (with-session
    (with-request ("/foo/bar" :app app-with-init)
      (let* ((response (handle-request *current-app*))
             (content (get-content response)))
        (ok (search "Hello world" content)
            "Result should have a greeting.")))))


(deftest process-request-with-missing-action
  (with-session
    (with-request ("/foo/bar?action=store-data" :app app-with-init)
      (let ((response (handle-request *current-app*)))
        (ok (equal (get-content response)
                   "")
            "Result should have an error message.")
        (ok (equal (get-code response)
                   302)
            "And response code should be 302")

        (testing "And user should be redirected to the app's prefix uri."
          (assert-that (get-custom-headers response)
                       (has-plist-entries :location "http://localhost/test/my-app")))))))

