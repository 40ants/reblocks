(defpackage #:reblocks-tests/request-handler
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:reblocks-tests/utils)
  (:import-from #:reblocks/app
                #:defapp
                #:*current-app*)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  ;; (:import-from #:reblocks/request-handler
  ;;               #:handle-request)
  (:import-from #:reblocks/response
                #:get-code
                #:get-content
                #:get-custom-headers)
  
  ;; Just dependencies
  (:import-from #:reblocks/session)
  (:import-from #:reblocks-tests/utils
                #:*current-env*))
(in-package #:reblocks-tests/request-handler)


(defapp app-with-init
  :prefix "/test/my-app"
  :autostart nil)


(defmethod reblocks/session:init ((app app-with-init))
  (make-string-widget
   "Hello world"))


(deftest process-first-request
  (with-test-session () `
    (with-request ("/foo/bar" :app app-with-init)
      ;; TODO: replace handle-request
      (let* ((route (make-instance 'reblocks/routes::page-route))
             (content (reblocks/routes:serve route *current-env*)))
        (ok (search "Hello world" content)
            "Result should have a greeting.")))))


(deftest process-request-with-missing-action
  (with-test-session ()
    (with-request ("/foo/bar?action=store-data" :app app-with-init)
      (let* ((reblocks/variables:*ignore-missing-actions* t)
             (route (make-instance 'reblocks/routes::page-route)))

        (handler-case
            (progn
              (reblocks/routes:serve route *current-env*)
              (rove:ok NIL
                       "REDIRECT condition was not signalled"))
          
          (reblocks/response:redirect (err)
            (let ((response (reblocks/response:get-response err)))
              (ok (equal (get-content response)
                         "")
                  "Result should have an error message.")
              (ok (equal (get-code response)
                         302)
                  "And response code should be 302")

              (testing "And user should be redirected to the same page, but without action parameter."
                (assert-that (get-custom-headers response)
                             (has-plist-entries :location "http://localhost/foo/bar?"))))))))))

