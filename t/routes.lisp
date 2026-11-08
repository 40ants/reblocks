(defpackage #:reblocks-tests/routes
  (:use #:cl
        #:rove
        #:reblocks-tests/utils)
  (:import-from #:reblocks/routes
                #:mount-route
                #:mount-route-app
                #:serve)
  (:import-from #:reblocks-tests/utils
                #:*current-env*)
  (:import-from #:40ants-routes/route
                #:route-name
                #:route-method
                #:url-path)
  (:import-from #:40ants-routes/url-pattern
                #:url-pattern-pattern))
(in-package #:reblocks-tests/routes)


(deftest defining-a-mount-route
  (let ((app (lambda (env)
               (list 200
                     (list :content-type "text/plain")
                     (list env)))))
    (testing "With default arguments"
      (let ((route (mount-route "/api/" app)))
        (ok (typep route 'mount-route)
            "Result should be of MOUNT-ROUTE class.")

        (ok (equal (url-pattern-pattern (url-path route))
                   "/api/")
            "Route's pattern should correspond to the pattern given to the constructor.")

        (ok (eq (mount-route-app route)
                app)
            "MOUNT-ROUTE-APP reader should return the function given to the constructor.")

        (ok (equal (route-method route)
                   :get)
            "Default HTTP method should be :GET.")

        (ok (and (stringp (route-name route))
                 (route-name route))
            "By default route should have a gensym name.")))

    (testing "With custom arguments"
      (let ((route (mount-route "/api/" app
                                :method :post
                                :name "my-api")))
        (ok (equal (route-method route)
                   :post)
            "HTTP method should be customizable.")

        (ok (equal (route-name route)
                   "my-api")
            "Route name should be customizable.")))))


(deftest serving-a-mount-route
  (let* ((env '(:path-info "/api/" :request-method :get))
         (app (lambda (env)
                (list 200
                      (list :content-type "text/plain")
                      (list env))))
         (route (mount-route "/api/" app))
         (response (serve route env)))
    (ok (equal (first response)
               200)
        "Status code should be returned by the Clack app.")

    (ok (equal (getf (second response)
                     :content-type)
               "text/plain")
        "Headers should be returned by the Clack app.")

    (ok (equal (third response)
               (list env))
        "Clack app should be called with the env given to SERVE.")))


(deftest serving-a-mount-route-with-real-env
  (with-test-session ()
    (with-request ("/api/")
      (let* ((app (lambda (env)
                    (list 200
                          nil
                          (list (getf env :path-info)))))
             (route (mount-route "/api/" app))
             (response (serve route *current-env*)))
        (ok (equal (third response)
                   (list "/api/"))
            "Clack app should receive a real Lack env with correct :PATH-INFO.")))))
