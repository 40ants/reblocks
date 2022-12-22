(defpackage #:reblocks-test/dependencies
  (:use #:cl
        ;; #:cl-mock
        ;; #:prove
        #:rove
        ;;        #:hamcrest/rove
        #:reblocks-test/utils)
  (:import-from #:cl-mock
                #:invocations
                #:with-mocks
                #:answer)
  (:import-from #:reblocks/page-dependencies
                #:push-dependency
                #:get-collected-dependencies
                #:with-collected-dependencies)
  (:import-from #:reblocks/dependencies
                #:*cache-remote-dependencies-in*
                #:make-dependency
                #:infer-type-from
                #:get-type
                #:get-path
                #:get-url
                #:get-content-type
                #:get-route
                #:serve
                #:render-in-head)
  (:import-from #:str
                #:ends-with-p))
(in-package reblocks-test/dependencies)


(deftest infer-type
  (testing "Correct inferring"
    (ok (equal (infer-type-from "/some/path/to.css")
               :css)
        "Inferring type from path to css file")

    (ok (equal (infer-type-from "/some/path/to.js")
               :js)
        "Inferring type from path to js file")
    
    (ok (equal (infer-type-from "http://example.com/some/file.css")
               :css)
        "Inferring type from url to css file")

    (ok (equal (infer-type-from "http://example.com/some/file.js")
               :js)
        "Same for javascript URL"))

  (testing "Incorrect inferring"
      (ok (signals (infer-type-from "/some/path/unknown"))
          "inferring type from path to unknown file should raise an error")))


(deftest creating-local-js-dependency
  (let ((dependency (make-dependency #P"t/data/some.js" :system "reblocks-test")))
    (testing "method get-path should return a local pathname"
      (ok (uiop:absolute-pathname-p (get-path dependency)))
      (ok (ends-with-p "/t/data/some.js"
                       (namestring (get-path dependency)))))
    (ok (equal (get-type dependency)
               :js)
        "dependency's type should be :js")
    (ok (equal (get-content-type dependency)
               "application/javascript")
        "content type should be application/javascript")

    (let ((route (get-route dependency)))
      (ng (equal route nil)
          "local dependency should have a lisp webserver's route.")
      (when route
        (ok (equal (routes:route-template route)
                   (list "static" "js" "some.js"))
            "Route should point to a same filename in a special root for static files."))))

  (testing "Also, local dependency can be created using a string."
    (let ((dependency (make-dependency "t/data/some.js" :system "reblocks-test")))
      (ok (uiop:absolute-pathname-p (get-path dependency)))
      (ok (ends-with-p "/t/data/some.js"
                       (namestring (get-path dependency)))))))


(deftest when-caching-is-turned-off
  (testing "If *cache-remote-dependencies-in* is nil, then:"
    (let* ((*cache-remote-dependencies-in* nil)
           (original-url "http://ya.ru/some.css")
           (dependency (make-dependency
                         original-url)))
      (ok (signals (get-path dependency))
          "Method get-path should raise a condition.")

      (ok (equal (get-url dependency)
                 original-url)
          "And URL should point to remote resource."))))


(deftest serving-from-local-cache
  (testing "Remote dependency can be served from local cache directory."
    (let* ((original-url "http://ya.ru/some.css")
           (random-string (cl-fad::generate-random-string))
           (*cache-remote-dependencies-in*
             (pathname (format nil "/tmp/reblocks-cache/unittests/~A/"
                               random-string)))
           (dependency (make-dependency
                         original-url))
           (resulting-path (get-path dependency))
           (resulting-url (get-url dependency)))
      
      (unwind-protect
           (progn (ok (equal (pathname-directory resulting-path)
                             `(:absolute "tmp" "reblocks-cache" "unittests" ,random-string))
                      "When *cache-remote-dependencies-in* is set to a path, get-path should return local path.")

                  (ok (equal resulting-url
                             (concatenate 'string
                                          "/remote-deps-cache/"
                                          (pathname-name resulting-path)))
                      "And URL should be local too.")

                  (with-mocks ()
                    (flexi-streams:with-input-from-sequence (remote-file
                                                             (babel:string-to-octets "File content"))
                      ;; dexador should return a stream
                      (answer dex:get remote-file)

                      (let ((result (serve dependency)))
                        (ok (equal (pathname-directory result)
                                   `(:absolute "tmp" "reblocks-cache" "unittests" ,random-string))
                            "Call to (serve) should return a path to a cached file.")

                        (ok (equal (length (invocations 'dex:get))
                                   1)
                            "First call should \"download\" content.")

                        (ok (cl-fad:file-exists-p result)
                            "File should exists in a cache directory.")))

                    ;; Now call it second time
                    (serve dependency)

                    ;; And check if it was downloaded again
                    (ok (equal (length (invocations 'dex:get))
                               1)
                        "Dependency shouldn't fetch file from the internet again.")))
        ;; Clean up tmp directory
        (cl-fad:delete-directory-and-files *cache-remote-dependencies-in*)))))


(deftest render-css-dependency
  (let ((dependency (make-dependency "t/data/some.css" :system "reblocks-test")))
    (is-html (render-in-head dependency)
             "<link rel=stylesheet type=text/css href=/static/css/some.css media=screen>"
             "Local CSS dependency should be rendered as a link tag."))

  (let* ((*cache-remote-dependencies-in* nil)
         (dependency (make-dependency "https://example.com/some.css")))
    (is-html (render-in-head dependency)
             "<link rel=stylesheet type=text/css href=https://example.com/some.css media=screen>"
             "Remote CSS dependency should be rendered with remote url if caching is turned off."))

  (let* ((*cache-remote-dependencies-in* "/tmp/cache/")
         (dependency (make-dependency "https://example.com/some.css")))
    (is-html (render-in-head dependency)
             "<!-- https://example.com/some.css --><link rel=stylesheet type=text/css href=/remote-deps-cache.* media=screen>"
             "Remote CSS dependency should be rendered with local if caching is turned on.")))


(deftest render-js-dependency
  (let ((dependency (make-dependency "t/data/some.js" :system "reblocks-test")))
    (is-html (render-in-head dependency)
             "<script src=/static/js/some.js type=text/javascript></script>"
             "JS dependency should be rendered as a script tag.")))


(deftest dont-render-png-dependency
  (testing "Png dependency should not be rendered"
    (let ((dependency (make-dependency "t/data/lisp.png" :system "reblocks-test")))
      (is-html (render-in-head dependency)
               ""
               "Methods render-in-header works only for js/css dependencies."))))


(deftest defining-a-route
  (testing "Cached remote dependency should define a route"
    (let* ((*cache-remote-dependencies-in* nil)
           (dependency (make-dependency "https://example.com/some.css")))
      (ok (typep (get-route dependency)
                 'null)
          "Non cached dependency shouldn't have a route."))
    
    (let* ((*cache-remote-dependencies-in* "/tmp/cache/")
           (dependency (make-dependency "https://example.com/some.css")))
      (ok (get-route dependency)
          "Cached dependency should have a route."))))


(deftest deduplication
  (testing "Dependencies should be deduplicated on collection."
    (with-collected-dependencies
      (push-dependency (make-dependency #P"t/data/some.js" :system "reblocks-test"))
      (push-dependency (make-dependency #P"t/data/some.js" :system "reblocks-test"))
      
      (let ((collected (get-collected-dependencies)))
        (ok (equal (length collected)
                   1))))))

