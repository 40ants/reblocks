(defpackage #:weblocks-test/response
  (:use #:cl
        #:rove
        #:weblocks-test/utils)
  (:import-from #:reblocks/response
                #:make-uri
                #:add-retpath-to))
(in-package weblocks-test/response)


(deftest make-uri-test
  (with-session
    (with-request ("http://example.com:10050/foo/bar")
      (ok (equal (make-uri "/login")
                 "http://example.com:10050/login")
          "Absolute URI should substitute whole path in original URL.")

      (ok (equal (make-uri "/login?code=100500")
                 "http://example.com:10050/login?code=100500")
          "Target URI can contain parameters."))))


(deftest relative-make-uri
  (with-session
    (with-request ("http://example.com:10050/foo/bar")
      (ok (equal (make-uri "minor")
                 "http://example.com:10050/foo/minor")
          "Relative path substitutes top level path component if it is not ended with slash")))

  (with-session
    (with-request ("http://example.com:10050/foo/bar/")
      (ok (equal (make-uri "minor")
                 "http://example.com:10050/foo/bar/minor")
          "Relative path added to the base path if it is ended with slash")))

  (with-session
    (with-request ("http://example.com:10050/foo/bar/")
      (ok (equal (make-uri "../minor")
                 "http://example.com:10050/foo/minor")
          "Two dots can be used to go to the upper level of the path"))))


(deftest add-retpath-to-test
  (with-session
    (with-request ("http://example.com:10050/foo/bar")
      (ok (equal (add-retpath-to "/login")
                 "/login?retpath=http%3A%2F%2Fexample.com%3A10050%2Ffoo%2Fbar")
          "Retpath should be added as a single parameter.")
      
      (ok (equal (add-retpath-to "/login?retpath=%2Fprojects")
                 "/login?retpath=http%3A%2F%2Fexample.com%3A10050%2Ffoo%2Fbar")
          "Existing retpath should be overwritten.")
      
      (ok (equal (add-retpath-to "/login?code=100500&retpath=%2Fprojects")
                 "/login?code=100500&retpath=http%3A%2F%2Fexample.com%3A10050%2Ffoo%2Fbar")
          "Existing parameters should be leaved as is."))))
