(uiop:define-package #:reblocks-tests/html
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:reblocks/html
                #:with-html
                #:with-html-string))
(in-package #:reblocks-tests/html)


(deftest test-by-default-html-is-pretty-printed ()
  (let ((result (with-html-string ()
                  (:link :rel "stylesheet"
                         :type "text/css"
                         :href "/static/css/some.css"
                         :media "screen")))
        (expected
          "<link rel=stylesheet type=\"text/css\"
      href=\"/static/css/some.css\" media=screen>"))
    (ok (equal result expected))))


(deftest test-nested-with-html-without-pretty-print ()
  (let ((result (with-html-string (:pretty nil)
                  (with-html ()
                    (:link :rel "stylesheet"
                           :type "text/css"
                           :href "/static/css/some.css"
                           :media "screen"))))
        (expected
          "<link rel=stylesheet type=\"text/css\" href=\"/static/css/some.css\" media=screen>"))
    (ok (equal result expected))))


(deftest test-pretty-print-override ()
  (let ((result (with-html-string (:pretty nil)
                  (with-html (:pretty t)
                    (:link :rel "stylesheet"
                           :type "text/css"
                           :href "/static/css/some.css"
                           :media "screen"))))
        ;; Result should be indented:
        (expected
          "<link rel=stylesheet type=\"text/css\"
      href=\"/static/css/some.css\" media=screen>"))
    (ok (equal result expected))))
