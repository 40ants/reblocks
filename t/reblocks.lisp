(defpackage #:reblocks-tests/reblocks
  (:use #:cl
        #:rove
        #:reblocks-tests/utils)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/js
                #:with-javascript
                #:with-javascript-to-string))
(in-package #:reblocks-tests/reblocks)


(deftest with-javascript-1
  (ok
   (equal (with-html-string
            (with-javascript
              "foo~A" "bar"))
          (with-javascript-to-string "foo~A" "bar"))
      
   "Macro with-javascript should write into reblocks.html:*stream*"))

