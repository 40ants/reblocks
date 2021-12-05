(defpackage #:weblocks-test/weblocks
  (:use #:cl
        #:rove
        #:weblocks-test/utils)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/js
                #:with-javascript
                #:with-javascript-to-string))
(in-package weblocks-test/weblocks)


(deftest with-javascript-1
  (ok
   (equal (with-html-string
            (with-javascript
              "foo~A" "bar"))
          (with-javascript-to-string "foo~A" "bar"))
      
   "Macro with-javascript should write into weblocks.html:*stream*"))

