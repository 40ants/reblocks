(defpackage #:weblocks/doc/intro
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:@intro))
(in-package weblocks/doc/intro)


(defsection @intro (:title "Introduction")
  "
You can get support in our Gitter Chat:

<https://gitter.im/40ants/weblocks>
")
