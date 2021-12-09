(defpackage #:reblocks-test/widgets/mop
  (:use #:cl
        #:rove)
  (:import-from #:reblocks/widget
                #:widget)
  (:import-from #:closer-mop
                #:effective-slot-definition-class
                #:direct-slot-definition-class)
  (:import-from #:reblocks/widgets/mop
                #:widget-effective-slot-definition
                #:widget-direct-slot-definition))
(in-package reblocks-test/widgets/mop)


(deftest test-direct-slot-definition-class-widget
  (ok (eq (class-name (direct-slot-definition-class (find-class 'widget)))
          'widget-direct-slot-definition)))

(deftest test-effective-slot-definition-class-widget
  (ok (eq (class-name (effective-slot-definition-class (find-class 'widget)))
          'widget-effective-slot-definition)))
