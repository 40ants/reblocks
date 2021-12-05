(defpackage #:weblocks-test/widgets/render-methods
  (:use #:cl
        #:rove)
  (:import-from #:reblocks/widget
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:weblocks-test/utils
                #:is-html))
(in-package weblocks-test/widgets/render-methods)

(defwidget simple-widget ()
  ())


(defmethod render ((widget simple-widget))
  (with-html
    (:p "Hello world")))


(defwidget widget-with-custom-tag (simple-widget)
  ())


(defmethod get-html-tag ((widget widget-with-custom-tag))
  :h1)


(deftest test-widget-rendering-as-a-div
  (let ((widget (make-instance 'simple-widget)))
    (is-html (render widget)
             "<div class=\"widget simple-widget\"><p>Hello world</div>")))


(deftest test-widget-with-custom-tag
  (let ((widget (make-instance 'widget-with-custom-tag)))
    (is-html (render widget)
             "<h1 class=\"widget widget-with-custom-tag\"><p>Hello world</h1>")))


(defwidget widget-inside-table ()
  ())

(defmethod render ((widget widget-inside-table))
  (with-html
    (:p "foo bar")))

(deftest test-widget-nested-into-table
  (let ((widget (make-instance 'widget-inside-table)))
    (is-html
     (with-html
       (:table
        (:tr
         (:td
          (render widget)))))
     "<table><tr><td><div class=\"widget widget-inside-table\"><p>foo bar</div></table>"
     )))


(deftest test-widget-table-child
  (let ((widget (make-instance 'widget-inside-table)))
    (is-html
     (with-html
       (:table
        (render widget)))
     "<table><tr class=\"widget widget-inside-table\"><p>foo bar</tr></table>")))

(deftest test-widget-tr-child
  (let ((widget (make-instance 'widget-inside-table)))
    (is-html
     (with-html
       (:table
        (:tr
         (render widget))))
     "<table><tr><td class=\"widget widget-inside-table\"><p>foo bar</td></table>")))
