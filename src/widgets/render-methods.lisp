(defpackage #:reblocks/widgets/render-methods
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:reblocks/dependencies
                #:get-dependencies
                #:render-in-ajax-response)
  (:import-from #:reblocks/page-dependencies
                #:push-dependencies)
  (:import-from #:reblocks/page
                #:register-widget)
  (:import-from #:reblocks/widget
                #:get-css-classes-as-string
                #:get-html-tag
                #:render)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:import-from #:log))
(in-package #:reblocks/widgets/render-methods)


(defmethod render (widget)
  "By default, widget rendered with a text, suggesting to define a rendering method."
  (let ((class-name (class-name (class-of widget))))
    (with-html
      (:p "Please, define:"
          (:pre (format nil
                        "(defmethod reblocks/widget:render ((widget ~a))
    (reblocks/html:with-html
        (:p \"My ~a widget\")))"
                        class-name
                        class-name))))))


(defmethod render :around (widget)
  "This function is intended for internal usage only.
   It renders widget with surrounding HTML tag and attributes."
  (check-type widget reblocks/widget:widget)
  
  (let ((widget-dependencies (get-dependencies widget)))
    ;; Update new-style dependencies
    (push-dependencies widget-dependencies))

  (register-widget widget)
  
  (with-html
    (:tag
     :name (get-html-tag widget)
     :class (get-css-classes-as-string widget)
     :id (dom-id widget)
     (call-next-method))))


