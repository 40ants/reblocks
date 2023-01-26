(defpackage #:reblocks/default-init
  (:use #:cl)
  (:import-from #:reblocks/app
                #:get-prefix
                #:webapp-name
                #:defapp)
  (:import-from #:reblocks/page
                #:page-root-widget
                #:page)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks/app
                #:*current-app*)
  (:import-from #:spinneret/cl-markdown))
(in-package #:reblocks/default-init)


(defun %common-init (app)
  (declare (ignore app))
  (let ((quickstart-url "http://40ants.com/reblocks/quickstart/"))
    (make-string-widget
     (with-html-string
       (:h1 "No reblocks/page:init-page method defined.")
       (:p "Please define a method reblocks/page:init-page to initialize a new page.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         (format nil "
CL-USER> (defmethod reblocks/page:init-page ((app ~A) (url-path string) (expire-at local-time:timestamp))
           \"Hello world!\")" (string-downcase
                               (type-of *current-app*)))))
       (:p "And reset current session:")
       (:pre
        (:code
         "
CL-USER> (reblocks/debug:reset-latest-session)"))

       (:p "Then reload the page.")
       (:p ("Read more in [documentaion](~A)."
            quickstart-url)))
     :escape-p nil)))


;; TODO: this generic function is deprecated, remove after 2023-07-01
(defmethod reblocks/session:init ((app t))
  (warn "Use REBLOCKS/PAGE:INIT instead of REBLOCKS/SESSION:INIT. This method will be removed after 2023-07-01.")
  (%common-init app))


(defmethod reblocks/page:init-page ((app t) (path string) expire-at)
  ;; TODO: replace with content of %common-init after deprecation
  (%common-init app))


(defmethod reblocks/page:init-page :around ((app t) (path string) expire-at)
  "If init function returned not object inherited from widget, it calls
   create-widget-from method, to transform value into the widget."

  (let ((result (call-next-method)))
    (flet ((make-page (widget)
             (make-instance 'page
                            :root-widget widget
                            :path path
                            :expire-at expire-at)))
      (etypecase result
        (page
         (unless (page-root-widget result)
           (error "Page ~A should have slot ROOT-WIDGET bound."
                  result))
         result)
        (widget (make-page result))
        (t (make-page (create-widget-from result)))))))
