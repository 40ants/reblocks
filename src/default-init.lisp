(uiop:define-package #:reblocks/default-init
  (:use #:cl)
  (:import-from #:reblocks/page
                #:page-root-widget
                #:page)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:reblocks/widgets/default-page
                #:make-default-init-page-widget)
  (:import-from #:reblocks/app
                #:page-constructor)
  (:import-from #:40ants-routes/handler
                #:call-handler))
(in-package #:reblocks/default-init)



(defun %common-init (app)
  (declare (ignore app))
  (make-default-init-page-widget))


;; TODO: this generic function is deprecated, remove after 2023-07-01
;; TODO: really remove
(defmethod reblocks/session:init ((app t))
  (warn "Use REBLOCKS/PAGE:INIT instead of REBLOCKS/SESSION:INIT. This method will be removed after 2023-07-01.")
  (%common-init app))


(defmethod reblocks/page:init-page ((app t) (path string) expire-at)
  ;; TODO: replace with content of %common-init after deprecation
  (let ((page-widget (call-handler)))
    (cond
      (page-widget
       (unless (typep page-widget
                      'reblocks/widget:widget)
         (error "Handler for ~S URL returned non widget object of type ~S."
                path
                (type-of page-widget)))
       
       ;; This way it is possible to apply a unified header, footer and sidebar to
       ;; all pages of the web application:
       (let ((wrapped-page-widget
               (funcall (page-constructor app)
                        page-widget)))
         (values wrapped-page-widget)))
      ;; If no handler were found for current path,
      ;; then show an example how to define a custom INIT-PAGE
      ;; method or to define routes for application.
      (t
       (%common-init app)))))


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
