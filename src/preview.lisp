(uiop:define-package #:reblocks/preview
  (:use #:cl)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:log)
  (:import-from #:reblocks/app)
  (:import-from #:reblocks/server)
  (:import-from #:reblocks/session)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:defwidget)
  (:export #:preview
           #:stop))
(in-package #:reblocks/preview)


(reblocks/app:defapp preview
  :prefix "/"
  :autostart nil)


(defvar *widget* nil)


(defvar *port* nil
  "A port where preview server is listening.")


(defwidget preview-widget ()
  ())


(defmethod reblocks/page:init-page ((app preview) path expire-at)
  (make-instance 'preview-widget))


(defmethod reblocks/widget:render ((widget preview-widget))
  (reblocks/widget:render *widget*))


(defun preview (widget)
  (when (null *port*)
    (let ((random-port (find-port))
          ;; To prevent Reblocks from complaining
          ;; about other running server
          (reblocks/server::*server* nil))
      (reblocks/server:start :port random-port
                             :apps 'preview)
      (setf *port* random-port)))

  (setf *widget* (create-widget-from widget))
  
  (let ((url (format nil "http://localhost:~A"
                     *port*)))
    (log:info "Opening a web browser to look at ~A"
              url)
    (open-browser url)))


(defun stop ()
  (when *port*
    (reblocks/server:stop "localhost" *port*)
    (setf *port* nil)))
