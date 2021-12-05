(uiop:define-package #:reblocks/preview
  (:use #:cl)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:log4cl)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:export #:preview))
(in-package reblocks/preview)


(reblocks/app:defapp preview
  :prefix "/"
  :autostart nil)


(defvar *widget* nil)


(defvar *port* nil
  "A port where preview server is listening.")


(defwidget preview-widget ()
  ())


(defmethod reblocks/session:init ((app preview))
  (make-instance 'preview-widget))


(defmethod reblocks/widget:render ((widget preview-widget))
  (reblocks/widget:render *widget*))


(defun preview (widget)
  (when (null *port*)
    (let ((random-port (find-port))
          ;; To prevent weblocks from complaining
          ;; about other running server
          (reblocks/server::*server* nil))
      (reblocks/server:start :port random-port
                             :apps 'preview)
      (setf *port* random-port)))

  (setf *widget* widget)
  
  (let ((url (format nil "http://localhost:~A"
                     *port*)))
    (log:info "Opening a web browser to look at ~A"
              url)
    (open-browser url)))
