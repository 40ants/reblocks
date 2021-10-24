(uiop:define-package #:weblocks/preview
  (:use #:cl)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:log4cl)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:export #:preview))
(in-package weblocks/preview)


(weblocks/app:defapp preview
  :prefix "/"
  :autostart nil)


(defvar *widget* nil)


(defvar *port* nil
  "A port where preview server is listening.")


(defwidget preview-widget ()
  ())


(defmethod weblocks/session:init ((app preview))
  (make-instance 'preview-widget))


(defmethod weblocks/widget:render ((widget preview-widget))
  (weblocks/widget:render *widget*))


(defun preview (widget)
  (when (null *port*)
    (let ((random-port (find-port))
          ;; To prevent weblocks from complaining
          ;; about other running server
          (weblocks/server::*server* nil))
      (weblocks/server:start :port random-port
                             :apps 'preview)
      (setf *port* random-port)))

  (setf *widget* widget)
  
  (let ((url (format nil "http://localhost:~A"
                     *port*)))
    (log:info "Opening a web browser to look at ~A"
              url)
    (open-browser url)))
