(uiop:define-package #:reblocks-examples/simple-form
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/server)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:update
                #:defwidget)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:documentation "This example demonstrating a simple web-page with a form having multiple fields and submit button.

                   When user pushes the submit button, entered data are processed on the backend and are rendered in read-only mode."))
(in-package #:reblocks-examples/simple-form)


(defapp simple-form
  :prefix "/")


(defun start (&key (port 8080) (interface "localhost") )
  (reblocks/server:stop interface
                        port)
  (reblocks/server:start :apps 'simple-form
                         :port port
                         :interface interface))


(defwidget form-page (reblocks-ui:ui-widget)
  ((edit-mode :initform t
              :accessor edit-mode-p)
   (name :initform ""
         :accessor name)
   (name-error :initform nil
               :accessor name-error)
   (profession :initform ""
               :accessor profession)
   (bio :initform ""
        :accessor bio)))


(defmethod reblocks/page:init-page ((app simple-form) (url-path string) expire-at)
  (check-type expire-at (or null local-time::timestamp))
  (make-instance 'form-page))


(defmethod reblocks/widget:render ((widget form-page))
  (labels ((to-edit-mode (&rest args)
             (log:info "Switching to edit mode" args)
             (setf (edit-mode-p widget)
                   t)
             (update widget))
           (valid-p (data)
             (let ((has-errors nil))
               (setf (name-error widget)
                     nil)
               
               (when (or (null (getf data :name))
                         (string= "" (getf data :name)))
                 (setf has-errors t)
                 (setf (name-error widget)
                       "Name is required field"))
               (values (not has-errors))))
           (handle-submission (&rest args &key submit-button &allow-other-keys)
             (log:info "Form was submitted" args)
             (cond
               ((and (not (null submit-button))
                     (string-equal submit-button
                                   "cancel"))
                (setf (edit-mode-p widget)
                      nil))
               ;; Saving
               (t
                (when (valid-p args)
                  (setf (name widget)
                        (getf args :name))
                  (setf (profession widget)
                        (getf args :profession))
                  (setf (bio widget)
                        (getf args :bio))
                  (setf (edit-mode-p widget)
                        nil))))
             (update widget)))
    
    (with-html
      (:div :style "width: 50%; margin: 4rem auto"
            (cond
              ((edit-mode-p widget)
               (reblocks-ui/form:with-html-form (:post #'handle-submission)
                 (when (name-error widget)
                   (:div :class "label alert"
                         (name-error widget)))
                 (:input :type "text"
                         :name "name"
                         :placeholder "Enter your name"
                         :value (name widget))

                 (:select :name "profession"
                          (loop for profession in '("Nothing special"
                                                    "Software Developer"
                                                    "Designer"
                                                    "Product Manager")
                                for selected = (string= (profession widget)
                                                        profession)
                                do (:option :selected selected
                                            profession)))
                 (:textarea :name "bio"
                            (bio widget))
                 
                 ;; This works:
                 (:button :type "submit"
                          :class "button"
                          :name "submit-button"
                          :value "submit"
                          "Submit")
                 (:button :class "button secondary"
                          :name "submit-button"
                          :value "cancel"
                          "Cancel")

                 
                 ;; And this should work too:
                 ;; (:input :class "button"
                 ;;         :name "submit-button"
                 ;;         :type "submit"
                 ;;         :value "submit")
                 ;; (:input :class "button secondary"
                 ;;         :type "submit"
                 ;;         :name "submit-button"
                 ;;         :value "cancel")
                 ))
              (t
               (:dl
                (:dt "Name")
                (:dd (name widget))
                (:dt "Profession")
                (:dd (profession widget))
                (:dt "Bio")
                (:dd (bio widget)))
               
               (render-form-and-button "Edit"
                                       #'to-edit-mode)))))))
