
(in-package :weblocks)

(export '(do-dialog do-choice do-confirmation do-information current-dialog))

(defstruct dialog
  "A structure that stores information about a currently displayed
dialog."
  title close widget css-class)

(defmacro current-dialog ()
  "Expands to code that signifies a place that contains information
about the currently active dialog, if any. The place holds a structure
of type 'dialog'."
  `(webapp-session-value 'dialog-contents))

(defun dialog-js-wt (&key image-src image-onclick &allow-other-keys)
  (with-html-to-string
    (:img :src  image-src
     :onclick image-onclick
     :onmouseover "this.style.cursor = \"pointer\";"
     :style "cursor: expression(\"hand\");")))

(deftemplate :dialog-js-wt 'dialog-js-wt)

(defun make-dialog-js (title widget css-class &optional close escape-script-tags-p)
  "Returns a string with JS code that shows a modal pop-up dialog with
the widget inside." 
  (declare (ignore escape-script-tags-p)) ; backwards compatibility
  (flet ((make-close-action ()
           (let* ((close-fn (if (functionp close)
                                close
                                (f_% (answer widget))))
                  (close-action (make-action close-fn)))
             (render-wt-to-string 
               :dialog-js-wt
               nil
               :image-src (make-webapp-public-file-uri "images/dialog/close.gif")
               :image-onclick (format nil "initiateAction(\"~A\", \"~A\");" close-action (session-name-string-pair)))))
         (widget-html (widget)
           (let ((*weblocks-output-stream* (make-string-output-stream)))
             (declare (special *weblocks-output-stream*))
             (render-widget widget)
             (get-output-stream-string *weblocks-output-stream*))))
    ;(format t "widget-html: ~S~%" (widget-html widget))
    (let ((inner (intern (string-upcase (gen-id "inner"))))
          (close-action (intern (string-upcase (gen-id "close")))))
      `(progn
         (setf ,inner ,(widget-html widget))
         (setf ,close-action ,(when close (make-close-action)) )
         (show-dialog ,title
                      ,inner
                      ,(or css-class "")
                      ,close-action)))))

(defun update-dialog-on-request ()
  "This callback function is called by 'handle-client-request'. If a
request is a refresh and a dialog was shown, appropriate JS is
inserted into the page to redraw the dialog."
  (let ((current-dialog (current-dialog)))
    (when (and current-dialog
               (refresh-request-p))
      (with-javascript
        (ps* `(funcall (slot-value *Event 'observe)
                       window "load"
                       (lambda ()
                         ,(make-dialog-js (dialog-title current-dialog)
                                          (dialog-widget current-dialog)
                                          (dialog-css-class current-dialog)
                                          (dialog-close current-dialog)))))))))

(defun/cc do-dialog (title callee &key css-class close)
  (declare (special *on-ajax-complete-scripts*))
  "Presents 'callee' to the user in a modal dialog, saves the
continuation, and returns from the delimited computation. When
'callee' answers, removes the modal interface and reactives the
computation. If the modal interface isn't available, automatically
scales down to 'do-modal' instead."
  (assert (stringp title))
  (if (ajax-request-p)
      (prog2
          (when (current-dialog)
            (error "Multiple dialogs not allowed."))
          (call callee (lambda (new-callee)
                         (setf (current-dialog) (make-dialog :title title
                                                             :widget new-callee
                                                             :close close
                                                             :css-class css-class))
                         (send-script (ps* (make-dialog-js title new-callee css-class close)))))
        (setf (current-dialog) nil)
        (send-script (ps (remove-dialog))))
      (do-modal title callee :css-class css-class)))

(defun choices-get-wt (&key message content)
  (with-html-to-string
    (:p (str message))
    (str content)))

(deftemplate :choices-get-wt 'choices-get-wt)

(defun render-choices-get (msg choices k)
  "Renders the contents of a choice dialog with choices displayed as
   links."
  (render-wt 
    :choices-get-wt 
    nil 
    :message msg
    :content (capture-weblocks-output 
               (with-html 
                 (mapc (lambda (choice)
                         (render-link (lambda (&rest args)
                                        (declare (ignore args))
                                        (answer k (car choice)))
                                      (cdr choice))
                         (htm "&nbsp;"))
                       choices)))))

(defun choices-post-wt (&key message content &allow-other-keys)
  (with-html-to-string 
    (:p (str message)) 
    (str content)))

(deftemplate :choices-post-wt 'choices-post-wt)

(defun render-choices-post (msg choices k &rest args)
  "Renders the contents of a choice dialog with choices displayed as
   form buttons in a POST form."
  (with-html-form (:post (lambda (&rest args)
                           (loop for choice in choices do
                                 (let ((choice-keyword (car choice)))
                                   (when (member choice-keyword args :test #'string-equal)
                                     (progn 
                                       (answer k (if (consp choice) 
                                                   (car choice)
                                                   choice))
                                       (return)))))))
    (render-wt :choices-post-wt nil 
               :message msg 
               :content (capture-weblocks-output 
                          (loop for choice in choices do 
                                (render-button (car choice) :value (cdr choice)))))))

;;; Presents a user with a message and a choice of elements
(defun/cc do-choice (msg choices &key (method :post) (css-class "") (title "Select Option"))
  (do-dialog title
             (make-widget
               (curry (ecase method
                        (:get #'render-choices-get)
                        (:post #'render-choices-post))
                      msg choices))
             :css-class (format nil "choice ~A" css-class)))

;;; Presents a user with a confirmation dialog
(defun/cc do-confirmation (msg &key (type :ok/cancel) (css-class ""))
          (do-choice msg (ecase type
                           (:ok/cancel `((:ok     . ,(widget-translate 'do-confirmation :ok))
                                         (:cancel . ,(widget-translate 'do-confirmation :cancel))))
                           (:yes/no `((:yes . ,(widget-translate 'do-confirmation :yes))
                                      (:no  . ,(widget-translate 'do-confirmation :no)))))
                     :css-class (format nil "confirmation ~A" css-class)
                     :title (widget-translate 'do-confirmation :title)))

(defmethod widget-translation-table append ((type (eql 'do-confirmation)) &rest args &key confirmation-type )
  "Returns"
  `((:title . ,(translate "Confirmation"))
    ,@(when (or (not confirmation-type) (equal confirmation-type :yes/no))
        `((:yes . ,(translate "Yes"))
          (:no  . ,(translate "No"))))
    ,@(when (or (not confirmation-type) (equal confirmation-type :ok/cancel))
        `((:ok . ,(translate "Ok"))
          (:cancel . ,(translate "Cancel"))))))

;;; Presents a user with an information dialog
(defun/cc do-information (msg &key (css-class ""))
  (do-choice msg (list (cons :ok (humanize-name :ok)))
             :css-class (format nil "information ~A" css-class)
             :title (translate "Information")))

