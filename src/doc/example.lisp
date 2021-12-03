(uiop:define-package #:weblocks/doc/example
  (:use #:cl)
  (:import-from #:clack-handler-hunchentoot)
  (:import-from #:40ants-doc/locatives/base
                #:define-locative-type)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:serapeum)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:weblocks/hooks
                #:defhook)
  (:import-from #:global-vars
                #:define-global-var)
  (:export #:defexample
           #:weblocks-example
           #:start-server
           #:update-examples
           #:*server-url*
           #:start-doc-server))
(in-package weblocks/doc/example)


(defvar *server-url* nil
  "This variable can be used to point to a production server for showing documentation examples.")

(defvar *port* nil
  "A port where examples server is listening.")


(defclass weblocks-example ()
  ((name :initarg :name
         :type symbol
         :reader example-name)
   (width :initarg :width
          :reader example-width)
   (height :initarg :height
           :reader example-height)
   (package :initarg :package
            :reader example-package)
   (original-body :initarg :original-body
                  :type cons
                  :reader example-original-body
                  :documentation "The code before any transformations.
                                  Used when some example inherits another example.")
   (body :initarg :body
         :type cons
         :reader example-body)))


(define-locative-type weblocks-example ()
  "Allows to define an example of Weblocks application and to insert it into the documentation as iframe.

   To show application, you also have to deploy a server part for serving dynamic content.")


(defmethod 40ants-doc/locatives/base:locate-object (symbol (locative-type (eql '40ants-doc/locatives::weblocks-example))
                                                    locative-args)
  (declare (ignore locative-args))
  (unless (and (boundp symbol)
               (typep (symbol-value symbol)
                      'weblocks-example))
    (error "Weblocks example locative works only with objects defined by DEFEXAMPLE."))
  (symbol-value symbol))


(define-global-var *iframe-id* 0)


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((example weblocks-example))
  (let* ((iframe-id (format nil "example-~A"
                            (incf *iframe-id*)))
         (full-url (format nil "~A~A?iframe-id=~A"
                           (or *server-url*
                               (if *port*
                                   (format nil "http://localhost:~A/examples"
                                           *port*)
                                   (error "Please start local documentation server using WEBLOCKS/DOC/EXAMPLE:START-SERVER or bind WEBLOCKS/DOC/EXAMPLE:*SERVER-URL* variable.")))
                           (example-path example)
                           iframe-id))
         (js-code "
window.addEventListener('message', function(e) {
  let message = e.data;
  let iframe_id = message.iframe_id;
  let iframe = document.querySelector('#' + iframe_id);
  iframe.style.height = message.height + 'px';
  iframe.style.width = message.width + 'px';
} , false);
"))
    (commondoc-markdown/raw-html:make-raw-html-block
     (weblocks/html:with-html-string
       (:div :class "demo"
             (:iframe :id iframe-id
                      :src full-url
                      :sandbox "allow-forms allow-same-origin allow-scripts"
                      :style (format nil "width: ~A; height: ~A; border: 0"
                                     (example-width example)
                                     (example-height example))))
       (:script js-code)))))


(defun replace-internal-symbols (body &key from-package to-package)
  "If example code mentions internal symbols, then most probably
   these symbols are used to define a widget class or some helpers.

   To prevent conflicts, we need to \"move\" thes symbols to a
   separate packages."
  (serapeum:map-tree (lambda (item)
                       (typecase item
                         (symbol (cond
                                   ((and (eql (symbol-package item)
                                              from-package)
                                         (not (40ants-doc/utils:is-external item from-package)))
                                    (intern (symbol-name item)
                                            to-package))
                                   (t
                                    item)))
                         (t item)))
                     body))


(defhook register-example (example)
  "Called when an weblocks documentation example is being defined.")


(defun register (example)
  (weblocks/hooks:call-register-example-hook example))


(defmacro defexample (name (&key (width "100%")
                              (height "10em")
                              ;; Includes code from listed examples: 
                              (inherits nil)
                              (show-code-tab t))
                      &body body)
  "Defines Weblocks app example.

   Body should contain a code snippet which includes

   TODO: write a doc

   When creating a server part, the BODY form will be
   evaluated as part of the new anonymous package."
  (declare (ignore show-code-tab))
  
  (let* ((package-name (format nil "WEBLOCKS/EXAMPLES/~A/~A"
                               (package-name (symbol-package name))
                               (symbol-name name)))
         (full-body (append (loop for another-example in (uiop:ensure-list inherits)
                                  appending (example-original-body (eval another-example)))
                            body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,name
         (let* ((package (or (find-package ,package-name)
                             (make-package ,package-name
                                           :use (list "CL"))))
                (new-body (replace-internal-symbols ',full-body
                                                    :from-package *package*
                                                    :to-package package))
                (example (make-instance 'weblocks-example
                                        :name ',name
                                        :package package
                                        :width ,width
                                        :height ,height
                                        :original-body ',full-body
                                        :body new-body)))
           (register example)
           (values example))))))


(defun example-path (example)
  (let ((name (example-name example)))
    (format nil "/~A/~A"
            (string-downcase (package-name (symbol-package name)))
            (string-downcase (symbol-name name)))))



(defmethod 40ants-doc/object-package::object-package ((object weblocks-example))
  (example-package object))


