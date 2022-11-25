(uiop:define-package #:reblocks/doc/example
  (:use #:cl)
  (:import-from #:clack-handler-hunchentoot)
  (:import-from #:40ants-doc/locatives/base
                #:define-locative-type)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:serapeum
                #:->)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc)
  (:import-from #:reblocks/hooks
                #:defhook)
  (:import-from #:global-vars
                #:define-global-var)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/ignored-words
                #:supports-ignored-words-p
                #:ignore-words-in-package)
  (:import-from #:40ants-doc-full/commondoc/piece
                #:documentation-piece)
  (:import-from #:40ants-doc-full/builder/vars
                #:*base-url*)
  (:import-from #:40ants-doc-full/utils
                #:is-external)
  (:export #:defexample
           #:reblocks-example
           #:start-server
           #:update-examples
           #:*server-url*
           #:start-doc-server))
(in-package #:reblocks/doc/example)


(defvar *server-url* nil
  "This variable can be used to point to a production server for showing documentation examples.")

(defvar *port* nil
  "A port where examples server is listening.")


(defclass reblocks-example ()
  ((name :initarg :name
         :type symbol
         :reader example-name)
   (width :initarg :width
          :reader example-width)
   (height :initarg :height
           :reader example-height)
   (package :initarg :package
            :reader example-package
            :documentation "This slot will contain a package, created for especially this example's body.")
   (original-body :initarg :original-body
                  :type cons
                  :reader example-original-body
                  :documentation "The code before any transformations.
                                  Used when some example inherits another example.")
   (body :initarg :body
         :type cons
         :reader example-body)))


(define-locative-type reblocks-example ()
  "Allows to define an example of Reblocks application and to insert it into the documentation as iframe.

   To show application, you also have to deploy a server part for serving dynamic content.")


(defmethod 40ants-doc/locatives/base:locate-object (symbol (locative-type (eql '40ants-doc/locatives::reblocks-example))
                                                    locative-args)
  (declare (ignore locative-args))
  (unless (and (boundp symbol)
               (typep (symbol-value symbol)
                      'reblocks-example))
    (error "Reblocks example locative works only with objects defined by DEFEXAMPLE."))
  (symbol-value symbol))


(defmethod canonical-reference ((example reblocks-example))
  (40ants-doc/reference:make-reference (example-name example)
                                       '40ants-doc/locatives::reblocks-example))


(define-global-var *iframe-id* 0)


(defclass example-block (documentation-piece
                         common-doc:document-node)
  ((example :initarg :example :reader example)))


(defmethod to-commondoc ((example reblocks-example))
  (make-instance 'example-block
                 :example example
                 :doc-reference (40ants-doc/reference-api:canonical-reference example)))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (node  example-block)
                                            stream)

  ;; TODO: here we may need to use current indentation
  ;; in case if this piece of code is nested inside in a list.
  ;; But for now it should work without an indentation.
  (format stream "~2&```lisp~&~A~&```~2&"
          (example-code (example node)))
  (when *base-url*
    (format stream "~2&Go to [HTML documentation](~A) to see this code in action.~2&"
            *base-url*)))


(common-html.emitter::define-emitter (obj example-block)
  (let* ((example (example obj))
         (iframe-id (format nil "example-~A"
                            (incf *iframe-id*)))
         (full-url (format nil "~A~A?iframe-id=~A"
                           (or *server-url*
                               (if *port*
                                   (format nil "http://localhost:~A/examples"
                                           *port*)
                                   (error "Please start local documentation server using REBLOCKS/DOC/EXAMPLE:START-SERVER or bind REBLOCKS/DOC/EXAMPLE:*SERVER-URL* variable.")))
                           (example-path example)
                           iframe-id))
         (js-code "
window.addEventListener('message', function(e) {
  let message = e.data;
  let iframe_id = message.iframe_id;
  let iframe = document.querySelector('#' + iframe_id);
  if (iframe) {
    iframe.style.height = message.height + 'px';
    iframe.style.width = message.width + 'px';
  } else {
    console.log('Unable to find iframe with id', iframe_id);
  }
} , false);
")
         (html (reblocks/html:with-html-string
                 (:div :class "demo"
                       (:iframe :id iframe-id
                                :src full-url
                                :sandbox "allow-forms allow-same-origin allow-scripts"
                                :style (format nil "width: ~A; height: ~A; border: 0"
                                               (example-width example)
                                               (example-height example))))
                 (:script
                  ;; TODO: remove :RAW after this issue will be solved:
                  ;; https://github.com/ruricolist/spinneret/issues/59
                  (:raw js-code)))))

    (write-string html
                  common-html.emitter::*output-stream*)))


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
                                         (not (is-external item from-package)))
                                    (intern (symbol-name item)
                                            to-package))
                                   (t
                                    item)))
                         (t item)))
                     body))


(defhook register-example (example)
  "Called when an reblocks documentation example is being defined.")


(defun register (example)
  (reblocks/hooks:call-register-example-hook example))


(defmacro defexample (name (&key (width "100%")
                              (height "10em")
                              ;; Includes code from listed examples: 
                              (inherits nil)
                              (show-code-tab t))
                      &body body)
  "Defines Reblocks app example.

   Body should contain a code snippet which includes

   TODO: write a doc

   When creating a server part, the BODY form will be
   evaluated as part of the new anonymous package."
  (declare (ignore show-code-tab))
  
  (let* ((package-name (format nil "REBLOCKS/EXAMPLES/~A/~A"
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
                (example (make-instance 'reblocks-example
                                        :name ',name
                                        :package package
                                        :width ,width
                                        :height ,height
                                        :original-body ',full-body
                                        :body new-body)))
           ;; Here we are registering a symbol
           ;; to prevent 40ANTS-DOC's complains.
           (let ((*package* package))
             (ignore-words-in-package ',name))

           (register example)
           (values example))))))


(defun example-path (example)
  (let ((name (example-name example)))
    (format nil "/~A/~A"
            (string-downcase (package-name (symbol-package name)))
            (string-downcase (symbol-name name)))))

(-> example-code (reblocks-example) string)
(defun example-code (example)
  "Returns example's code as a string."
  
  (let ((*package* (example-package example)))
    (with-output-to-string (s)
      (loop for form in (example-body example)
            do (write form
                      :stream s
                      :case :downcase
                      :readably t)
               (terpri s)
               (terpri s)))))



(defmethod 40ants-doc/object-package::object-package ((object reblocks-example))
  (example-package object))



