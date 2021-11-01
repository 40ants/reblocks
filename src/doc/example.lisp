(uiop:define-package #:weblocks/doc/example
  (:use #:cl)
  (:import-from #:clack-handler-hunchentoot)
  (:import-from #:40ants-doc/locatives/base
                #:define-locative-type)
  (:import-from #:weblocks/widget
                #:create-widget-from
                #:defwidget)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:serapeum)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:weblocks/server)
  (:export #:defexample
           #:weblocks-example
           #:start-server
           #:update-examples
           #:*server-url*))
(in-package weblocks/doc/example)


(defvar *server-url* nil
  "This variable can be used to point to a production server for showing documentation examples.")

(defvar *port* nil
  "A port where examples server is listening.")


(defvar *examples* (make-hash-table :test 'equal
                                    #+sbcl
                                    :synchronized
                                    #+sbcl
                                    t))


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


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((example weblocks-example))
  (let ((full-url (concatenate 'string
                               (or *server-url*
                                   (if *port*
                                       (format nil "http://localhost:~A/examples"
                                               *port*)
                                       (error "Please start local documentation server using WEBLOCKS/DOC/EXAMPLE:START-SERVER or bind WEBLOCKS/DOC/EXAMPLE:*SERVER-URL* variable.")))
                               (example-path example))))
    (commondoc-markdown/raw-html:make-raw-html-block
     (weblocks/html:with-html-string
       (:div :class "demo"
             (:iframe :src full-url
                      :style (format nil "width: ~A; height: ~A"
                                     (example-width example)
                                     (example-height example))))))))


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


(defmacro defexample (name (&key (width "100%")
                                 (height "10em")
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
                               (symbol-name name))))
    `(defparameter ,name
       (let* ((package (or (find-package ,package-name)
                           (make-package ,package-name
                                         :use (list "CL"))))
              (new-body (replace-internal-symbols ',body
                                                  :from-package *package*
                                                  :to-package package)))
         (make-instance 'weblocks-example
                        :name ',name
                        :package package
                        :width ,width
                        :height ,height
                        :body new-body)))))


(defun widget-class (example)
  (loop for form in (example-body example)
        when (eql (first form)
                  'weblocks/widget:defwidget)
          do (return (second form))
        finally (error "No DEFWIDGET form was found in the example's body.")))


(defun example-path (example)
  (let ((name (example-name example)))
    (format nil "/~A/~A"
            (string-downcase (package-name (symbol-package name)))
            (string-downcase (symbol-name name)))))


(defmethod create-widget-from ((example weblocks-example))
  ;; First, we need to evaluate example's code
  (eval (list* 'progn
               (example-body example)))
  ;; And now, when we have class created,
  ;; we can use it for instantiation.
  ;; Example may define MAKE-EXAMPLE function
  ;; or we can guess as widget class and create it
  (let ((make-example (find-symbol "MAKE-EXAMPLE" (example-package example))))
    (cond
      (make-example
       (funcall make-example))
      (t
       (make-instance (widget-class example))))))


(defapp examples-server
  :prefix "/examples/"
  :autostart nil)


(defwidget examples-widget ()
  ((current-path :initform nil
                 :accessor current-path)
   (current-widget :initform nil
                   :accessor current-widget)))


(defmethod weblocks/session:init ((app examples-server))
  (make-instance 'examples-widget))


(defmethod weblocks/widget:render ((widget examples-widget))
  
  (let* ((app-prefix (weblocks/app:get-prefix (weblocks/app:get-current)))
         (full-path (string-downcase (weblocks/request:get-path)))
         (path (subseq full-path (length app-prefix))))
    (unless (string-equal path
                          (current-path widget))
      (let ((new-widget (gethash path *examples*)))
        (setf (current-widget widget)
              (when new-widget
                (weblocks/widget:create-widget-from new-widget))
              (current-path widget)
              path)))
    (cond
      ((current-widget widget)
       (weblocks/widget:render (current-widget widget)))
      (t
       (with-html
         (:h1 ("No widget with path ~A" path))
         (cond
           ((zerop (hash-table-count *examples*))
            (:p "No examples are registered yet."))
           (t
            (:p "Here is list of awailable examples:")
            (loop for path in (sort (hash-table-keys *examples*)
                                    #'string>)
                  for uri = (format nil "/examples~A" path)
                  do (:li (:a :href uri uri))))))))))


(defmethod 40ants-doc/object-package::object-package ((object weblocks-example))
  (example-package object))


(defun collect-examples (asdf-system-name)
  "Searches packages belonging to the given asdf-system."
  (check-type asdf-system-name string)
  
  (loop with asdf-system-name = (string-downcase asdf-system-name)
        with results = (make-hash-table :test 'equal)
        with prefix = (concatenate 'string asdf-system-name "/")
        for package in (list-all-packages)
        for name = (string-downcase (package-name package))
        when (or (string= asdf-system-name name)
                 (str:starts-with-p prefix
                                    name))
          do (loop for symbol being the symbols of package
                   for var = (and (boundp symbol)
                                  (symbol-value symbol))
                   when (and var
                             (typep var 'weblocks-example))
                     do (setf (gethash (example-path var) results)
                              (weblocks/widget:create-widget-from var)))
        finally (return results)))


(defun update-examples (asdf-system-name)
  (setf *examples*
        (collect-examples asdf-system-name)))


(defun start-server (&key
                       port
                       (interface "localhost")
                       for-asdf-system)
  (when for-asdf-system
    (update-examples (string-downcase for-asdf-system)))
  
  (when (null *port*)
    (let ((port (or port
                    (find-port)))
          ;; To prevent weblocks from complaining
          ;; about other running server
          (weblocks/server::*server* nil))
      (weblocks/server:start :port port
                             :interface interface
                             :apps 'examples-server)
      (setf *port* port)))

  (let ((url (format nil "http://localhost:~A/examples/"
                     *port*)))
    (log:info "Started examples server at ~A"
              url)))


;; Entry-point for Heroky deployment
(defun cl-user::initialize-application (&key (port 8080) (interface "0.0.0.0"))
  (format t "Starting examples server on ~A:~A~%"
          interface port)
  (update-examples "weblocks")
  (start-server :port port
                :interface interface))
