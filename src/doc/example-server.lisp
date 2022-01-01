(uiop:define-package #:reblocks/doc/example-server
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:clack-handler-hunchentoot)
  (:import-from #:40ants-doc/locatives/base
                #:define-locative-type)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:defwidget)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:serapeum)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:reblocks/doc/example
                #:*server-url*
                #:*port*
                #:reblocks-example
                #:example-name
                #:example-body
                #:example-original-body
                #:example-package)
  (:import-from #:reblocks/server)
  (:import-from #:reblocks-file-server)
  (:import-from #:reblocks/hooks)
  (:export #:start-server
           #:update-examples
           #:stop-server))
(in-package reblocks/doc/example-server)


(defvar *examples* (make-hash-table :test 'equal
                                    #+sbcl
                                    :synchronized
                                    #+sbcl
                                    t))

(defvar *interface* "localhost")


(defun widget-class (example)
  (loop for form in (example-body example)
        when (eql (first form)
                  'reblocks/widget:defwidget)
          do (return (second form))
        finally (error "No DEFWIDGET form was found in the example's body.")))


(defun example-path (example)
  (let ((name (example-name example)))
    (format nil "/~A/~A"
            (string-downcase (package-name (symbol-package name)))
            (string-downcase (symbol-name name)))))


(defmethod create-widget-from ((example reblocks-example))
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


(reblocks/hooks:on-application-hook-register-example
    add-example-to-registry (example)
  
  (let ((path (example-path example))
        (name (example-name example))
        (package (example-package example)))
    (log:info "Registering ~A:~A at path ~A"
              (package-name package)
              name
              path)
    (setf (gethash path *examples*)
          (cons example
                (create-widget-from example)))))


(defapp examples-server
  :prefix "/examples/"
  :autostart nil)


(defwidget examples-widget ()
  ((current-path :initform nil
                 :accessor current-path)
   (current-example :initform nil
                    :accessor current-example)
   (current-widget :initform nil
                   :accessor current-widget)))


(defmethod reblocks/app::initialize-webapp ((app examples-server))
  (call-next-method)

  ;; We only need to serve docs in the development mode,
  ;; when they we built and lay on the disk at docs/build
  (let ((path (asdf:system-relative-pathname :reblocks "docs/build/")))
    (when (probe-file path)
      (reblocks-file-server:make-route :uri "/docs/"
                                       :root path
                                       :dir-listing t))))


(defmethod reblocks/session:init ((app examples-server))
  (make-instance 'examples-widget))


(defmethod reblocks/page:render-dependencies :after ((app examples-server) dependencies)
  (with-html
    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/a11y-dark.min.css")
    (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js")
    (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/lisp.min.js")
    (:script "hljs.highlightAll()")
    
    (:link :rel "stylesheet"
           :href "https://cdn.jsdelivr.net/npm/foundation-sites@6.7.4/dist/css/foundation.min.css"
           :crossorigin "anonymous")
    (:script :src "https://cdn.jsdelivr.net/npm/foundation-sites@6.7.4/dist/js/foundation.min.js"
             :crossorigin "anonymous")
    
    (:script "
window.addEventListener('load', function() {
  $(document).foundation();

  let params = new URLSearchParams(window.location.search);
  let iframe_id = params.get('iframe-id');

  setInterval(function () {
    let message = {
      iframe_id: iframe_id,
      height: document.body.scrollHeight,
      width: document.body.scrollWidth
    };

    // window.top refers to parent window
    window.top.postMessage(message, \"*\");
  }, 500);
});
")
    
    (:style "
body {margin: 0px}
pre {
    margin: 0px;
    padding: 0.2em;
    border: 0.1em solid #CCC;
    color: black;
    background-color: #F5F3ED;
}
")))


(defmethod reblocks/widget:render ((widget examples-widget))
  (let* ((app-prefix (reblocks/app:get-prefix (reblocks/app:get-current)))
         (full-path (string-downcase (reblocks/request:get-path)))
         (path (subseq full-path (length app-prefix)))
         (item (gethash path *examples*)))
    (unless (or (null item)
                (string-equal path
                              (current-path widget)))
      (destructuring-bind (example . new-widget)
          item
        (setf (current-widget widget)
              (when new-widget
                (reblocks/widget:create-widget-from new-widget))
              (current-example widget)
              example
              (current-path widget)
              path)))
    (cond
      ((current-widget widget)
       (with-html

         (:ul :class "tabs small"
              :data-tabs ""
              :id "example-tabs"
              (:li :class "tabs-title is-active"
                   (:a :href "#demo"
                       :aria-selected "true"
                       "Demo"))
              (:li :class "tabs-title"
                   (:a :href "#code"
                       "Code")))
         
         (:div :class "tabs-content"
               :data-tabs-content "example-tabs"
               (:div :class "tabs-panel is-active"
                     :id "demo"
                     (reblocks/widget:render (current-widget widget)))
               (:div :class "tabs-panel"
                     :id "code"
                     (:pre
                      (:code :class "language-lisp"
                             ;; :onload "alert('Herel')"
                             ;; :onload "hljs.highlightElement(this)"
                             (let* ((example (current-example widget))
                                    (*package* (example-package example)))
                               (with-output-to-string (s)
                                 (loop for form in (example-body example)
                                       do (write form
                                                 :stream s
                                                 :case :downcase
                                                 :readably t)
                                          (terpri s)
                                          (terpri s)))))))))

       )
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


(defun collect-examples (asdf-system-name &key (results (make-hash-table :test 'equal)))
  "Searches packages belonging to the given asdf-system."
  (check-type asdf-system-name string)
  
  (loop with asdf-system-name = (string-downcase asdf-system-name)
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
                             (typep var 'reblocks-example))
                     do (setf (gethash (example-path var) results)
                              (cons var
                                    (reblocks/widget:create-widget-from var))))
        finally (return results)))


(defparameter *known-asdf-systems* nil
  "When UPDATE-EXAMPLES is called without arguments, it will
   update examples for all these systems.")

(defun update-examples (&rest asdf-system-names)
  (setf *known-asdf-systems*
        (union asdf-system-names
               *known-asdf-systems*
               :test #'string-equal))
  
  (loop with results = (make-hash-table :test 'equal)
        for asdf-system-name in *known-asdf-systems*
        do (collect-examples asdf-system-name
                             :results *examples*))
  (values))


(defun start-server (&key
                       port
                       (interface "localhost")
                       (for-asdf-system "reblocks"))
  (when for-asdf-system
    (update-examples (string-downcase for-asdf-system)))
  
  (when (null *port*)
    (let ((port (or port
                    (find-port)))
          ;; To prevent reblocks from complaining
          ;; about other running server
          (reblocks/server::*server* nil))
      (reblocks/server:start :port port
                             :interface interface
                             :apps 'examples-server)
      (setf *port* port
            *interface* interface)))
  
  (let ((url (format nil "http://~A:~A/docs/"
                     *interface*
                     *port*)))
    (log:info "Started examples server at ~A"
              url)))


(defun stop-server ()
  (when *port*
    (reblocks/server:stop *interface* *port*)
    (setf *port* nil
          *interface* nil)))


;; Entry-point for Heroku deployment
(defun cl-user::initialize-application (&key
                                          (port 8080)
                                          (interface "0.0.0.0"))
  (format t "Starting examples server on ~A:~A~%"
          interface port)
  (start-server :port port
                :interface interface
                :for-asdf-system "reblocks"))
