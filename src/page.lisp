(uiop:define-package #:reblocks/page
  (:use #:cl)
  (:import-from #:parenscript)
  (:import-from #:reblocks/variables
                #:*default-content-type*)
  (:import-from #:reblocks/html
                #:*lang*
                #:with-html
                #:get-rendered-chunk)
  (:import-from #:reblocks/dependencies
                #:render-in-head
                #:get-dependencies
                #:register-dependencies
                #:get-collected-dependencies)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:alexandria
                #:symbolicate)

  ;; Just dependencies
  (:import-from #:log)
  
  (:export
   #:render
   #:render-body
   #:render-dependencies
   #:render-headers
   #:get-title
   #:get-description
   #:get-keywords
   #:get-language))
(in-package #:reblocks/page)


(defvar *title*)
(defvar *description*)
(defvar *keywords*)
(defvar *language*)
(defvar *default-language* "en")


(defmacro def-get-set (variable)
  "Generates a function get-<variable> and a corresponding setf part."
  (let ((func-name (symbolicate :get- variable))
        (var-name (symbolicate :* variable :*)))
    `(progn
       (defun ,func-name ()
         ,var-name)

       (defun (setf ,func-name) (value)
         (setf ,var-name value)))))


(def-get-set title)
(def-get-set description)
(def-get-set keywords)
(def-get-set language)


(defgeneric render-headers (app)
  (:documentation   "
   By default, adds <meta> entries with Content-Type, description and keywords to the app's page."))

(defmethod render-headers ((app app))
  (with-html
    (:meta :http-equiv "Content-type"
           :content *default-content-type*)
    
    (when (get-description)
      (:meta :name "description"
             :content (get-description)))
    (when (get-keywords)
      (:meta :name "keywords"
             :content (format nil "~{~A~^,~}" (get-keywords))))))


(defgeneric render-body (app body-html-string)
  (:documentation "By default, it just renders BODY-HTML-STRING as is, without escaping.

                   BODY-HTML-STRING argument contains a rendered widget tree, generated
                   by REBLOCKS/WIDGET:RENDER generic-function call on the root widget."))

(defmethod render-body ((app app) body-html-string)
  "Default page-body rendering method"
  
  (with-html
    (:raw body-html-string)))


(defgeneric render-dependencies (app dependencies)
  (:documentation "Renders links to CSS and JS dependencies.

                   DEPENDENCIES argument contains a list of
                   objects inherited from REBLOCKS/DEPENDENCIES:DEPENDENCY class."))


(defmethod render-dependencies ((app app) dependencies)
  (etypecase dependencies
    (list (mapc #'render-in-head
                dependencies))
    (string (with-html
              (:raw dependencies)))))


(defgeneric render (app inner-html &key dependencies)
  (:documentation "Renders HTML page for the current application.

                   By default, it renders:

                   * HTML <head> where calls
                     RENDER-HEADERS generic-function and RENDER-DEPENDENCIES
                     generic-function.
                   * HTML <body> where RENDER-BODY generic-function is called
                     with APP and INNER-HTML arguments."))


(defmethod render ((app app)
                   inner-html
                   &key (dependencies (get-dependencies app)))
  "Default page rendering template and protocol."
  (log:debug "Rendering page for" app)
  (unless (boundp '*title*)
    (error "Method REBLOCKS/PAGE:RENDER should be called inside WITH-PAGE-DEFAULTS block."))

  (register-dependencies dependencies)

  (let ((*lang* (get-language))
        (deps-urls (when (typep dependencies 'list)
                     (mapcar #'reblocks/dependencies:get-url
                             dependencies))))
    (with-html
      (:doctype)
      (:html
       (:head
        (:title (get-title))

        ;; It is XXI century, you know?
        ;; All sites should be optimized for mobile screens
        (:meta :name "viewport"
               :content "width=device-width, initial-scale=1")
              
        (render-headers app)
        (render-dependencies app dependencies)
        (:script (:raw
                  (ps:ps* `(setf (ps:@ window loaded-dependencies)
                                 (list ,@deps-urls))))))
       (:body
        (render-body app inner-html)
        ;; (:script :type "text/javascript"
        ;;          "updateWidgetStateFromHash();")
        )))))


(defun call-with-page-defaults (body-func)
  (let ((*title* nil)
        (*description* nil)
        (*keywords* nil)
        (*language* *default-language*))
    (funcall body-func)))


(defmacro with-page-defaults (&body body)
  `(call-with-page-defaults
    (lambda () ,@body)))


(defmethod render-page-with-widgets ((app app))
  "Renders a full HTML by collecting header elements, dependencies and inner
   HTML and inserting them into the `render' method.

   This function will be called inside WITH-PAGE-DEFAULTS block,
   where such variables as *TITLE* are bound to their default values.
   These variables can be changed by user during widgets or page rendering."
  (log:debug "Special Rendering page for" app)

  ;; At the moment when this method is called, there is already
  ;; rendered page's content in the reblocks/html::*stream*.
  ;; All we need to do now – is to render dependencies in the header
  ;; and paste content into the body.
  (let* ((rendered-html (get-rendered-chunk))
         (all-dependencies (get-collected-dependencies)))

    (render app rendered-html :dependencies all-dependencies)))

