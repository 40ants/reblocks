(uiop:define-package #:reblocks/page
  (:use #:cl)
  (:import-from #:parenscript)
  (:import-from #:reblocks/variables
                #:*delay-between-pages-cleanup*
                #:*extend-page-expiration-by*
                #:*max-pages-per-session*
                #:*pages-expire-in*
                #:*current-app*
                #:*default-content-type*)
  (:import-from #:reblocks/html
                #:*lang*
                #:with-html
                #:get-rendered-chunk)
  (:import-from #:reblocks/dependencies
                #:render-in-head
                #:get-dependencies
                #:register-dependencies)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:alexandria
                #:hash-table-keys
                #:symbolicate)

  ;; Just dependencies
  (:import-from #:log)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:local-time
                #:timestamp-maximum
                #:timestamp>
                #:timestamp<
                #:universal-to-timestamp
                #:timestamp
                #:now)
  (:import-from #:serapeum
                #:take
                #:length<)
  (:import-from #:reblocks/session
                #:map-sessions
                #:do-sessions
                #:init)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:with-lock-held
                #:thread-alive-p
                #:make-lock)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  
  (:export
   #:render
   #:render-body
   #:render-dependencies
   #:render-headers
   #:get-title
   #:get-description
   #:get-keywords
   #:get-language
   #:max-pages-per-session
   #:page-expire-in
   #:extend-page-expiration-by
   #:page-metadata
   #:current-page
   #:in-page-context-p))
(in-package #:reblocks/page)


(defvar *title*)
(defvar *description*)
(defvar *keywords*)
(defvar *language*)
(defvar *default-language* "en")
(defvar *current-page*)

(defvar *pages-cleaner-thread* nil)

(defvar *pages-cleaner-thread-lock*
  (make-lock "pages cleaner lock"))


(defclass page ()
  ((path :initarg :path
         :type string
         :reader page-path)
   (created-at :initform (now)
               :type timestamp
               :reader page-created-at)
   (expire-at :initarg :expire-at
              :initform nil
              :type (or null timestamp)
              :accessor page-expire-at)
   (code-to-action :initform (make-hash-table :test 'equal)
                   :reader page-actions)
   (metadata :initform (make-hash-table :test 'equal)
             :reader %page-metadata)))


(defmethod print-object ((obj page) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A"
            (page-path obj)
            (page-created-at obj)
            (when (page-expire-at obj)
              (let ((expire-in
                      (coerce (floor (local-time:timestamp-difference (page-expire-at obj)
                                                                      (local-time:now)))
                              'integer)))
                (if (<= expire-in 0)
                    "will expire now"
                    (format nil "~As till expiration"
                            expire-in)))))))


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
        (*language* *default-language*)
        (*current-page* (let* ((path (get-path))
                               (expire-in (page-expire-in *current-app*
                                                          path))
                               (expire-at (when expire-in
                                            (universal-to-timestamp
                                             (+ (get-universal-time)
                                                expire-in)))))
                          (make-instance 'page
                                         :path path
                                         :expire-at expire-at)))
        (max-pages (max-pages-per-session *current-app*))
        (session-pages
          (reblocks/session:get-value :pages))
        (next-year (universal-to-timestamp
                    (+ (get-universal-time)
                       (* 365 24 60 60)))))
    ;; Here we are adding a new session object to the map of known sessions
    (setf (gethash *current-page* session-pages)
          t)
    
    ;; This code is not optimal and should be
    ;; replaced with some sort of priority queue,
    ;; but I didn't find a priority queue allowing
    ;; to reprioretize elements (we need it to update
    ;; expire-at slot of pages).
    ;; The first element in this list will be the oldest
    ;; in terms of expiration time or creation order
    (let* ((current-pages-count (hash-table-count session-pages))
           (num-pages-to-expire (if max-pages
                                    (- current-pages-count
                                       max-pages)
                                    0)))
      (when (< 0 num-pages-to-expire)
        ;; Probably some sort of priority heap with limited length whould work
        ;; faster here. But right now I'm not sure it deserve an optimization,
        ;; because typically a number of pages within one session should not be
        ;; to large
        (let* ((sorted-pages (sort (hash-table-keys session-pages)
                                   #'timestamp<
                                   :key (lambda (page)
                                          (or (page-expire-at page)
                                              next-year))))
               (pages-to-expire (take num-pages-to-expire sorted-pages )))
          (loop for page in pages-to-expire
                do (log:debug "Expiring page ~A because only ~A pages is allowed for one session but we already have ~A."
                              page
                              max-pages
                              current-pages-count)
                   (remhash page session-pages)))))
    
    (funcall body-func)))


(defmacro with-page-defaults (&body body)
  `(call-with-page-defaults
    (lambda () ,@body)))


(defgeneric render-page-with-widgets (app)
  (:documentation "Renders a full HTML by collecting header elements, dependencies and inner
   HTML and inserting them into the `render' method.

   This function will be called inside WITH-PAGE-DEFAULTS block,
   where such variables as *TITLE* are bound to their default values.
   These variables can be changed by user during widgets or page rendering."))


(defgeneric page-expire-in (app page-path)
  (:documentation "Returns NIL or a number of seconds after which page should be removed from the memory.

                   Default method returns current value of *PAGES-EXPIRE-IN* variable.")
  (:method ((app t) (page-path t))
    *pages-expire-in*))


(defgeneric extend-page-expiration-by (app page)
  (:documentation "Returns NIL or a number of seconds after which page should be removed from the memory.

                   Default method returns current value of *PAGES-EXPIRE-IN* variable.")
  (:method ((app t) (page t))
    (if (boundp '*extend-page-expiration-by*)
        *extend-page-expiration-by*
        *pages-expire-in*)))


(defgeneric max-pages-per-session (app)
  (:documentation "Returns NIL or a maximum number of pages to keep in the session.
                   Older pages will be expired and free memory.

                   Default method returns current value of *MAX-PAGES-PER-SESSION* variable.")
  (:method ((app t))
    *max-pages-per-session*))


(defun expire-session-pages (pages)
  (list
   (when pages
     (loop with now = (now)
           for page being the hash-key of pages
           for expire? = (and (page-expire-at page)
                              (timestamp< (page-expire-at page)
                                          now))
           when expire?
           do (log:debug "Expiring page" page)
           and collect page into expired-pages
           finally (mapc (lambda (page )
                           (remhash page pages))
                         expired-pages)
                   (return pages)))))


(defun expire-pages ()
  "Removes from memory expired pages. This function is called periodically
   from a separate thread."
  (when reblocks/session::!map-sessions
    (map-sessions #'expire-session-pages :pages)))


(defun extend-expiration-time (app page)
  (when (and page
             (page-expire-at page))
    (setf (page-expire-at page)
          (timestamp-maximum
           (universal-to-timestamp
            (+ (get-universal-time)
               (extend-page-expiration-by app page)))
           (page-expire-at page)))))


(defun pages-cleaner-loop ()
  (loop do (handler-case
               (with-log-unhandled ()
                 (expire-pages)
                 (sleep *delay-between-pages-cleanup*))
             (error ()
               (sleep *delay-between-pages-cleanup*)))))


(defun ensure-pages-cleaner-is-running ()
  (with-lock-held (*pages-cleaner-thread-lock*)
    (when (or (null *pages-cleaner-thread*)
              (not (thread-alive-p *pages-cleaner-thread*)))
      (setf *pages-cleaner-thread*
            (make-thread #'pages-cleaner-loop
                         :name "Reblocks Pages Cleaner")))))



(defun page-expired-p (page)
  ;; Page consided exired when it is not found in the list of all session pages
  (when page
    (null (gethash page (reblocks/session:get-value :pages)))))


(defun initialize-session-pages ()
  (setf (reblocks/session:get-value :pages)
        ;; Here we don't use weak hash table,
        ;; because this map will keep pages
        ;; in memory, while in all other places
        ;; we need to use weak references.
        ;; This way, memory will be freed when
        ;; pages are expiring.
        (make-hash-table)))


(defmethod init :before ((app t))
  (initialize-session-pages))


(defun page-metadata (page name)
  "Returns a metadata with NAME, bound to the current page"
  (gethash name (%page-metadata page)))


(defun (setf page-metadata) (value page name)
  (setf (gethash name (%page-metadata page))
        value))


(defun in-page-context-p ()
  "Returns T when CURRENT-PAGE function will be able to return page instead of throwing an error."
  (and (boundp '*current-page*)
       *current-page*))


(defun current-page ()
  "Returns current page object. Can be useful to pass to call PAGE-METADATA."
  (unless (in-page-context-p)
    (error "Please, run function CURRENT-PAGE in the context where current page is known."))
  *current-page*)
