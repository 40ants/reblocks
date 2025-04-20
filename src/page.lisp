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
                #:get-dependencies)
  ;; (:import-from #:reblocks/routes/server
  ;;               #:register-dependencies)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only
                #:hash-table-keys
                #:symbolicate)

  ;; Just dependencies
  (:import-from #:log)
  (:import-from #:reblocks/request
                #:ajax-request-p
                #:refresh-request-p
                #:get-path)
  (:import-from #:local-time
                #:timestamp-maximum
                #:timestamp>
                #:timestamp<
                #:universal-to-timestamp
                #:timestamp
                #:now)
  (:import-from #:serapeum
                #:dict
                #:maybe-invoke-restart
                #:fmt
                #:take
                #:length<)
  (:import-from #:reblocks/session
                #:expire-session
                #:init-session
                #:map-sessions
                #:do-sessions)
  (:import-from #:bordeaux-threads
                #:make-recursive-lock
                #:make-thread
                #:with-lock-held
                #:with-recursive-lock-held
                #:thread-alive-p
                #:make-lock)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:uuid
                #:uuid
                #:make-uuid-from-string
                #:uuid=
                #:make-v4-uuid)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  
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
   #:in-page-context-p
   #:page-id
   #:get-page-by-id
   #:init-page
   #:page
   #:page-root-widget
   #:on-page-refresh
   #:on-page-redirect
   #:on-page-load
   #:extend-expiration-time
   #:page-app
   #:with-metadata-lock
   #:ensure-page-metadata
   #:find-widget-by-id
   #:body-classes))
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
  ((id :initform (make-v4-uuid)
       :type uuid:uuid
       :reader page-id)
   (path :initarg :path
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
   (root-widget :initform nil
                :initarg :root-widget
                :accessor page-root-widget)
   (id-to-widget :initform (dict)
                 :reader id-to-widget)
   (app :initarg :app
        :initform *current-app*
        :accessor page-app)
   (metadata-lock :initform (make-recursive-lock "Page Metadata")
                  :accessor %metadata-lock)
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

(defgeneric body-classes (app)
  (:documentation "Should return a string of CSS classes for the body HTML element or NIL.")
  (:method ((app t))
    nil))


(defmethod render ((app app)
                   inner-html
                   &key (dependencies (get-dependencies app)))
  "Default page rendering template and protocol."
  (log:debug "Rendering page for" app)
  (unless (boundp '*title*)
    (error "Method REBLOCKS/PAGE:RENDER should be called inside WITH-PAGE-DEFAULTS block."))

  ;; (register-dependencies dependencies)

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
        :class (body-classes app)
        (render-body app inner-html)
        ;; (:script :type "text/javascript"
        ;;          "updateWidgetStateFromHash();")
        )))))


(defun find-page-by-path (session-pages path)
  (check-type session-pages hash-table)
  (check-type path string)
  (loop for page being the hash-key of session-pages
        when (string-equal (page-path page)
                           path)
        do (return page)))


(defun call-with-page-defaults (body-func)
  (let* ((*title* nil)
         (*description* nil)
         (*keywords* nil)
         (*language* *default-language*)
         (session-pages
           (reblocks/session:get-value 'session-pages))
         (*current-page* (let* ((path (get-path))
                                (expire-in (page-expire-in *current-app*
                                                           path))
                                (expire-at (when expire-in
                                             (universal-to-timestamp
                                              (+ (get-universal-time)
                                                 expire-in)))))
                           (or (find-page-by-path session-pages path)
                               (progn (log:debug "Initializing a new page for path ~A" path)
                                      (init-page *current-app* path expire-at)))))
         (max-pages (max-pages-per-session *current-app*))
         (next-year (universal-to-timestamp
                     (+ (get-universal-time)
                        (* 365 24 60 60)))))
    ;; Here we are adding a new session object to the map of known sessions
    (setf (gethash *current-page* session-pages)
          t)

    (unless (ajax-request-p)
      (log:debug "Calling ON-PAGE-LOAD generic-function")
        
      (on-page-load *current-page*))
    
    (when (refresh-request-p)
      (log:debug "Calling ON-PAGE-REFRESH generic-function")
        
      (on-page-refresh *current-page*))
    
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

                   Default method returns current value of *EXTEND-PAGE-EXPIRATION-BY* variable or *PAGES-EXPIRE-IN* variable.")
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


(defun expire-pages ()
  "Removes from memory expired pages. This function is called periodically
   from a separate thread.

   Returns a multiple values with counters:

   - processed-sessions
   - processed-pages
   - expired-sessions-count
   - sessions-without-restart-count (non-expired but should be expired, if reblocks works correctly you should see 0 here)
   - expired-pages-count
"
  (let ((processed-sessions 0)
        (processed-pages 0)
        (expired-pages-count 0)
        (expired-sessions-count 0)
        (sessions-without-restart-count 0))
    (labels ((expire-session-pages (pages)
               (incf processed-sessions)
               (list
                (cond
                  (pages (process pages))
                  (t (try-to-expire-session)))))
             (try-to-expire-session ()
               (let ((restart (find-restart 'expire-session)))
                 (cond
                   (restart
                    (incf expired-sessions-count)
                    (invoke-restart restart))
                   (t
                    (incf sessions-without-restart-count)))))
             (process (pages)
               (loop with now = (now)
                     for page being the hash-key of pages
                     for expire? = (and (page-expire-at page)
                                        (timestamp< (page-expire-at page)
                                                    now))
                     do (incf processed-pages)
                     when expire?
                     do (log:debug "Expiring page" page)
                     and collect page into expired-pages
                     finally (mapc (lambda (page)
                                     (remhash page pages))
                                   expired-pages)
                             (incf expired-pages-count
                                   (length expired-pages))
                             ;; If all pages were expired, then
                             ;; we'll expire session too, because we don't want
                             ;; these empty sessions to fill the memory.
                             ;; 
                             ;; TODO: Probably we need to setup a separate TTL
                             ;;       for empty sessions and expire them after longer interval?
                             ;;       Or maybe we have to create a way to dump such sessions into the database?
                             (when (zerop (hash-table-count pages))
                               (try-to-expire-session))
                             (return pages))))
      
      (declare (dynamic-extent #'expire-session-pages
                               #'try-to-expire-session
                               #'process))
      (when reblocks/session::!map-sessions
        (map-sessions #'expire-session-pages 'session-pages)))

    (values processed-sessions
            processed-pages
            expired-sessions-count
            sessions-without-restart-count
            expired-pages-count)))


(defun extend-expiration-time-impl (app page)
  (when (and page
             (page-expire-at page))
    (setf (page-expire-at page)
          (timestamp-maximum
           (universal-to-timestamp
            (+ (get-universal-time)
               (extend-page-expiration-by app page)))
           (page-expire-at page)))))


(defun extend-expiration-time ()
  "Extends expiration time of the current page."
  (extend-expiration-time-impl *current-app* *current-page*))


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
    (null (gethash page (reblocks/session:get-value 'session-pages)))))


(defun initialize-session-pages ()
  (setf (reblocks/session:get-value 'session-pages)
        ;; Here we don't use weak hash table,
        ;; because this map will keep pages
        ;; in memory, while in all other places
        ;; we need to use weak references.
        ;; This way, memory will be freed when
        ;; pages are expiring.
        (make-hash-table)))


(defmethod init-session :before ((app t))
  (initialize-session-pages))


(defmacro with-metadata-lock ((page) &body body)
  `(with-recursive-lock-held ((%metadata-lock ,page))
     ,@body))


(defun page-metadata (page name)
  "Returns a metadata with NAME, bound to the current page"
  (with-metadata-lock (page)
    (gethash name (%page-metadata page))))


(defun (setf page-metadata) (value page name)
  (with-metadata-lock (page)
    (setf (gethash name (%page-metadata page))
          value)))


(defmacro ensure-page-metadata (page name &optional default)
  "Like PAGE-METADATA, but if metadata piece with NAME is not found saves the DEFAULT
   before returning it. Secondary return value is true if NAME was
   already in the metadata."
  (once-only (page name)
    (with-unique-names (value presentp)
      `(with-metadata-lock (,page)
         (multiple-value-bind (,value ,presentp) (gethash ,name (%page-metadata ,page))
           (if ,presentp
               (values ,value ,presentp)
               (values (setf (gethash ,name (%page-metadata ,page))
                             ,default)
                       nil)))))))


(defun in-page-context-p ()
  "Returns T when CURRENT-PAGE function will be able to return page instead of throwing an error."
  (and (boundp '*current-page*)
       *current-page*))


(defun current-page ()
  "Returns current page object. Can be useful to pass to call PAGE-METADATA."
  (unless (in-page-context-p)
    (error "Please, run function CURRENT-PAGE in the context where current page is known."))
  *current-page*)


(defun get-page-by-id (id)
  "Returns a page from a current session by it's id."
  (let* ((id (etypecase id
               (string (make-uuid-from-string id))
               (uuid id)))
         (session-pages (reblocks/session:get-value 'session-pages)))
    (when session-pages
      (find id (hash-table-keys session-pages)
            :key #'page-id
            :test #'uuid=))))


(defgeneric init-page (app url-path expire-at)
  (:documentation "A method for this generic function should be defined to initialize a new page object.

                   It should return a widget which become a root widget of the page or it might return the
                   page with initialized root widget in case if you want to use your own subclass of the PAGE class."))



(defgeneric on-page-load (page)
  (:documentation "This generic function gets called when user loads a page in the browser.
                   It is called for all non Ajax requests.

                   When user reloads page multiple times, the page object can be reused.
                   Thus this function may be used to reset some page attributes.

                   Default method resets a list of loaded dependencies to ensure that all of them
                   will be sent to the browser again."))


(defgeneric on-page-refresh (page)
  (:documentation "This generic function gets called when user refreshes page in the browser.

                   Default method does nothing.")
  (:method ((page t))))


(defgeneric on-page-redirect (from-page to-url)
  (:documentation "This generic function gets called when user get's a redirect to another page.

                   Default method does nothing.")
  (:method ((from-page t) (to-url t))))


(defun find-widget-by-id (widget-id)
  (gethash widget-id (id-to-widget (current-page))))


(defun register-widget (widget)
  (when (in-page-context-p)
    (setf (gethash (dom-id widget)
                   (id-to-widget (current-page)))
          widget)))
