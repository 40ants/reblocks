(uiop:define-package #:reblocks/page-dependencies
  (:use #:cl)
  (:import-from #:reblocks/app
                #:app)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:import-from #:log)
  (:import-from #:reblocks/dependencies
                #:dependency-equal
                #:get-url)
  (:import-from #:reblocks/page
                #:on-page-load
                #:on-page-redirect
                #:page
                #:on-page-refresh
                #:current-page
                #:in-page-context-p
                #:render
                #:page-metadata
                #:render-page-with-widgets)
  (:import-from #:reblocks/html
                #:*lang*
                #:with-html
                #:get-rendered-chunk)
  (:export
   #:with-collected-dependencies
   #:get-collected-dependencies
   #:push-dependencies
   #:already-loaded-p
   #:page-dependencies
   #:push-dependency))
(in-package #:reblocks/page-dependencies)


(defvar-unbound *page-dependencies*
  "A list which contains all page dependencies.

Reblocks fills this list during page rendering.")


(defmacro with-collected-dependencies (&body body)
  "Use this macro to wrap code which may push new dependencies for
the page or an action."
  `(let (*page-dependencies*)
     ,@body))


(defun push-dependency (dependency)
  "Pushes dependency into the currently collected list of dependencies.

Makes deduplication by comparing dependencies' urls."
  
  (unless (boundp '*page-dependencies*)
    (error "Please, use push-dependency in code, wrapped with with-collected-dependencies macro."))

  (cond
    ((in-page-context-p)
     (let* ((page (current-page)))
       ;; TODO: this check makes hard to update
       ;; css and js via websocket, because it
       ;; prevents some files loading. I need
       ;; to find a way how to disable this when
       ;; hot CSS/JS reloading is enabled
       (unless (already-loaded-p page dependency)
         (push dependency (page-dependencies (current-page)))
         (pushnew dependency *page-dependencies*
                  :key #'get-url
                  :test #'string-equal))))
    (t
     ;; If application wide action was called, then symbol *current-page*
     ;; will be unbound and we collect dependencies just inside *page-dependencies*:
     (pushnew dependency *page-dependencies*
              :key #'get-url
              :test #'string-equal))))


(defun push-dependencies (list-of-dependencies)
  "Same as `push-dependency' but for the list."
  (mapc #'push-dependency
        list-of-dependencies))


(defun get-collected-dependencies ()
  (unless (boundp '*page-dependencies*)
    (error "Please, use push-dependency in code, wrapped with with-collected-dependencies macro."))

  ;; Dependencies returned as reversed list because that way
  ;; they will have same order as they were pushed.
  (remove-duplicates (reverse *page-dependencies*)
                     :key #'get-url
                     :test #'string-equal))


(defun page-dependencies (page)
  "Returns as list of dependencies loaded into the page. Most recently loaded go first."
  (page-metadata page :dependencies))


(defun (setf page-dependencies) (new-value page)
  "Associates a list of dependencies with the page."
  (setf (page-metadata page :dependencies)
        new-value))


(defun already-loaded-p (page dependency)
  "Returns T, if dependency is already loaded into the page."
  (when (member dependency
                (page-dependencies page)
                :test 'dependency-equal)
    t))


(defmethod render-page-with-widgets ((app app))
  (log:debug "Special Rendering page for" app)

  ;; At the moment when this method is called, there is already
  ;; rendered page's content in the reblocks/html::*stream*.
  ;; All we need to do now – is to render dependencies in the header
  ;; and paste content into the body.
  (let* ((rendered-html (get-rendered-chunk))
         (all-dependencies (get-collected-dependencies)))

    (render app rendered-html :dependencies all-dependencies)))


(defmethod on-page-load ((page page))
  (setf (page-dependencies page)
        nil))
