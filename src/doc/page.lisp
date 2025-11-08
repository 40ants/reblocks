(uiop:define-package #:reblocks/doc/page
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/page
                #:body-classes
                #:find-widget-by-id
                #:page
                #:init-page
                #:on-page-refresh
                #:on-page-redirect
                #:on-page-load
                #:extend-expiration-time
                
                #:current-page
                #:ensure-page-metadata
                #:extend-page-expiration-by
                #:get-page-by-id
                #:in-page-context-p
                #:max-pages-per-session
                #:page-app
                #:page-expire-in
                #:page-id
                #:page-metadata
                #:page-root-widget
                #:with-metadata-lock
                )
  (:import-from #:reblocks/variables
                #:*max-pages-per-session*
                #:*pages-expire-in*
                #:*extend-page-expiration-by*
                #:*delay-between-pages-cleanup*))
(in-package #:reblocks/doc/page)


(defsection @page (:title "Pages"
                   :ignore-words ("REBLOCKS"
                                  "REBLOCKS/PAGE"
                                  "CSS"
                                  "HTML"
                                  "HTTP"))

  "
The functions and macros to work with pages are defined in the
`REBLOCKS/PAGE` package.

# Page life-cycle

When a user opens a new page of the app, the INIT-PAGE generic function
gets called. It should return a root widget or an object of the PAGE class.

If the variable *PAGES-EXPIRE-IN* is not NIL, then the page will be deleted from memory
after this number of seconds. However, each action call extends the expiration time
to *EXTEND-PAGE-EXPIRATION-BY* seconds.

The variable *MAX-PAGES-PER-SESSION* controls the maximum number of pages in one user
session. Older pages will be expired at the moment when a user tries to open a new page.

These settings are NIL by default, but may be used to protect the server from DoS attacks
where an attacker tries to fill all server memory with useless pages.

If you define your own page class, you can define
PAGE-EXPIRE-IN, EXTEND-PAGE-EXPIRATION-BY and MAX-PAGES-PER-SESSION generic functions
to control how long the page should live.

# Page metadata

Any data can be associated with a page. For example, dependencies loaded into the page
are saved into the metadata to avoid asset duplication.

You can get the current page using the CURRENT-PAGE function and then retrieve metadata with
PAGE-METADATA and change it using (SETF PAGE-METADATA) or the ENSURE-PAGE-METADATA macro.
Warning: if there are parallel threads which might change metadata,
then you should wrap all changing code in the WITH-METADATA-LOCK macro.
"
  ;; "# API"
  ;; (page class)
  ;; (page-id (reader page))
  ;; (page-app (accessor page))
  ;; (page-root-widget (accessor page))

  ;; (init-page generic-function)
  ;; (on-page-refresh generic-function)
  ;; (on-page-load generic-function)
  ;; (on-page-redirect generic-function)
  ;; (extend-page-expiration-by generic-function)
  ;; (max-pages-per-session generic-function)
  ;; (page-expire-in generic-function)

  ;; (body-classes generic-function)
  ;; (page-metadata function)
  ;; (extend-expiration-time function)
  ;; (current-page function)
  ;; (get-page-by-id function)
  ;; (in-page-context-p function)
  ;; (find-widget-by-id function)
  
  ;; (ensure-page-metadata macro)
  ;; (with-metadata-lock macro)

  ;; (*max-pages-per-session* variable)
  ;; (*pages-expire-in* variable)
  ;; (*extend-page-expiration-by* variable)
  ;; (*delay-between-pages-cleanup* variable)
  )

