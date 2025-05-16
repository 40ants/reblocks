(defpackage #:todo
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/routes
                #:page)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get))
(in-package #:todo)


;;;;;; Application

(defapp tasks
  :prefix "/"
  :routes ((page ("/" :name "tasks-list")
             (make-string-widget "Hello World!"))))



(defun start (&key (port 8080))
  (reblocks/server:start :port port
                         :apps '(tasks)))
