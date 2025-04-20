(uiop:define-package #:reblocks/welcome/widget
  (:use #:cl)
  (:import-from #:reblocks/server
                #:get-port
                #:get-interface)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/welcome/app
                #:welcome-screen-app)
  (:import-from #:reblocks/app
                #:get-prefix
                #:webapp-name
                #:*current-app*)
  (:import-from #:local-time)
  (:import-from #:reblocks/page))
(in-package #:reblocks/welcome/widget)


(defwidget welcome-screen-widget ()
  ())


(defmethod reblocks/page:init-page ((app welcome-screen-app) url-path expire-at)
  (check-type url-path (or null string))
  (check-type expire-at (or null local-time:timestamp))
  (make-instance 'welcome-screen-widget))


(defmethod render ((widget welcome-screen-widget))
  ;; I have to comment out this code because app activity was refactored in the
  ;; latest version. We need to rethink this idea of a welcome screen.
  ;; 
  (let ((apps (reblocks/server::apps reblocks/server::*server*)))
    (with-html
      (:h1 "Welcome to Reblocks!")
      (:p ("To learn more about Reblocks, head over its [documentation](http://40ants.com/reblocks/)."))
      (:p ("To learn how to create apps and widgets, see the [quickstart guide](http://40ants.com/reblocks/quickstart/)."))
      (:h3)
      (when apps
        (:div "You have some apps, congratulations! Here they are:")
        (:ul
         (loop for app in apps
               for prefix = (get-prefix app)
               for app-name = (webapp-name app)
               do (:li
                   ;; After routes refactoring, all apps urls should end with a slash
                   (:a :href (str:ensure-suffix "/"
                                                prefix)
                       (format nil "~A" app-name))
                   (:span (format nil " on \"~A\"" prefix)))))
        
        (:h3)
        ;; Just in case if this widget will be used in some
        ;; other apps:
        (when (typep *current-app* 'welcome-screen-app)
          (:p "This is the welcome screen, a Reblocks app itself.")
          (:p "If you want to disable this welcome screen, just define an application with \"/\" prefix:")
          (:pre (:code "
CL-USER> (reblocks/app:defapp my-app
           :prefix \"/\")"))
          (:h3)
          (:p "And then restart the server like this:")
          (let* ((server reblocks/server::*server*)
                 (interface (get-interface server))
                 (port (get-port server)))
            (:pre (:code (format nil "
CL-USER> (reblocks/server:stop ~S ~S)

CL-USER> (reblocks/server:start :interface ~S
                                :port ~S)
"
                                 interface port
                                 interface port)))))))))
