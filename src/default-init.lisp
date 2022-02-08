(defpackage #:reblocks/default-init
  (:use #:cl)
  (:import-from #:reblocks/app
                #:get-prefix
                #:webapp-name
                #:defapp)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/session
                #:init)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/app
                #:*current-app*)
  (:import-from #:spinneret/cl-markdown))
(in-package #:reblocks/default-init)


(defmethod init ((app t))
  (let ((quickstart-url "http://40ants.com/reblocks/quickstart/"))
    (make-string-widget
     (with-html-string
       (:h1 "No reblocks/session:init method defined.")
       (:p "Please define a method reblocks.session:init to initialize a session.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         (format nil "
CL-USER> (defmethod reblocks/session:init ((app ~A))  
           \"Hello world!\")" (string-downcase
                               (type-of *current-app*)))))
       (:p "And reset current session:")
       (:pre
        (:code
         "
CL-USER> (reblocks/debug:reset-latest-session)"))

       (:p "Then reload the page.")
       (:p ("Read more in [documentaion](~A)."
            quickstart-url)))
     :escape-p nil)))


(defmethod init :around ((app t))
  "If init function returned not object inherited from widget, it calls
   create-widget-from method, to transform value into the widget."

  (let ((root (call-next-method)))
    (create-widget-from root)))


(defapp welcome-screen-app
  :documentation "Idea of this app is to have some default application will be served on \"/\"
                  prefix when there is no other app for \"/\" prefix is defined."
  :prefix "/"
  :autostart nil)


(defwidget welcome-screen-widget ()
  ())


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
               for app-name = (reblocks/app::webapp-name app)
               do (:li
                   (:a :href prefix (format nil "~A" app-name))
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
                 (interface (reblocks/server::get-interface server))
                 (port (reblocks/server::get-port server)))
            (:pre (:code (format nil "
CL-USER> (reblocks/server:stop ~S ~S)

CL-USER> (reblocks/server:start :interface ~S
                                :port ~S)
"
                                 interface port
                                 interface port)))))))))


(defmethod reblocks/session:init ((app welcome-screen-app))
  (make-instance 'welcome-screen-widget))
