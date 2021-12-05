(defpackage #:weblocks/default-init
  (:use #:cl)
  (:import-from #:weblocks/app
                #:app-active-p
                #:get-active-apps
                #:get-prefix
                #:webapp-name
                #:defapp)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:weblocks/session
                #:init)
  (:import-from #:weblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:weblocks/html
                #:with-html-string)
  (:import-from #:weblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/app
                #:*current-app*)
  (:export #:welcome-screen-app))
(in-package weblocks/default-init)


(defmethod init ((app t))
  (let ((quickstart-url "http://40ants.com/weblocks/quickstart.html"))
    (make-string-widget
     (with-html-string
       (:h1 "No weblocks/session:init method defined.")
       (:p "Please define a method weblocks.session:init to initialize a session.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         ("(defmethod weblocks/session:init ((app ~A))  
            \"Hello world!\")" (string-downcase
                                (type-of *current-app*)))))

       (:p ("Read more in [documentaion]()."
            quickstart-url)))
     :escape-p nil)))


(defmethod init :around ((app t))
  "If init function returned not object inherited from widget, it calls
   create-widget-from method, to transform value into the widget."

  (let ((root (call-next-method)))
    (create-widget-from root)))

(defapp welcome-screen-app
    :prefix "/")

(defparameter *welcome-screen-enabled* t
  "If t, show the welcome screen on the root url, if no other widget use the path.")

(defwidget welcome-screen-widget ()
  ())

(defmethod render ((widget welcome-screen-widget))
  (let ((apps (weblocks/app:get-active-apps)))
    (with-html
      (:h1 "Welcome to Weblocks!")
      (:div "To learn more about Weblocks, head over its "
            (:a :href "http://40ants.com/weblocks/" "documentation.")
            " To learn how to create apps and widgets, see the "
            (:a :href "http://40ants.com/weblocks/quickstart.html" "quickstart guide")
            ".")
      (:h3)
      (when apps
        (:div "You also have active apps, congratulations! You are running:")
        (loop for app in apps
           do
             (let ((prefix (get-prefix app)))
               (:ul
                (:li
                 (:a :href prefix (format nil "~a" (weblocks/app::webapp-name app)))
                 (:span (format nil " on \"~a\"" prefix))))))

        (:h3)
        (when (app-active-p 'welcome-screen-app)
          (:div "This is the welcome screen, a Weblocks app itself.")
          (:div "If you want to stop it, do:")
          (:code "(weblocks/app:stop 'weblocks/default-init:welcome-screen-app)")
          (:div "And in case you want to disable it before the server starts, do:")
          (:code "(setf weblocks/default-init::*welcome-screen-enabled* nil)")
          (:h3)
          (:div "If you want to bind your app to \"/\", use the prefix argument of defapp:")
          (:code "(defapp myapp :prefix \"/\")"))))))

(defmethod weblocks/session:init ((app welcome-screen-app))
  (make-instance 'welcome-screen-widget))

(when *welcome-screen-enabled*
  (weblocks/app:start 'welcome-screen-app))
