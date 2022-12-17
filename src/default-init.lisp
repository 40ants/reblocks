(defpackage #:reblocks/default-init
  (:use #:cl)
  (:import-from #:reblocks/app
                #:get-prefix
                #:webapp-name
                #:defapp)
  (:import-from #:reblocks/session
                #:init)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:widget)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks/app
                #:*current-app*)
  (:import-from #:spinneret/cl-markdown))
(in-package #:reblocks/default-init)


(defmethod init ((app t))
  (let ((quickstart-url "http://40ants.com/reblocks/quickstart/"))
    (make-string-widget
     (with-html-string
       (:h1 "No reblocks/session:init method defined.")
       (:p "Please define a method reblocks/session:init to initialize a session.")
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
