(uiop:define-package #:reblocks/widgets/default-page
  (:use #:cl)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/variables
                #:*current-app*))
(in-package #:reblocks/widgets/default-page)


(defun make-default-init-page-widget ()
  (let ((quickstart-url "http://40ants.com/reblocks/quickstart/"))
    (make-string-widget
     (with-html-string
       (:h1 "No reblocks/page:init-page method defined.")
       (:p "Please define a method reblocks/page:init-page to initialize a new page.")
       (:p "It could be something simple, like this one:")
       (:pre
        (:code
         (format nil "
CL-USER> (defmethod reblocks/page:init-page ((app ~A) (url-path string) expire-at)
           (check-type expire-at (or null local-time::timestamp))
           \"Hello world!\")" (string-downcase
                               (type-of *current-app*)))))
       (:p "And reset current session:")
       (:pre
        (:code
         "
CL-USER> (reblocks/debug:reset-latest-session)"))

       (:p "Then reload the page.")
       (:p "Read more in "
           (:a :href quickstart-url
               "documentation")
           "."))
     :escape-p nil)))
