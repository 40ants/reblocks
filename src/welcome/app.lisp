(uiop:define-package #:reblocks/welcome/app
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp))
(in-package #:reblocks/welcome/app)


(defapp welcome-screen-app
  :documentation "Idea of this app is to have some default application will be served on \"/\"
                  prefix when there is no other app for \"/\" prefix is defined."
  :prefix "/"
  :autostart nil)
