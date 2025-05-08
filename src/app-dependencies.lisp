(defpackage #:reblocks/app-dependencies
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks/app
                #:%get-js-backend
                #:app))
(in-package #:reblocks/app-dependencies)


(defmethod get-dependencies ((self app))
  (log:debug "Returning new-style dependencies for base application class.")
  
  (get-dependencies
   (%get-js-backend self)))


