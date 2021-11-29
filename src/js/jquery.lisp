(defpackage #:weblocks/js/jquery
  (:use #:cl)
  (:import-from #:weblocks/js
                #:js-backend
                #:make-js-backend)
  (:import-from #:weblocks/dependencies
                #:get-dependencies
                #:make-dependency)
  ;; Just dependencies
  (:import-from #:log))
(in-package weblocks/js/jquery)


(defclass jquery-backend (js-backend)
  ())


(defmethod make-js-backend ((name (eql :jquery)))
  (make-instance 'jquery-backend))


(defvar *js-dependencies*
  (list (make-dependency "src/js/jquery/jquery-3.6.0.min.js"
                         ;; "https://code.jquery.com/jquery-3.6.0.min.js"
                         :system :weblocks)
        (make-dependency "src/js/jquery/jquery.js"
                         :system :weblocks)

        ;; TODO: read code in jquery.js and learn how it uses ba-bbq and seq plugins.
        ;;       Probably we don't need them anymore. These plugins are not working
        ;;       with recent jQuery version 3.6.0, but were loaded without errors
        ;;       on jQuery 1.8.2.
        ;; (make-dependency "src/js/jquery/jquery.ba-bbq.js"
        ;;                  :system :weblocks)
        ;; (make-dependency "src/js/jquery/jquery-seq.js"
        ;;                  :system :weblocks)

        (make-dependency "src/js/jquery/progress.gif"
                         :system :weblocks)))


(defmethod get-dependencies ((self jquery-backend))
  (log:debug "Returning dependencies for jquery backend.")
  (append *js-dependencies*
          (call-next-method)))
