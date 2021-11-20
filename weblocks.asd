;; Временно сохранил зависимости
;; (ql:quickload '(
;;                 :log4cl
;;    :clack
;;    :lack-request
;;    :routes
;;    :function-cache
;;    :local-time
;;    :local-time-duration
;;    :dexador ;; to retrive remote dependencies and put them into the cache
;;    :closer-mop
;;    :puri
;;    :cl-json
;;    :alexandria
;;    :serapeum ;; utilities, like alexandria
;;    :spinneret
;;    :parenscript
;;    :cl-fad
;;    :optima
;;    :cl-cont
;;    :metatilities
;;    :cl-ppcre
;;    :anaphora
;;    :f-underscore
;;    :bordeaux-threads
;;    :salza2
;;    :html-template
;;    :trivial-timeout
;;    :trivial-backtrace 
;;    :parse-number 
;;    :pretty-function 
;;    :babel
;;    :metacopy
;;    :split-sequence
;;    :cl-strings
;;                 ))

#-asdf3.1
(error "weblocks requires at least ASDF 3.1")

(defun search-version-in-changelog (lines)
  (let* ((line (nth 4 lines))
         (space-pos (position #\Space line)))
    (when space-pos
      (subseq line 0 space-pos))))


(defsystem weblocks
  :name "weblocks"
  :class :package-inferred-system
  :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :maintainer "Alexander Artemenko, Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :pathname "src"
  :depends-on ("weblocks/doc/index"
               "weblocks/app"
               "weblocks/server"
               "weblocks/debug"
               "weblocks/default-init"
               "weblocks/widgets/string-widget"
               "weblocks/widgets/funcall-widget"
               "weblocks/utils/clos"
               "weblocks/utils/i18n")
  :in-order-to ((test-op (test-op "weblocks-test"))))


(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-middleware-session" '(#:lack.middleware.session
                                                      #:lack.session.state.cookie
                                                      #:lack.middleware.session.store.memory))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "log4cl" '(#:log))
