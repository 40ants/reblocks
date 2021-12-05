(defun search-version-in-changelog (lines)
  (let* ((line (nth 4 lines))
         (space-pos (position #\Space line)))
    (when space-pos
      (subseq line 0 space-pos))))


(defsystem reblocks
  :name "reblocks"
  :class :package-inferred-system
  :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :maintainer "Alexander Artemenko, Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :pathname "src"
  :depends-on ("reblocks/app"
               "reblocks/server"
               "reblocks/debug"
               "reblocks/default-init"
               "reblocks/commands-hook"
               "reblocks/widgets/string-widget"
               "reblocks/widgets/funcall-widget"
               "reblocks/utils/clos"
               "reblocks/utils/i18n")
  :in-order-to ((test-op (test-op "reblocks-test"))))


(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-middleware-session" '(#:lack.middleware.session
                                                      #:lack.session.state.cookie
                                                      #:lack.middleware.session.store.memory))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "log4cl" '(#:log))
