;; (defun search-version-in-changelog (lines)
;;   (let* ((line (nth 4 lines))
;;          (space-pos (position #\Space line)))
;;     (when space-pos
;;       (subseq line 0 space-pos))))


(defsystem reblocks
  :name "reblocks"
  :class :package-inferred-system
  ;; TODO: Take version from the src/doc/changelog.lisp
  ;; :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :maintainer "Alexander Artemenko"
  :author "Alexander Artemenko"
  :licence "LLGPL"
  :description "A Common Lisp web framework, successor of the Weblocks."
  :pathname "src"
  :depends-on ("reblocks/app"
               "reblocks/server"
               "reblocks/debug"
               "reblocks/default-init"
               "reblocks/commands-hook"
               "reblocks/widgets/string-widget"
               "reblocks/widgets/funcall-widget"
               "reblocks/utils/clos"
               "reblocks/utils/i18n"
               "reblocks/preview")
  :in-order-to ((test-op (test-op "reblocks-test"))))


(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-middleware-session" '(#:lack.middleware.session
                                                      #:lack.session.state.cookie
                                                      #:lack.middleware.session.store.memory))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "log4cl" '(#:log))
