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
  :homepage "https://40ants.com/reblocks/"
  :source-control (:git "https://github.com/40ants/reblocks")
  :pathname "src"
  :depends-on ("40ants-doc"
               "reblocks/app"
               "reblocks/server"
               "reblocks/debug"
               "reblocks/default-init"
               "reblocks/commands-hook"
               "reblocks/widgets/string-widget"
               "reblocks/widgets/funcall-widget"
               "reblocks/welcome/widget"
               "reblocks/utils/clos"
               "reblocks/utils/i18n"
               "reblocks/preview"
               ;; We need this because this module defines important method
               ;; make-js-backend
               "reblocks/js/jquery"
               ;; to load js dependencies after app was started
               "reblocks/app-dependencies"
               ;; This package defines an :around method for reblocks/widgets:render
               ;; which adds a wrapper around widget body
               "reblocks/widgets/render-methods"
               ;; we need to depend on this package, because
               ;; lack:builder will try to find `LACK.MIDDLEWARE.SESSION`
               ;; package
               "lack-middleware-session")
  :in-order-to ((test-op (test-op "reblocks-tests"))))


(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-response" '(#:lack.response))
(register-system-packages "lack-middleware-session" '(#:lack.middleware.session
                                                      #:lack.session.store
                                                      #:lack.session.state.cookie
                                                      #:lack.middleware.session.store.memory))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "log4cl" '(#:log))
