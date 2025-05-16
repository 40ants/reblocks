(defpackage #:reblocks/doc/api
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/cached-dependencies-mixin)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/html)
  (:import-from #:reblocks/actions)
  (:import-from #:reblocks/response)
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/variables)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export
   #:@api))
(in-package #:reblocks/doc/api)


(defautodoc @api
    (:title "API"
     :system "reblocks"
     :ignore-words ("JS"
                    "URI"
                    "URL"
                    "API"
                    "TODO"
                    "RFC"
                    "AJAX"
                    "CSS"
                    "CDN"
                    "HTML"
                    "MIME"
                    "HTTP"
                    "CDATA"
                    "POST"
                    "IP"
                    "CPU"
                    "ID"
                    "CLOS"
                    "DIV"
                    "DOM")

     :external-docs ("https://40ants.com/routes/")
     
     ;; :ignore-words ("API"
     ;;                "URI"
     ;;                "HTTP"
     ;;                "HTML"
     ;;                "SQL"
     ;;                "ON-SESSION-HOOK-CREATE-USER"
     ;;                "ON-REQUEST-HOOK-CREATE-USER"
     ;;                "ON-APPLICATION-HOOK-CREATE-USER"
     ;;                "ON-APPLICATION-HOOK-RENDER"
     ;;                "WITH-CREATE-USER-HOOK"
     ;;                "CALL-CREATE-USER-HOOK"
     ;;                "WITH-*-HOOK"
     ;;                "CALL-*-HOOK"
     ;;                "ON-APPLICATION-HOOK"
     ;;                "ON-SESSION-HOOK"
     ;;                "ON-REQUEST-HOOK"
     ;;                "CALL"
     ;;                "WITH"
     ;;                "REBLOCKS"
     ;;                "HOOKS"
     ;;                "HTML"
     ;;                "DIV"
     ;;                "CSS"
     ;;                "CL-WHO"
     ;;                "UI"
     ;;                "REBLOCKS-UI"
     ;;                "HTML"
     ;;                "CSS"
     ;;                "JS"
     ;;                "AJAX"
     ;;                "JS"
     ;;                "URL"
     ;;                "POST"
     ;;                "GET"
     ;;                "HTML"
     ;;                "API"
     ;;                "USER"
     ;;                "REBLOCKS"
     ;;                "REBLOCKS/PAGE"
     ;;                "CSS"
     ;;                "HTML"
     ;;                "HTTP"
     ;;                "ASDF"
     ;;                "TODO"
     ;;                "CLOS"
     ;;                "REPL"
     ;;                "POST"
     ;;                "HTML"
     ;;                "DOM"
     ;;                "UI"
     ;;                "DONE"
     ;;                "ADD-TASK"
     ;;                "ONCLICK"
     ;;                "TOGGLE"
     ;;                "TASK-LIST"
     ;;                "RENDER"
     ;;                "AJAX"
     ;;                "URL"
     ;;                "SETF"
     ;;                "AJAX"
     ;;                "HTTP")
     ))


;; (defsection @api (:title "Unsorted API"
;;                   :ignore-words ("API"
;;                                  "URI"))
;;   (reblocks/widgets/mop:widget-class class)
;;   (reblocks/response:redirect function)
;;   (reblocks/session:init generic-function)

;;   "## Variables"

;;   (reblocks/variables:*current-app* variable)
;;   (reblocks/variables:*default-content-type* variable)
;;   (reblocks/variables:*invoke-debugger-on-error* variable)
;;   (reblocks/variables:*backtrace-on-session-init-error* variable))
