(defpackage #:reblocks/doc/api
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/cached-dependencies-mixin)
  (:import-from #:reblocks/widget)
  (:import-from #:reblocks/html)
  (:import-from #:reblocks/routes)
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
     :external-docs ("https://40ants.com/log4cl-extras/"
                     "https://40ants.com/routes/"
                     "https://40ants.com/reblocks-ui"
                     "https://40ants.com/reblocks-navigation-widget/")))
