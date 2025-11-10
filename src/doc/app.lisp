(uiop:define-package #:reblocks/doc/app
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/app
                #:defapp
                #:app
                #:get-registered-apps
                #:get-autostarting-apps
                #:get-current
                #:get-prefix
                #:initialize-webapp
                #:with-app))
(in-package #:reblocks/doc/app)


(defsection @apps (:title "Apps"
                   :ignore-words ("URI"
                                  "URL"
                                  "WEBAPP"))
  "
Reblocks sites contain one or more apps, where each app is responsible
for serving requests starting from a specific prefix.

For example, a site can have a few apps mounted to `/`, `/shop/`, `/blog/` and `/admin/`
prefixes.

The simplest way to define a new app is to use the DEFAPP macro like this:

```
(defapp blog
  :prefix \"/blog/\")
```

And then define a method for the REBLOCKS/SESSION:INIT generic function. It will be called if
a user opens https://yoursite.com/blog/ in their browser.

"
  ;; ## API

  ;; (defapp macro)
  ;; (app class)
  ;; (get-registered-apps function)
  ;; (get-autostarting-apps function)
  ;; (get-current function)
  ;; (get-prefix (reader app))
  ;; (initialize-webapp generic-function)
  ;; (with-app macro)

  ;; ;; TODO: probably, we should move these to a separate documentation chapter
  ;; ;; about the "rendering flow".
  ;; (reblocks/page:render-headers generic-function)
  )
