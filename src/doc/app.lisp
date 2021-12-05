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
(in-package reblocks/doc/app)


(defsection @apps (:title "Apps"
                   :ignore-words ("URI"
                                  "URL"
                                  "WEBAPP"))
  "
Reblocks sites contains one or more apps. Where each app is responsible
for serving requests starting from some prefix.

For example, your site can have a few apps mounted to `/`, `/shop/`, `/blog/` and `/admin/`
prefixes.

The simplest way to define a new app is to use DEFAPP macro like this:

```
(defapp blog
  :prefix \"/blog/\")
```

And then to define a method for REBLOCKS/SESSION:INIT generic-function. It will be called if
user will open https://yoursite.com/blog/ in his browser.

## API"

  (defapp macro)
  (app class)
  (get-registered-apps function)
  (get-autostarting-apps function)
  (get-current function)
  (get-prefix (reader app))
  (initialize-webapp generic-function)
  (with-app macro))
