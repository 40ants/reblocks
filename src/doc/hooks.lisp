(uiop:define-package #:reblocks/doc/hooks
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/hooks
                #:handle-http-request
                #:start-reblocks
                #:stop-reblocks
                #:action
                #:render
                #:reset-session
                #:call-next-hook
                #:on-application-hook-render
                #:defhook)
  (:import-from #:alexandria
                #:ensure-symbol
                #:symbolicate))
(in-package #:reblocks/doc/hooks)


(40ants-doc/locatives/base:define-locative-type hook ()
  "A reference to the hook")

(defmethod 40ants-doc/locatives/base:locate-object ((symbol symbol)
                                                    (locative-type
                                                     (eql '40ants-doc/locatives::hook))
                                                    locative-args)
  (40ants-doc/reference:make-reference symbol
                                       (cons locative-type locative-args)))


(defmethod 40ants-doc/locatives/base:locate-and-find-source (symbol (locative-type (eql '40ants-doc/locatives::hook))
                                                             locative-args)
  (declare (ignore locative-args))
  (40ants-doc/source-api:find-source
   (macro-function (ensure-symbol (symbolicate "WITH-" symbol "-HOOK")
                                  :reblocks/hooks))))


(defmethod 40ants-doc/commondoc/builder:reference-to-commondoc
    ((symbol symbol)
     (locative-type (eql '40ants-doc/locatives::hook))
     locative-args)
  (let* ((reference
           (40ants-doc/reference:make-reference symbol
                                                (cons locative-type locative-args)))
         (docstring
           (get symbol :docstring 
                "Hook is not documented"))
         (arglist
           (get symbol :args))
         (children
           (when docstring
             (40ants-doc/commondoc/markdown:parse-markdown docstring))))
    (40ants-doc/commondoc/bullet:make-bullet reference
                                             :arglist arglist
                                             :children children
                                             :ignore-words symbol)))


(defsection @hooks (:title "Hooks"
                    :ignore-words ("HTTP"
                                   "HTML"
                                   "SQL"
                                   "ON-SESSION-HOOK-CREATE-USER"
                                   "ON-REQUEST-HOOK-CREATE-USER"
                                   "ON-APPLICATION-HOOK-CREATE-USER"
                                   "ON-APPLICATION-HOOK-RENDER"
                                   "WITH-CREATE-USER-HOOK"
                                   "CALL-CREATE-USER-HOOK"
                                   "WITH-*-HOOK"
                                   "CALL-*-HOOK"
                                   "ON-APPLICATION-HOOK"
                                   "ON-SESSION-HOOK"
                                   "ON-REQUEST-HOOK"
                                   "CALL"
                                   "WITH"
                                   "REBLOCKS"
                                   "HOOKS"))
  "Reblocks provides a hooks mechanism to allow to set callbacks on different events.
   These callbacks are cooled \"hooks\". Each hook can be added on one of three levels:

   - application;
   - session;
   - request.

   Hooks added on request level expire after the request was processed. This could be useful
   for commiting changes to the database.

   Session level hooks are called have a longer life time and work while user session is active.
   They are bound to a session. Some user might have a hook installed in his session and other user don't.
   This way you might implement such things like turning on debug information, etc.

   Application level hooks are stored permanently and work for every user. Despite the name, they aren't
   bound to an application. These hooks are global and work for any application and any reblocks server.

   Hooks are defined to be called on some event happened in the reblocks application.
   Here is a list of hooks, predefined in the Reblocks:"

  (handle-http-request hook)
  (start-reblocks hook)
  (stop-reblocks hook)
  (action hook)
  (render hook)
  (reset-session hook)

  "# How to use these hooks?

   Lets pretend we want to create an extension which will collect all SQL queries
   and render them in hidden panel like Django Debug Toolbar does? To accomplish
   this task, we need to add an application-wide RENDER hook:

   ```
   (defun inject-debug-panel (html)
     (str:replace-first \"</html>\"
                        \"<div id=\\\"debug-panel\\\">
                            DEBUG PANEL EXAMPLE
                         </div>
                      </html>\"
                        html))

   (reblocks/hooks:on-application-hook-render
     collect-sql-queries-on-render (app)
     
     (let* ((*collect-sql* t)
            (resulting-html (reblocks/hooks:call-next-hook)))
       (inject-debug-panel resulting-html)))
   ```

   As you can see, we've added a hook using ON-APPLICATION-HOOK-RENDER macro
   and used CALL-NEXT-HOOK function inside it's body. This is very similar
   to generic functions, but instead of methods, we are calling nested hooks. Hooks added
   later wraps hooks added earlier.

   If you don't call CALL-NEXT-HOOK function explicitly, it will be called implicitly
   after your hook's body. This makes your code called before hook's event.

   # Defining your own hooks

   You can use hooks mechanics in your own code by defining and calling hooks.

   First, you need to define a hook using DEFHOOK macro. Then choose piece of code
   you want to make hookable and wrap it with another macro, defined by DEFHOOK.

   For example, you creating a Reblocks extension which allows users to register
   on a website. You also want to allow develope who will use your extension
   to define additional actions when a new user get registered on site. For example,
   admin might want to validate age and also to send user a greeting email after registration.

   To accomplish this task, we need to define a hook:


   ```
   (reblocks/hooks:defhook create-user (age name email)
       \"Called around the code which creates a new user. Returns a user object.\")
   ```

   This macro will define more macros inside REBLOCKS/HOOKS package:

   * ON-SESSION-HOOK-CREATE-USER
   * ON-REQUEST-HOOK-CREATE-USER
   * ON-APPLICATION-HOOK-CREATE-USER
   * WITH-CREATE-USER-HOOK
   * CALL-CREATE-USER-HOOK

   Now, you need to use WITH-CREATE-USER-HOOK macro to wrap around
   code creating the user. In some cases, when you just want to call hooks
   you can use `CALL-…-HOOK` version which is equal to calling `WITH-…-HOOK`
   without any body.

   Here is how you can use it now:

   ```
   (defun process-new-user-form (age name email)
     (reblocks/hooks:with-create-user-hook (age name email)
       ;; For simplicity, we using a plist here.
       ;; A real application should store the record
       ;; in a database and return class instance.
       (list :age age
             :name name
             :email email)))
   ```

   When we call this function, it will return our plist:

   ```
   REBLOCKS/DOC/HOOKS> (process-new-user-form 42 \"Sasha\" \"sasha@40ants.com\")
   (:AGE 42 :NAME \"Sasha\" :EMAIL \"sasha@40ants.com\")
   ```

   Now let's add a hook:

   ```
   (reblocks/hooks:on-application-hook-create-user
       check-age (age name email)
    (format t \"Checking age of a new user\"))
   ```

   and create a new user

   ```
   REBLOCKS/DOC/HOOKS> (process-new-user-form 9 \"Ilia\" \"ilia@40ants.com\")
   Checking age of a new user
   (:AGE 9 :NAME \"Ilia\" :EMAIL \"ilia@40ants.com\")
   ```

   # Hooks API
"
  (defhook macro)
  (call-next-hook function))
