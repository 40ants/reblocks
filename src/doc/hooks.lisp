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
                #:symbolicate)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:reference-to-commondoc)
  (:import-from #:40ants-doc-full/commondoc/markdown
                #:parse-markdown)
  (:import-from #:40ants-doc-full/commondoc/bullet
                #:make-bullet))
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


(defmethod reference-to-commondoc
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
             (parse-markdown docstring))))
    (make-bullet reference
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
  "Reblocks provides a hooks mechanism to allow setting callbacks on different events.
   These callbacks are called \"hooks\". Each hook can be added at one of three levels:

   - application;
   - session;
   - request.

   Hooks added at the request level expire after the request was processed. This could be useful
   for committing changes to the database.

   Session level hooks have a longer life time and work while the user session is active.
   They are bound to a session. Some users might have a hook installed in their session and other users don't.
   This way you might implement features like turning on debug information, etc.

   Application level hooks are stored permanently and work for every user. Despite the name, they are not
   bound to an application. These hooks are global and work for any application and any Reblocks server.

   Hooks are defined to be called when some event happens in the Reblocks application.
   Here is a list of hooks, predefined in Reblocks:"

  (handle-http-request hook)
  (start-reblocks hook)
  (stop-reblocks hook)
  (action hook)
  (render hook)
  (reset-session hook)

  "# How to use these hooks?

   Let's pretend we want to create an extension which will collect all SQL queries
   and render them in a hidden panel like Django Debug Toolbar does. To accomplish
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

   As you can see, we've added a hook using the ON-APPLICATION-HOOK-RENDER macro
   and used the CALL-NEXT-HOOK function inside its body. This is very similar
   to generic functions, but instead of methods, we are calling nested hooks. Hooks added
   later wrap hooks added earlier.

   If you don't call the CALL-NEXT-HOOK function explicitly, it will be called implicitly
   after your hook's body. This makes your code called before hook's event.

   # Defining your own hooks

   You can use hook mechanics in your own code by defining and calling hooks.

   First, you need to define a hook using the DEFHOOK macro. Then choose a piece of code
   you want to make hookable and wrap it with another macro defined by DEFHOOK.

   For example, if you are creating a Reblocks extension which allows users to register
   on a website. You also want to allow developers who will use your extension
   to define additional actions when a new user gets registered on the site. For example,
   an admin might want to validate age and also send the user a greeting email after registration.

   To accomplish this task, we need to define a hook:


   ```
   (reblocks/hooks:defhook create-user (age name email)
       \"Called around the code which creates a new user. Returns a user object.\")
   ```

   This macro will define more macros inside the REBLOCKS/HOOKS package:

   * ON-SESSION-HOOK-CREATE-USER
   * ON-REQUEST-HOOK-CREATE-USER
   * ON-APPLICATION-HOOK-CREATE-USER
   * WITH-CREATE-USER-HOOK
   * CALL-CREATE-USER-HOOK

   Now, you need to use the WITH-CREATE-USER-HOOK macro to wrap around
   code creating the user. In some cases, when you just want to call hooks
   you can use `CALL-…-HOOK` version which is equal to calling `WITH-…-HOOK`
   without any body.

   Here is how you can use it:

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
"
  ;;    # Hooks API
  ;; (defhook macro)
  ;; (call-next-hook function)
  )
