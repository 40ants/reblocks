(uiop:define-package #:reblocks/hooks
  (:use #:cl
        #:f-underscore)
  ;; Just dependencies
  (:import-from #:reblocks/session)
  (:import-from #:log)
  (:import-from #:metatilities)
  (:import-from #:alexandria
                #:symbolicate
                #:ensure-symbol
                #:with-gensyms)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  
  (:export #:call-next-hook
           #:defhook))
(in-package reblocks/hooks)


(defclass hooks ()
  ((hooks :initform (make-hash-table)))
  
  (:documentation "A data structure that maintains appropriate
  callback functions used to hook into request evaluation."))


(defvar *application-hooks* (make-instance 'hooks)
  "A request hook object used in the application scope.")


(defun reset-session-hooks ()
  (let ((hooks (make-instance 'hooks)))
    (setf (reblocks/session:get-value 'hooks)
          hooks)
    hooks))


(defun get-or-create-session-hooks ()
  "A request hook object used in the session scope."
  (if (reblocks/session:get-value 'hooks)
      (reblocks/session:get-value 'hooks)
      (reset-session-hooks)))


;; Variables *session-hooks* or *request-hooks*
;; should be bound by prepare-hooks macro, during request processing.
(defvar *session-hooks* nil
        "A session hooks object is stored in user's session and should be
bound to this variable by `prepare-hooks' macro.")


(defvar *request-hooks* nil
  "A request hooks object used in the request scope.")


;; internal function
(defun add-hook (hooks hook-name callback-name callback)
  "Adds a callback to the callbacks list with name `hook-name'."
  (check-type hooks hooks)
  (check-type hook-name symbol)
  (check-type callback (or symbol function))
  (check-type callback-name symbol)

  (let ((hooks-hash (slot-value hooks 'hooks)))
    (if (assoc callback-name
               (gethash hook-name hooks-hash)
               :test #'eql)
        ;; replacing callback with given name
        (setf (cdr (assoc callback-name
                          (gethash hook-name hooks-hash)
                          :test #'eql))
              callback)
        ;; adding new callback
        (push (cons callback-name callback)
              (gethash hook-name hooks-hash)))))


(defun call-next-hook ()
  "This function should be called inside a body, surrounded by `ON-APPLICATION-HOOK-…`, `ON-SESSION-HOOK-…` or a `ON-REQUEST-HOOK-…` type of macro."
  (error "This function should be called inside a body, surrounded by ON-APPLICATION-HOOK-…, ON-SESSION-HOOK-… or a ON-REQUEST-HOOK-… type of macro."))


;; TODO: Add (declare...) processing for body
(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defun add-hook-helper (hook-storage hook-name callback-name args body error-message-on-empty-storage)
    (metatilities:with-gensyms (result-name)
      `(flet ((,callback-name (next-hooks ,@args)
                (declare (ignorable next-hooks))
                (let (next-hook-was-called)
                  (flet ((call-next-hook ()
                           (prog1 (eval-next-hooks next-hooks ,@args)
                             ;; Remember that we already called other
                             ;; callbacks
                             (setf next-hook-was-called t))))
                    ;; Call the hook's body
                    (let ((,result-name (progn ,@body)))
                      (cond
                        ;; If (call-next-hook) was called somewhere in the body,
                        ;; then returns body result, because it is a wrapper:
                        (next-hook-was-called ,result-name)
                        ;; otherwise, ,@body works as a :before method and we
                        ;; should return results of the next hook:
                        (t (call-next-hook))))))))

         (unless ,hook-storage
           (error ,error-message-on-empty-storage))
         (add-hook ,hook-storage ',hook-name ',callback-name
                   #',callback-name)))))


(defmacro add-session-hook (hook-name callback-name (&rest args) &body body)
  "Registers a new session hook like that:

\(add-session-hook :request
    update-dialog \(\)
  \(do-some-useful job\)\)
 "
  (add-hook-helper '*session-hooks*
                   hook-name
                   callback-name
                   args
                   body
                   "Unable to add a session hook because there is user session at the moment."))


(defmacro add-application-hook (hook-name callback-name (&rest args) &body body)
  (add-hook-helper '*application-hooks*
                   hook-name
                   callback-name
                   args
                   body
                   "Unable to add an application hook because application hook store is not ready yet."))


(defmacro add-request-hook (hook-name callback-name (&rest args) &body body)
  (add-hook-helper '*request-hooks*
                   hook-name
                   callback-name
                   args
                   body
                   "Request hook can be added only during request processing."))


(defmacro prepare-hooks (&body body)
  "Prepares internal special variables for request processing.

It takes hooks from user session and creates an empty hooks
list bound to a current request."
  `(let ((*request-hooks* (make-instance 'hooks))
         (*session-hooks* (get-or-create-session-hooks)))
     ,@body))


(defun get-callback-name (callback)
  (car callback))


(defun get-callback-value (callback)
  (cdr callback))


(defun get-callbacks (hooks name)
  "Internal function to get callbacks list from a hooks storage."
  (check-type hooks (or hooks null))
  (check-type name symbol)
  (when hooks
    (let* ((hash (slot-value hooks 'hooks))
           (callbacks (gethash name hash)))
      (mapcar #'get-callback-value callbacks))))


(defun get-callbacks-names (hooks name)
  "Internal function to get a list of callbacks names from a hooks storage."
  (check-type hooks (or hooks null))
  (check-type name symbol)
  (when hooks
    (let* ((hash (slot-value hooks 'hooks))
           (callbacks (gethash name hash)))
      (mapcar #'get-callback-name callbacks))))


(defun eval-hook-callbacks (hooks name args)
  "Internal function to eval hooks from all three hook storages."
  (check-type hooks hooks)
  (check-type name symbol)

  (let* ((hash (slot-value hooks 'hooks))
         (callbacks (gethash name hash)))
    (dolist (callback callbacks)
      (log:debug "Calling" callback)
      (apply (get-callback-value callback) args))))


;; external

(defun log-hooks (name)
  "A helper function to log all known hooks with given name."
  (mapc (f_ (log:debug "Application hook" _))
        (get-callbacks *application-hooks* name))
  (mapc (f_ (log:debug "Session hook" _))
        (get-callbacks *session-hooks* name))
  (mapc (f_ (log:debug "Request hook" _))
        (get-callbacks *request-hooks* name)))


(defmacro with-hook ((name &rest args) &rest body)
  "Performs nested calls of all the hooks of name, the innermost call is
   a closure over the body expression. All hooks take at least one
   argument, which is a list of inner hooks. After doing their job, they
   apply the first element of the list to the rest, but this machanics
   is hidden by add-(application|session|request)-hook macroses:

   (add-session-hook :action
      log-action (action)
      (log:info \"Before calling\" action)
      (call-next-hook)
      (log:info \"After calling\" action))

   Next, somewhere in the code, where action is called:

   (with-hook (:action action-object)
      ;; any custom code here
      (process action-object))

   Streamlined code execution will looks like:

   (defun action-handler (action-object)
      (log:info \"Before calling\" action-object)
      (process action-object)
      (log:info \"After calling\" action-object))"
  (metatilities:with-gensyms (null-list ignored-args hooks-chain)
    `(let ((,hooks-chain (append (get-callbacks *application-hooks* ',name)
                                 (get-callbacks *session-hooks* ',name)
                                 (get-callbacks *request-hooks* ',name))))
       (eval-next-hooks 
        (append ,hooks-chain
                (list (lambda (,null-list &rest ,ignored-args)
                        (declare (ignore ,ignored-args))
                        (check-type ,null-list null)
                        ,@body)))
        ,@args))))


(defun eval-next-hooks (next-hooks &rest args)
  "Helper function that makes it easier to write dynamic hooks.

It whould be called somewhere inside a hook, to evaluate inner
hooks. But you don't need to call it manually, just use
one of add-xxxx-hook and a (call-next-hook) inside of it."
  (let ((list (etypecase next-hooks 
                (symbol (symbol-value next-hooks))
                (list next-hooks))))

    ;; (loop with result = nil
    ;;       for hooks on list
    ;;       for current-hook = (car hooks)
    ;;       for rest-hooks = (cdr hooks)
    ;;       do (setf result
    ;;                (apply current-hook rest-hooks args))
    ;;       finally (return result))
    (unless (null list)
      (let ((current-hook (first list))
            (next-hooks (rest list)))
        (apply current-hook
               next-hooks
               args)))))



(defmacro defhook (name (&rest args) &optional docstring)
  "Registers a hook"
  (with-gensyms ()
    (let ((session-macro-name (ensure-symbol (symbolicate "ON-SESSION-HOOK-" name)
                                             :reblocks/hooks))
          (request-macro-name (ensure-symbol (symbolicate "ON-REQUEST-HOOK-" name)
                                             :reblocks/hooks))
          (application-macro-name (ensure-symbol (symbolicate "ON-APPLICATION-HOOK-" name)
                                     :reblocks/hooks))
          (with-macro-name (ensure-symbol (symbolicate "WITH-" name "-HOOK")
                                          :reblocks/hooks))
          (call-macro-name (ensure-symbol (symbolicate "CALL-" name "-HOOK")
                                          :reblocks/hooks)))
      ;; Here we need eval-when, because otherwice, exported functions
      ;; will not be available at load time
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name :docstring)
               ,docstring)
         (setf (get ',name :args)
               ',args)

         (let ((*package* (find-package "REBLOCKS/HOOKS")))
           (ignore-words-in-package ',name
                                    ',session-macro-name
                                    ',request-macro-name
                                    ',application-macro-name
                                    ',with-macro-name
                                    ',call-macro-name))
         
         (defmacro ,session-macro-name (callback-name ,args &body body)
           ,docstring
           (let ((callback-args (list ,@args))
                 (hook-name ',name))
             `(add-session-hook ,hook-name
                  ,callback-name
                  ,callback-args
                ,@body)))
         (defmacro ,request-macro-name (callback-name ,args &body body)
           ,docstring
           (let ((callback-args (list ,@args))
                 (hook-name ',name))
             `(add-request-hook ,hook-name
                  ,callback-name
                  ,callback-args
                ,@body)))
         (defmacro ,application-macro-name (callback-name ,args &body body)
           ,docstring
           (let ((callback-args (list ,@args))
                 (hook-name ',name))
             `(add-application-hook ,hook-name
                  ,callback-name
                  ,callback-args
                ,@body)))
         
         (defmacro ,with-macro-name ((,@args) &body body)
           ,docstring
           (let ((hook-name ',name)
                 (hook-args (list ,@args)))
             `(with-hook (,hook-name ,@hook-args)
                         ,@body)))
         
         (defmacro ,call-macro-name (,@args)
           ,docstring
           (let ((hook-name ',name)
                 (hook-args (list ,@args)))
             `(with-hook (,hook-name ,@hook-args))))
         
         (export (list ',session-macro-name
                       ',request-macro-name
                       ',application-macro-name
                       ',with-macro-name
                       ',call-macro-name)
                 :reblocks/hooks)))))


;; Weblocks core hooks

(defhook handle-http-request (env)
  "Called around code reponsible for an HTTP request processing,
   before any application was choosen.

   Accepts Lack's HTTP request environment.

   Should return a list of three items:

   * HTTP status code;
   * a plist of HTTP headers;
   * a list of strings corresponding to the page's content.")

(defhook start-weblocks ()
  "Called around code which starts all applications and a webserver.

   Returns a weblocks server object.")

(defhook stop-weblocks ()
  "Called around code which stops all applications and a webserver.

   Returns a weblocks server object.")

(defhook action (app action-name action-arguments)
  "Called when action is processed.

   Returns a result of REBLOCKS/ACTIONS:EVAL-ACTION generic-function application
   to the arguments.")

(defhook render (app)
  "Called when around whole page or ajax request processing.
   Should return HTML string.")

(defhook reset-session (session)
  "Called when session is resetted for some reason.")
