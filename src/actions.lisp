(uiop:define-package #:reblocks/actions
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:reblocks/utils/misc
                #:safe-apply)
  (:import-from #:reblocks/variables
                #:*action-string*
                #:*ignore-missing-actions*
                #:*current-app*)
  (:import-from #:lack.util
                #:generate-random-id)
  ;; Just dependencies
  (:import-from #:reblocks/session)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:quri)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  
  (:export #:eval-action
           #:on-missing-action
           #:make-action
           #:make-js-action
           #:make-js-form-action
           #:make-action-url))
(in-package reblocks/actions)

(in-readtable pythonic-string-syntax)


(defgeneric on-missing-action (app action-name)
  (:documentation "Must be overridden by application to prevent default
behaviour - redirect to a root of the application.
The new method should determine the behavior in this
situation (e.g. redirect, signal an error, etc.)."))


(defgeneric eval-action (app action-name arguments)
  (:documentation   "
   Evaluates the action that came with the request.

   First it resolves action by trying to find ACTION-NAME inside session
   or current app's actions.
   If actions wasn't found, then EVAL-ACTION calls ON-MISSING-ACTION generic-function.
   Otherwise it applies arguments to the action callback."))


(defmethod eval-action (app action-name arguments)
  (let ((action (get-request-action app action-name)))

    (unless action
      (on-missing-action app action-name))
    
    (log:debug "Calling" action "with" arguments "and" action-name)
    (safe-apply action arguments)))


(defun generate-action-code ()
  "Generates unique, hard to guess action codes."
  (let ((new-action-id (gensym "")))
    (format nil "~A:~A"
            new-action-id
            (lack.util:generate-random-id))))


(defun internal-make-action (action-fn &optional (action-code (generate-action-code)))
  "Converts a function into an action that can be rendered into HTML. A
   unique, hard to guess string is generated for the function, and a
   function is added to the session hashtable under this string. The
   string is then returned. When later requests come in,
   'get-request-action' machinery determines if the action string that
   came with the request is stored in the hashtable, and if so, invokes
   the stored function.

   ACTION-FN - A function that will be called if the user initiates
   appropriate control (link, form, etc.) GET and POST parameters will be
   passed to this function as keyword arguments by the framework.

   ACTION-CODE - The code to use for an action (if not specified
   INTERNAL-MAKE-ACTION generates a unique value for each action). Note, if you
   don't provide a hard to guess code ('generate-action-code' is used by
   default), the user will be vulnerable to an attack where a malicious
   attacker can attempt to guess a dangerour action id and send the user
   a link to it. Only use guessable action codes for GET actions."

  ;; Here we put into the session two maps:
  ;; code->action which maps from string code to a function
  ;; and
  ;; action->code which maps backward from a function to a code.
  (let ((code->action 
          (reblocks/session:get-value 'code->action
                                      (make-hash-table :test #'equal))))

    (setf (gethash action-code code->action) action-fn))

  ;; Now, get or create a table for function->code mapping
  (let ((action->code
          (reblocks/session:get-value 'action->code
                                      (make-hash-table))))

    (setf (gethash action-fn action->code) action-code))
  
  action-code)


(defun make-action (function-or-action)
  "Accepts a function or an existing action. If the value is a
   function, adds it to the session actions and returns its unique code as a string.
   Otherwise, checks if the action already exists. If it does, returns the argument as is.
   If it does not, signals an error."
  (cond ((functionp function-or-action)
         ;; If it is a function, first we'll try to find
         ;; a code for it in the session.
         (multiple-value-bind (code code-p)
             (gethash function-or-action
                      (reblocks/session:get-value 'action->code
                                                  (make-hash-table)))
           (if code-p
               code
               (internal-make-action function-or-action))))

        ;; if it is an action code
        (t
         (multiple-value-bind (res presentp)
             (reblocks/app-actions:get-action *current-app* function-or-action)
           (declare (ignore res))
           (if presentp
               function-or-action
               (multiple-value-bind (res presentp)
                   (gethash function-or-action
                            (reblocks/session:get-value 'code->action
                                                        (make-hash-table)))
                 (declare (ignore res))
                 (if presentp
                     function-or-action
                     (error "The value '~A' is not an existing action." function-or-action))))))))


(defun make-action-url (function-or-action-code &key (keep-query-params t))
  """Accepts action code and returns a URL that can be used as `href` attribute of HTML link.

  For example:

  ```cl-transcript
  (let ((reblocks/request::*request*
          (lack.request:make-request
           '(:path-info "/blah/minor"))))
    (reblocks/actions:make-action-url "test-action"))
  => "/blah/minor?action=test-action"
  ```

  If KEEP-QUERY-PARAMS is true (default), then all query arguments are preserved
  and will be passed as arguments to the action.
  """
  (let* ((action-code (make-action function-or-action-code))
         (old-params (when keep-query-params
                       (reblocks/request:get-parameters)))
         (params (list* (cons *action-string*
                              action-code)
                        old-params)))
    (concatenate 'string
                 (get-path) ;; Current URL path
                 "?"
                 (quri:url-encode-params params))))


(defun make-js-action (action)
  "Returns JS code which can be inserted into `onclick` attribute and will
   execute given Lisp function on click.

   It accepts any function as input and produces a string with JavaScript code."
  (let* ((action-code (make-action action)))
    (format nil "initiateAction(\"~A\"); return false;"
            action-code)))


(defun make-js-form-action (action)
  "Returns JS code which can be inserted into `onsubmit` form's attribute.

   It accepts any function as input and produces a string with JavaScript code.

   On form submit given action will be executed and all input values
   will be passed as arguments."
  (let* ((action-code (make-action action)))
    (format nil "initiateFormAction(\"~A\", $(this)); return false;"
            action-code)))


(defun get-session-action (action-name)
  "Returns an action bound to the current session."
  (let ((code->action 
          (reblocks/session:get-value 'code->action)))
    (when code->action
      (gethash action-name code->action))))


(defun get-request-action (app action-name)
  "Gets an action from the request. If the request contains
   a parameter with name equal to *ACTION-STRING* variable, the action
   is looked up in the session and appropriate function is returned.
   If no action is in the parameter, returns NIL. If the action
   isn't in the session (somehow invalid), raises an assertion."
  (when action-name
    (let* ((app-wide-action (reblocks/app-actions:get-action app action-name))
           (session-action (unless app-wide-action
                             (get-session-action action-name)))
           (request-action (or app-wide-action session-action)))
      ;; TODO: rethink this form. May be throw a special condition instead of string
      (unless *ignore-missing-actions*
        (assert request-action (request-action)
                (concatenate 'string "Cannot find action: " action-name)))
      request-action)))
