(defpackage #:reblocks/commands
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:parenscript)
  (:export #:add-command
           #:get-collected-commands
           #:with-collected-commands
           #:add-commands))
(in-package #:reblocks/commands)


(defvar *commands*)

(setf (documentation '*commands* 'variable)
      "A list of commands to execute on a client as a result of action call.

       These commands are collected during action processing and rendered to resulting JSON
       as some sort of JSON-rpc calls to be esecuted on a client-side.")


(defun cl-symbol-to-js-symbol (symbol)
  "A little helper to transform symbols like :foo-baz-bar into keywords :|fooBazBar|."
  (alexandria:make-keyword
   (parenscript:symbol-to-js-string symbol)))


(defun create-command (name &rest args)
  "Prepares command for rendering into JSON-rpc style dict."
  (check-type name symbol)
  (list :|jsonrpc| "2.0"
        :|method| (cl-symbol-to-js-symbol name)
        :|params| (loop for (key value) on args by #'cddr
                        append (list (cl-symbol-to-js-symbol key)
                                     value))))


(defun add-command (name &rest args)
  "Pushes a new command into the stack.

   After action processing these commands will be sent for execution on the client."
  (push
   (apply #'create-command name args)
   *commands*))


(defun add-commands (commands)
  "Pushes all commands from the list into the stack.

   After action processing these commands will be sent for execution on the client."
  (loop for command in commands
        do (push command *commands*)))


(defun get-collected-commands ()
  "Returns all commands created during AJAX request processing in order as they where added."
  ;; Because we are pushing command into the list,
  ;; we need to reverse it now.
  (reverse *commands*))



(defmacro with-collected-commands (() &body body)
  "Collects commands added using a call to ADD-COMMANDS during the body execution.

   Commands list can be aquired using GET-COLLECTED-COMMANDS function."
  `(let (*commands*)
     ,@body))
