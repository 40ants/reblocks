(defpackage #:reblocks/commands/history
  (:use #:cl)
  (:import-from #:reblocks/actions
                #:make-action)
  (:import-from #:reblocks/commands
                #:add-command)
  (:export #:browser-history-push-state
           #:browser-history-back
           #:browser-history-forward))

(in-package :reblocks/commands/history)

(defun browser-history-push-state (url &key (state "{}")
                                         pop-action)
  "Invoke history.pushState on the client.

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (let ((action-code (when pop-action
                       (etypecase pop-action
                         (string pop-action)
                         (function (reblocks/actions:make-action pop-action))))))
    (add-command :update-history
                 :operation "pushState"
                 :state state
                 :url url
                 :pop-action-code action-code)))

(defun browser-history-forward ()
  "Invoke history.forward() on the client.

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (add-command :update-history
               :operation "forward"))

(defun browser-history-back ()
  "Invoke history.back() on the client.

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (add-command :update-history
               :operation "back"))
