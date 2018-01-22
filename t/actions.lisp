(defpackage #:weblocks-test/actions
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:weblocks-test/utils)
  (:import-from #:weblocks/actions
                #:function-or-action->action
                #:eval-action
                #:on-missing-action
                #:make-action)
  (:import-from #:weblocks/request
                #:get-action-name-from-request))
(in-package weblocks-test/actions)


(deftest get-action-name-from-request-test
  (testing "Checking if get-action-name-from-request works with GET and POST"
    (with-session
      (with-request ("/?action=blah" :method :get)
        (testing "It should work with GET parameters"
          (ok (equal (get-action-name-from-request)
                     "blah"))))
    
      (with-request ("/" :method :post
                         :data '(("action" . "blah")))
        (testing "And with POSTs"
          (ok (equal (get-action-name-from-request)
                     "blah")))))))


(deftest action-evaluation
  (testing "Function eval-action should return action function's result"
    (with-session
      (let ((action-name (make-action (lambda (&rest keys)
                                        (declare (ignore keys))
                                        123))))
        (testing "This action just returns 123 when evaluated."
          (ok (eql (eval-action nil action-name nil)
                   123)))))))

(deftest missing-action
    (testing "eval-action should call weblocks/actions:on-missing-action if action is not found"
      (with-session
        (defclass someapp ()
          ())
        (let ((app (make-instance 'someapp))
              result)
          (defmethod on-missing-action ((app someapp) action-name)
            (setf result (format nil "Action \"~a\" is missing." action-name)))

          (eval-action app "missing-action" nil)
          (ok (equal result
                     "Action \"missing-action\" is missing.")
              "Result should be changed as a side-effect of method call.")))))


(deftest function-or-action->action-signals-when-action-is-not-defined
  (with-session
    (with-request ("/")
      (ok (signals (function-or-action->action "abc123"))
          "Action with name \"abc123\" wasn't defined and function should raise an exception."))))


(deftest function-or-action->action-success
  (with-session
    (make-action #'identity "abc123")
    
    (ok (equal (function-or-action->action "abc123")
               "abc123")
        "When action is defined function-or-action->action should return it's name")

    (ok (equal (function-or-action->action #'identity)
               "abc123")
        "This also should work if a function was given as an argument")))
