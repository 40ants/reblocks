(defpackage #:reblocks-tests/actions
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:reblocks-tests/utils)
  (:import-from #:reblocks/actions
                #:generate-action-code
                #:make-js-action
                #:make-action
                #:eval-action
                #:on-missing-action
                #:make-action-url
                #:internal-make-action)
  (:import-from #:reblocks/request
                #:get-action-name-from-request)
  (:import-from #:reblocks/request-handler
                #:handle-action-if-needed)
  (:import-from #:reblocks/page
                #:initialize-session-pages
                #:with-page-defaults)
  (:import-from #:reblocks/variables
                #:*current-app*)
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:cl-mock
                #:with-mocks
                #:answer)
  (:import-from #:40ants-routes/with-url
                #:with-partially-matched-url))
(in-package #:reblocks-tests/actions)


(reblocks/app:defapp test-app
  :autostart nil)


(deftest get-action-name-from-request-test
  (testing "Checking if get-action-name-from-request works with GET and POST"
    (with-test-session ()
        (with-request ("/?action=blah" :method :get)
          (testing "It should work with GET parameters"
            (ok (equal (get-action-name-from-request)
                       "blah"))))
    
      (with-request ("/" :method :post
                         :data '(("action" . "blah")))
        (testing "And with POSTs"
          (ok (equal (get-action-name-from-request)
                     "blah")))))))


(defmacro with-test-app (() &body body)
  `(reblocks/app:with-app ((make-instance 'test-app))
    (with-test-session ()
        (initialize-session-pages)
      ,@body)))


(defmacro with-test-request ((uri &key
                                  data
                                  (method :get)
                                  headers
                                  (new-page t))
                             &body body)
  (let ((content (if new-page
                     `(with-page-defaults ()
                        ,@body)
                     `(progn ,@body))))
    ;; `(let ((uri (etypecase ,uri
    ;;               (cons (eval ,uri))
    ;;               (string ,uri)))))
    `(with-request (,uri :method ,method :data ,data :headers ,headers)
       (with-partially-matched-url ((40ants-routes/routes:routes ("empty-routes")
                                      (reblocks/routes::page ("/")
                                        ;; Handler is empty
                                        )
                                      (reblocks/routes::page ("/foo/bar")
                                        ;; Handler is empty
                                        ))
                                    ,uri)
         ,content))))


(deftest action-evaluation
  (testing "Function eval-action should return action function's result"
    (with-test-app ()
      (with-test-request ("/foo/bar")
        (let ((action-name
                (internal-make-action (lambda (&rest keys)
                                        (declare (ignore keys))
                                        123))))
          (testing "This action just returns 123 when evaluated."
            (ok (eql
                 (eval-action reblocks/variables:*current-app*
                              action-name nil)
                 123))))))))


(deftest eval-action-with-arguments
  (with-test-app ()
    (let* (action-result action-name)
      (with-test-request ("/")
        (setf action-name
              (internal-make-action (lambda (&rest args)
                                      (setf action-result
                                            args)))))
      (with-test-request ((fmt "/?name=Bob&cancel=Cancel&~A=~A"
                               reblocks/variables:*action-string*
                               action-name)
                          :method :get
                          :headers (("X-Requested-With" . "XMLHttpRequest"))
                          :new-page nil)
        (handle-action-if-needed *current-app*))

      (assert-that action-result
                   (contains :name "Bob"
                             :cancel "Cancel")))))


(defclass someapp ()
  ())


(deftest missing-action
  (testing "eval-action should call reblocks/actions:on-missing-action if action is not found"
    (with-test-session ()
      
      (let ((app (make-instance 'someapp))
            result)
        (defmethod on-missing-action ((app someapp) action-name)
          (setf result (format nil "Action \"~a\" is missing." action-name)))

        (eval-action app "missing-action" nil)
        (ok (equal result
                   "Action \"missing-action\" is missing.")
            "Result should be changed as a side-effect of method call.")))))


(deftest make-action-signals-when-action-is-not-defined
  (with-test-session ()
      (with-request ("/")
        (ok (signals (make-action "abc123"))
            "Action with name \"abc123\" wasn't defined and function should raise an exception."))))


(deftest make-action-success
  (with-test-app ()
    (with-test-request ("/")
      (internal-make-action #'identity "abc123")
     
      (ok (equal (make-action "abc123")
                 "abc123")
          "When action is defined make-action should return it's name")

      (ok (equal (make-action #'identity)
                 "abc123")
          "This also should work if a function was given as an argument"))))


(defun test-action ()
  )

(deftest make-action-url-test
  (with-test-session ()
      (initialize-session-pages)
      (with-test-request ("/foo/bar" :method :get)
        (with-page-defaults ()
          (internal-make-action 'test-action "test-action")
      
          (ok (equal (make-action-url "test-action")
                     "/foo/bar?action=test-action"))))))


(deftest make-js-action-test ()
  (with-test-app ()
    (with-test-request ("/")
      (with-mocks ()
        (answer generate-action-code "action:code")
        
        (ok (equal (make-js-action 'identity)
                   "return initiateAction(\"action:code\")"))

        (ok (equal (make-js-action 'identity
                                   :args (dict
                                          "foo" 100500))
                   "return initiateAction(\"action:code\", {\"args\":{\"foo\":100500}})"))))))
