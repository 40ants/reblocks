(defpackage #:reblocks-tests/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:ensure-symbol
                #:symbolicate)
  (:import-from #:lack.test
                #:generate-env)
  (:import-from #:lack.request
                #:make-request)
  (:import-from #:cl-ppcre
                #:all-matches)
  (:import-from #:rove
                #:ok)
  (:import-from #:reblocks/session
                #:*session*)
  (:import-from #:reblocks/app
                #:*current-app*
                #:defapp)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/hooks
                #:prepare-hooks)
  ;; Just to point to dependencies
  (:import-from #:reblocks/request)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains)
  
  (:export #:with-request
           #:with-test-session
           #:is-html
           #:catch-hooks
           #:assert-hooks-called))
(in-package #:reblocks-tests/utils)


(defmacro with-test-session (() &body body)
  `(let ((*session* (make-hash-table :test 'equal)))
     ,@body))


(defapp empty-app
  :prefix "/"
  :autostart nil)


(defvar *current-env*)

(defmacro with-request ((uri &key
                               data
                               (method :get)
                               headers  ; it should be an alist
                               (app 'empty-app))
                        &body body)
  "Argument 'data' should be an alist with POST parameters if method is :POST."

  ;; Lack stores headers in a dict with lowercased keys,
  ;; that is why to simulate it, we need to ensure that
  ;; keys are lowercased.
  (let ((lowercased-headers
          (loop for (key . value) in headers
                collect (cons (string-downcase key)
                              value))))
    `(prepare-hooks
       (let* ((*current-env* (generate-env
                              ,uri
                              :method ,method
                              :content ,data
                              :headers ',lowercased-headers))
              ;; we need to setup a current webapp, because
              ;; uri tokenizer needs to know app's uri prefix
              ;; (*routes* (reblocks/routes::make-routes))
              (*current-app* (make-instance ',app)))
         (reblocks/request:with-request
             ((make-request *current-env*))
           (reblocks/response::with-response
               ()
             ,@body
             ;; Returned value will be set as
             ;; the body of the *response* object.
             ;; But in tests we have no body to return.
             (values))))
       ;; This way we make Rove test will not return response
       ;; structure as a result of test running:
       (values))))


(defmacro is-html (form expected &optional message)
  `(let* (;; (reblocks/html:*pretty-html* nil)
          (result (with-html-string (:pretty nil)
                    ,form)))
     (ok (all-matches ,expected result)
         ;;(string= result ,expected)
         ,message)))


(defmacro catch-hooks ((&rest hook-names) &body body)
  "Catches all hook calls and makes available function assert-hooks-contains
during the body execution. Hook names should be a list of keywords.

Call assert-hooks-contains inside the body, to check if proper hooks were called:

\(handle-hooks \(:fact-created :fact-removed\)
   \(do-something-to-add-a-fact\)
   \(assert-hooks-called
      \(contains :fact-created a-contact a-twitter-name\)
      \(contains :fact-removed a-contact a-twitter-name\)\)\)"
  (with-gensyms (hook-calls)
    (let ((hook-handlers
            (loop for hook-name in hook-names
                  collect `(,(ensure-symbol (symbolicate 'on-session-hook- hook-name)
                                            :reblocks/hooks)
                            ,(symbolicate 'handle- hook-name)
                               (&rest args)
                             (push (cons ,hook-name args)
                                   ,hook-calls))
                  )))
      `(prepare-hooks
         (let* ((,hook-calls nil)) 
           ,@hook-handlers

           (macrolet ((assert-hooks-called (&body fact-matchers)
                        "Returns a matcher which checks if all called hooks are match to given matchers.
                         Example:

                             \(assert-hooks-called
                                  (:fact-created contact email\)\)"
                        `(assert-that 
                          (reverse ,',hook-calls)
                          (contains
                           ,@(mapcar (lambda (matcher)
                                       (cons 'contains matcher))
                                     fact-matchers)))))
             ,@body))))))
