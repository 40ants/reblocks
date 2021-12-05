(uiop:define-package #:reblocks/response
  (:use #:cl)
  (:import-from #:reblocks/request
                #:get-uri
                #:ajax-request-p
                #:get-header)
  (:import-from #:reblocks/js/base
                #:with-javascript-to-string
                #:with-javascript)
  (:import-from #:reblocks/actions
                #:on-missing-action)
  (:import-from #:reblocks/app
                #:get-prefix)
  (:import-from #:reblocks/commands
                #:add-command)
  (:import-from #:quri)
  (:import-from #:alexandria
                #:assoc-value)
  (:export #:immediate-response
           #:make-response
           #:add-header
           #:send-script
           #:make-uri
           #:redirect
           #:get-response
           #:get-content
           #:get-code
           #:get-headers
           #:get-custom-headers
           #:get-content-type
           #:add-retpath-to
           #:response))
(in-package reblocks/response)


(defvar *custom-headers* nil
  "Additional HTTP headers to return in response to request.

   Use (add-header ...) to add one header.")


(defun get-default-content-type-for-response ()
  (if (ajax-request-p)
      "application/json"
      "text/html"))

(defclass response ()
  ((content :type string
            :initarg :content
            :initform ""
            :reader get-content
            :documentation "A string with a content of the response.")
   (code :type integer
         :initarg :code
         :initform 200
         :reader get-code
         :documentation "HTTP status code to return in response to request.

                         By default, this slot will be set to 200.")
   (custom-headers :type (or null list)
                   :initarg :custom-headers
                   :initform nil
                   :reader get-custom-headers
                   :documentation "Custom HTTP headers of request.

                         By default, this slot will be set to 200.")
   (content-type :type string
                 :initarg :content-type
                 :initform (get-default-content-type-for-response)
                 :reader get-content-type
                 :documentation "HTTP content type to return in response to request.

                                 By default, have text/html value for usual requests
                                 and application/json for AJAX requests.")))


(defun get-headers (response)
  (check-type response response)
  (append (list :content-type (get-content-type response))
          (get-custom-headers response)))


(defgeneric get-response (obj)
  (:documentation "Extracts response from the object. Usually, obj will be an [IMMEDIATE-RESPONSE][condition] condition."))


(define-condition immediate-response ()
  ((response :type response
             :initarg :response
             :reader get-response)))


(define-condition redirect (immediate-response)
  ())


(defun make-response (content &key
                                (code 200)
                                (content-type (get-default-content-type-for-response))
                                (headers *custom-headers*))
  (make-instance 'response
                 :content content
                 :code code
                 :content-type content-type
                 :custom-headers headers))


(defun add-header (name value)
  "Use this function to add a HTTP header:

   ```lisp
   (add-header :x-request-id 100500)
   ```"

  (declare (type symbol name)
           (type string value))
  (push value *custom-headers*)
  (push name *custom-headers*))


(defun make-uri (new-path)
  "Makes a new URL, based on the current request's URL.

   If new-path can be absolute, like /logout or relative,
   like ./stories.

   Also, it can contain a query params like /login?code=100500"
  (let* ((base (get-uri))
         (parsed-base (quri:uri base))
         (parsed-new-path (quri:uri new-path))
         (new-url (quri:merge-uris parsed-new-path
                                   parsed-base)))
    (quri:render-uri new-url)))


(defun add-retpath-to (uri &key (retpath (reblocks/request:get-uri)))
  "Adds a \"retpath\" GET parameter to the giving URL.

   Keeps all other parameters and overwrites \"retpath\" parameter if it is
   already exists in the URL.

   By default, retpath is the current page, rendered by the weblocks.
   This is very useful to redirect user to login page and return him to the
   same page where he has been before."
  (let* ((parsed-base (quri:uri uri))
         (query (quri:uri-query parsed-base))
         (parsed-query (when query
                         (quri:url-decode-params query)))
         (_ (setf (assoc-value parsed-query
                                          "retpath"
                                          :test 'string-equal)
                  retpath))
         (new-query (quri:url-encode-params parsed-query))
         (parsed-new-path (quri:uri (concatenate 'string "?"
                                                 new-query)))
         (new-url (quri:merge-uris parsed-new-path
                                   parsed-base)))
    (declare (ignorable _))
    (quri:render-uri new-url)))


(defun immediate-response (content &key
                                     (content-type (get-default-content-type-for-response))
                                     (code 200)
                                     (headers *custom-headers*)
                                     (condition-class 'immediate-response))
  "Aborts request processing by signaling an [IMMEDIATE-RESPONSE][condition]
   and returns a given value as response.

   HTTP code and headers are taken from CODE and CONTENT-TYPE."

  ;; This abort could be a normal, like 302 redirect,
  ;; that is why we are just informing here
  (log:info "Aborting request processing"
            code
            content-type
            headers)

  (error condition-class
         :response (make-response 
                    content
                    :code code
                    :content-type content-type
                    :headers headers)))


(defun send-script (script &optional (place :after-load))
  "Send JavaScript to the browser. The way of sending depends
  on whether the current request is via AJAX or not.

  Script may be either a string or a list; if it is a list
  it will be compiled through Parenscript first."
  (declare (ignorable place))
  (let ((script (etypecase script
                  (string script)
                  (list (ps:ps* script)))))
    (if (ajax-request-p)
        (let ((code (if (equalp (get-header "X-Weblocks-Client")
                                "JQuery")
                        script
                        (with-javascript-to-string script))))
          (add-command :execute-code
                       :code code))
        (with-javascript
          script))))


(defun redirect (uri)
  "Redirects the client to a new URI."
  (if (ajax-request-p)
      (add-command :redirect
                   :to uri)
      (immediate-response ""
                          :condition-class 'redirect
                          :headers (list :location uri)
                          :code 302)))


(defmethod on-missing-action (app action-name)
  (declare (ignorable app action-name))
  (redirect
   (make-uri (get-prefix app))))


