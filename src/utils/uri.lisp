(defpackage #:reblocks/utils/uri
  (:use #:cl)
  (:import-from #:puri
                #:uri
                #:parse-uri
                #:uri-query
                #:render-uri)
  (:import-from #:quri
                #:url-encode-params
                #:url-decode-params)
  (:import-from #:cl-ppcre)
  (:export #:remove-parameter-from-uri))
(in-package #:reblocks/utils/uri)


;;; URI from pathname
(defmethod uri ((thing pathname))
  (puri:uri
   (format nil "~A~{~A/~}~A~A"
           (if (eql (car (pathname-directory thing)) :absolute) "/" "")
           (cdr (pathname-directory thing))
           (or (pathname-name thing) "")
           (if (pathname-type thing)
               (format nil ".~A" (pathname-type thing))
               ""))))

(defun remove-parameter-from-uri (uri parameter)
  "Removes the given parameter from a URI."
  (let* ((parsed-uri (parse-uri uri))
         (query (or (uri-query parsed-uri)
                    ""))
         (params (url-decode-params query))
         (final-params (remove-if (lambda (item)
                                    (let ((name (first item)))
                                      (equal name parameter)))
                                  params)))

    (setf (uri-query parsed-uri)
          (url-encode-params final-params))
    (render-uri parsed-uri nil)))

(defun remove-url-query-string (str)
  (cl-ppcre:regex-replace "\\?.*" str ""))
