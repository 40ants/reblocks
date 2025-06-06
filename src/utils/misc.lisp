(uiop:define-package #:reblocks/utils/misc
  (:use #:cl)
  (:import-from #:salza2
                #:gzip-stream)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-sequence)
  (:import-from #:babel
                #:string-to-octets)
  (:import-from #:anaphora
                #:it
                #:aand)
  (:import-from #:closer-mop
                #:funcallable-standard-object
                #:required-args)
  (:import-from #:serapeum
                #:->)
  (:export #:safe-apply
           #:safe-funcall
           #:public-file-relative-path
           #:public-files-relative-paths
           #:symbol-status
           #:hash-keys
           #:append-custom-fields
           #:function-designator-p
           #:find-own-symbol 
           #:relative-path 
           #:read-from-file 
           #:write-to-file
           #:slurp-file
           #:with-file-write
           #:merge-files-with-newline
           #:gzip-file
           #:md5
           #:concatenate-keywords))
(in-package #:reblocks/utils/misc)


(defun safe-apply (fn &rest args)
  "Apply 'fn' if it isn't nil. Otherwise return nil."
  (when fn
    (apply #'apply fn args)))

(defun safe-funcall (fn &rest args)
  "Funcall 'fn' if it isn't nil. Otherwise return nil."
  (when fn
    (apply #'funcall fn args)))


(defun public-file-relative-path (type filename)
  "Infer FILENAME's relative path and extension from TYPE.

Example:

\(public-file-relative-path :stylesheet \"navigation\")
=> \"stylesheets/navigation.css\""
  (multiple-value-bind (folder ext) (ecase type
                                      (:stylesheet (values "stylesheets" "css"))
                                      (:script (values "scripts" "js")))
    (concatenate 'string folder "/" filename "." ext)))

(defun public-files-relative-paths (&rest args)
  "A helper function that returns a list of paths for files provided
in 'args'. Each argument must be a cons cell where car is
either :stylesheet or :script and cdr is a name of the file.

Useful when generating a list of dependencies for widgets and/or the
application (see the 'dependencies' generic function and
*application-dependencies*.)

Ex:
\(get-public-files-paths '(:stylesheet . \"navigation\")
                         '(:script . \"effects\"))
=> (#P\"stylesheets/navigation.css\" #P\"scripts/effects.js\")"
  (loop for i in args
     collect (public-file-relative-path (car i) (cdr i))))

;;; Status of a symbol
(defun symbol-status (symbol)
  "Returns a status of 'symbol' in its package (internal, external,
etc.)"
  (nth-value 1 (find-symbol (symbol-name symbol)
                            (symbol-package symbol))))

(defvar *asdf-system-cache* (make-hash-table :test #'equalp))


(defun hash-keys (hashtable)
  "Returns all keys in the hashtable."
  (loop for key being the hash-keys in hashtable
        collect key))

(defun append-custom-fields (custom-fields args)
  "Appends 'custom-fields' to the end of custom fields that are
already defined in 'args'."
  (append (cadr (member :custom-fields args))
          custom-fields))

(defun function-designator-p (obj)
  "Returns true if the object is a function designator."
  (or (functionp obj)
      (and (symbolp obj)
           (not (null (fboundp obj))))
      (typep obj 'funcallable-standard-object)))

(defun find-own-symbol (name &optional (package nil packagep))
  "Like `find-symbol', but reject symbols not really in PACKAGE."
  (multiple-value-bind (sym status)
      (if packagep (find-symbol name package) (find-symbol name))
    (and (member status '(:internal :external))
         (values sym status))))

(defun congruent-lambda-expression (lambda-list function)
  "Answer a lambda expression with LAMBDA-LIST that passes all
args (assuming the call is allowed by LAMBDA-LIST) to FUNCTION,
answering its result."
  (let* ((reqs (required-args lambda-list))
         (opts (aand (member '&optional lambda-list)
                     (loop for llelt in (cdr it)
                           until (member llelt lambda-list-keywords)
                           if (consp llelt)
                             collect (list (first llelt) (second llelt)
                                           (or (third llelt) (gensym "OPTP")))
                           else collect (list llelt nil (gensym "OPTP")))))
         (keys? (member '&key lambda-list))
         (more (and (or keys? (position-if (lambda (item)
                                             (member item '(&body &rest)))
                                           lambda-list))
                    (gensym "MORE"))))
    `(lambda (,@reqs
              ,@(and opts (cons '&optional opts))
              ,@(and more (list '&rest more))
              ,@(and keys? '(&key &allow-other-keys)))
       ,(if (or opts more)
            `(apply ',function ,@reqs
                    ,(reduce (lambda (optarg rest)
                               `(and ,(third optarg)
                                     (cons ,(first optarg) ,rest)))
                             opts :from-end t :initial-value more))
            `(funcall ',function ,@reqs)))))


(defmacro with-file-write ((stream-name path &key (element-type ''base-char))
                           &body body)
  "Ensures that directories exists, then opens a file for write and executes a body."
  `(progn
     (ensure-directories-exist ,path)
     (with-open-file (,stream-name ,path :direction :output
                                         :element-type ,element-type
                                         :if-exists #+ccl :overwrite #-ccl :supersede
                                         :if-does-not-exist :create)
       ,@body)))

(defun write-to-file (object path)
  (with-file-write (stream path)
    (write object :stream stream)))

(defun read-from-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (eval (read stream nil nil))))

(defun slurp-file (filepath &key (element-type 'base-char))
  (with-open-file (stream filepath :element-type element-type)
    (let ((seq (make-array (file-length stream) :element-type element-type)))
      (read-sequence seq stream)
      seq)))

(defun merge-files (file-list saved-path
                    &key (element-type '(unsigned-byte 8)) linkage-element-fn)
  (with-file-write (stream saved-path :element-type element-type)
    (write-sequence (slurp-file (car file-list) :element-type element-type)
                    stream)
    (dolist (file (cdr file-list))
      (when linkage-element-fn
        (funcall linkage-element-fn stream))
      (write-sequence (slurp-file file :element-type element-type)
                      stream))))

(defun merge-files-with-newline (file-list saved-path)
  (merge-files file-list saved-path
               :linkage-element-fn (lambda (stream) (write-byte 10 stream))))


(-> relative-path (pathname pathname)
    (values pathname &optional))

(defun relative-path (full-path prefix-path)
  (make-pathname :directory (cons :relative
                                  (nthcdr (length (pathname-directory prefix-path))
                                          (pathname-directory full-path)))
                 :name (pathname-name full-path)
                 :type (pathname-type full-path)))


(defun gzip-file (input output &key (if-exists #+ccl :overwrite #-ccl :supersede) (if-does-not-exist :create)
                                 (minimum-length 300))
  "Redefined salsa2:gzip-file with more keywords."
  (with-open-file (istream input :element-type '(unsigned-byte 8))
    (unless (< (file-length istream) minimum-length)
      (with-open-file (ostream output
                               :element-type '(unsigned-byte 8)
                               :direction :output
                               :if-does-not-exist if-does-not-exist
                               :if-exists if-exists)
        (gzip-stream istream ostream)))
    (probe-file output)))

(defun md5 (string)
  (byte-array-to-hex-string
    (digest-sequence
      :md5 (string-to-octets string :encoding :utf-8))))

(defun concatenate-keywords (&rest symbols)
  (intern 
    (apply #'concatenate (list* 'string (mapcar #'string-upcase symbols)))
    "KEYWORD"))
