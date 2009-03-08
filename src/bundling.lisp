(in-package :weblocks)

(export '(*initial-bundle-id*))

(defclass bundle-tally ()
  ((last-bundle-id :accessor last-bundle-id :initarg :last-bundle-id
	    :documentation "The id of the last created bundle file.")
   (composition-list :accessor composition-list :initarg :composition-list
		     :documentation "A list of lists. Each list contains a bundle id and a list of files that the bundle is composed of.")
   (modified-p :accessor modified-p :initform nil
	       :documentation "Indicate if the tally has been modified.")
   (bundle-folder :accessor bundle-folder :initarg :bundle-folder
		  :documentation "Stores the path of the bundle folder for easier access.")
   (tally-path :reader tally-path :initarg :tally-path
	       :documentation "Stores the path of the tally file for easier access."))
  (:documentation "A record of the locations and composition files of all bundled files."))

(defvar *initial-bundle-id* 1)

(defun get-bundle-tally ()
  "Copy the tally file into a bundle-tally object"
  (let* ((bundle-folder (merge-pathnames "bundles/" (compute-webapp-public-files-path (current-webapp))))
	 (tally-path (merge-pathnames "tally" bundle-folder))
	 (file-data (when (cl-fad:file-exists-p tally-path)
		      (read-from-file tally-path)))
	 (last-bundle-id (if file-data (car file-data) (1- *initial-bundle-id*)))
	 (composition-list (cdr file-data)))
    (make-instance 'bundle-tally :last-bundle-id last-bundle-id :composition-list composition-list
		   :bundle-folder bundle-folder :tally-path tally-path)))


(defun store-bundle-tally (tally)
  "Save the bundle-tally object into the tally file"
  (when (modified-p tally)
    (write-to-file `',(cons (last-bundle-id tally) (composition-list tally))
		   (tally-path tally))))

(defun add-to-tally (bundle-name file-list tally)
  (with-slots (composition-list modified-p) tally
    (push (cons bundle-name file-list) composition-list)			      
    (setf modified-p t)))


(defun remove-from-tally (bundle-name tally)
  (with-slots (composition-list modified-p) tally
    (setf composition-list (remove-if #'(lambda (x) (string-equal (car x) bundle-name)) composition-list))
    (setf modified-p t)))

(defun create-bundle-file (file-list type tally)
  (let ((bundle-name (format nil "~A.~A"
			     (incf (last-bundle-id tally))
			     (ecase type
			       (stylesheet-dependency "css")
			       (script-dependency "js")))))
    (add-to-tally bundle-name file-list tally)
    (merge-files file-list (merge-pathnames bundle-name (bundle-folder tally)))
    bundle-name))

(defun delete-bundle-file (bundle-name tally)
  (remove-from-tally bundle-name tally)
  (delete-file (merge-pathnames bundle-name (bundle-folder tally))))


(defun find-bundle (file-list tally)
  "If the same files have already been bundled, return the bundle-name"
  (car (find-if #'(lambda (x) (equalp (cdr x) file-list))
		(composition-list tally))))

(defvar *bundle-dependencies-lock* (bordeaux-threads:make-lock))

(defun build-bundle (file-list type &key media)
  (bordeaux-threads:with-lock-held (*bundle-dependencies-lock*)
    (let* ((tally (get-bundle-tally))
	   (bundle-name (find-bundle file-list tally)))
      (when (null bundle-name)		
	(setf bundle-name (create-bundle-file file-list type tally)))
      (store-bundle-tally tally)
      ;; make new dependency object for the bundle file
      (let ((physical-path (merge-pathnames bundle-name (bundle-folder tally)))
	    (virtual-path (merge-pathnames (format nil "bundles/~A" bundle-name)
					   (maybe-add-trailing-slash (compute-webapp-public-files-uri-prefix (current-webapp))))))
	(ecase type
	  (stylesheet-dependency
	   (make-instance 'stylesheet-dependency
			  :url virtual-path :media media
			  :local-path physical-path))
	  (script-dependency
	   (make-instance 'script-dependency :url virtual-path
			  :local-path physical-path)))))))


(defun bundle-some-dependencies (dependency-list dependency-type)
  (let (exceptions)
    (when (listp dependency-type)
      (setf exceptions (cdr dependency-type))
      (setf dependency-type (car dependency-type)))
    (loop
       for dep in dependency-list
       for path = (local-path dep)
       if (and (typep dep dependency-type)
	       (null (find path exceptions :test #'string-equal)))
          if (cl-ppcre:scan "(?i)-import\.css$" path)
             collect path into imports
          else
	     collect path into main
	  end
       else
          collect dep into others
       finally
	 (return (push (build-bundle (append imports main) dependency-type)
		       others)))))