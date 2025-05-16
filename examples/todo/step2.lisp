(defpackage #:todo
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/routes
                #:page)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks/html
                #:with-html)
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get))
(in-package #:todo)


(defclass task ()
  ((id :initarg :id
       :initform (error "Field ID is required")
       :accessor id
       :type integer)
   (title :initarg :title
          :initform ""
          :accessor title)
   (description :initarg :description
                :initform ""
                :accessor description)
   (done :initarg :done
         :initform nil
         :accessor done)))


(defvar *store* (make-hash-table)
  "Dummy store for tasks: id -> task.")


(defvar *counter* 0
  "Simple counter for the hash table store.")


(defun make-task (title &key done)
  "Create a task and store it by its id."
  (let* ((id (incf *counter*))
         (task (make-instance 'task :title title :done done :id id)))
    (setf (gethash id *store*) task)
    task))


(defun get-task (id)
  (gethash id *store*))


;;;;;; Task list item

(defwidget list-item ()
  ((task :initarg :task
         :type task
         :reader task)))


(defun make-list-item (task)
  (make-instance 'list-item
                 :task task))


(defun toggle (list-item)
  (let ((task (task list-item)))
    (setf (done task)
          (if (done task)
              nil
              t))
    (update list-item)))


(defmethod reblocks/widget:render ((list-item list-item))
  (let ((task (task list-item)))
    (with-html ()
      (:p (:input :type "checkbox"
                  :checked (done task)
                  :onclick (make-js-action
                            (lambda (&key &allow-other-keys)
                              (toggle list-item))))
          (if (done task)
              (:s (title task))
              (title task))))))


;;;;;; Index page

(defwidget task-list ()
  ((items :initarg :items
          :type (soft-list-of list-item)
          :accessor list-items)))


(defun make-task-list (&rest task-titles)
  (let ((items
          (loop for title in task-titles
                for task = (make-task title)
                collect (make-list-item task))))
    (make-instance 'task-list
                   :items items)))


(defun add-task (task-list  title)
  (serapeum:push-end (make-list-item (make-task title))
                     (list-items task-list))
  (update task-list))


;; Alternative version for sending only a new item's HTML to the frontend
;; (defun add-task (task-list  title)
;;   (let ((last-item (alexandria:last-elt
;;                     (list-items task-list)))
;;         (new-item (make-list-item (make-task title))))
;;     (serapeum:push-end new-item
;;                        (list-items task-list))

;;     ;; This time we are calling update on a new list item:
;;     (update new-item
;;             ;; And providing to the frontend
;;             ;; a hint that we've inserted this new-item
;;             ;; after the some other item:
;;             :inserted-after last-item)))


(defmethod reblocks/widget:render ((task-list task-list))
  (with-html ()
    (:h1 "Tasks")
    
    (loop for item in (list-items task-list) do
      (reblocks/widget:render item))

    ;; Form for adding a new task
    (flet ((on-submit (&key title &allow-other-keys)
             (add-task task-list title)))
      (:form :onsubmit (make-js-form-action #'on-submit)
             (:input :type "text"
                     :name "title"
                     :placeholder "Task's title")
             (:input :type "submit"
                     :class "button"
                     :value "Add")))))


;;;;;; Application

(defapp tasks
  :prefix "/"
  :routes ((page ("/" :name "tasks-list")
             (make-task-list "First"
                             "Second"
                             "Third"))))



(defun start (&key (port 8080))
  (reblocks/server:start :port port
                         :apps '(tasks)))
