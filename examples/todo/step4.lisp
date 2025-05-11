(uiop:define-package #:todo
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-form-action
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
  (let* ((task (task list-item))
         ;; Here is how URL reversing works:
         (details-url
           (route-url "task-details"
                                              :task-id (id task))))
    (with-html ()
      (:p (:input :type "checkbox"
                  :checked (done task)
                  :onclick (make-js-action
                            (lambda (&key &allow-other-keys)
                              (toggle list-item))))
          (:a :href details-url
              (if (done task)
                  (:s (title task))
                  (title task)))))))


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
  (push (make-list-item (make-task title))
        (list-items task-list))
  (update task-list))


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


;;;;;; Task page

(defwidget task-page ()
  ((task :initarg :task
         :type task
         :accessor task)
   (edit-mode-p :initform nil
                :type boolean
                :accessor edit-mode-p)))


(defun make-task-page (task-id)
  (let ((task (get-task task-id)))
    (cond
      (task (make-instance 'task-page
                           :task task))
      (t
       (reblocks/response:not-found-error
        (format nil "Task with id ~A not found."
                task-id))))))


(defmethod render ((task-page task-page))
  (cond
    ;; Edit mode
    ((edit-mode-p task-page)
     (let ((task (task task-page)))
       (flet ((on-submit (&key title description cancel-button &allow-other-keys)
                (unless cancel-button
                  (setf (title task) title
                        (description task) description))
                ;; Switch back to read-only mode
                (setf (edit-mode-p task-page) nil)
                (update task-page)))

         (with-html ()
           (:form :onsubmit (make-js-form-action #'on-submit)
                  :style "display: flex; flex-direction: column; gap: 1rem"
                  (:input :type "text"
                          :name "title"
                          :value (title task))
                  (:textarea :name "description"
                             (description task))
                  (:div :style "display: flex; gap: 1rem"
                        (:input :type "submit"
                                :name "cancel-button"
                                :value "Cancel")
                        (:input :type "submit"
                                :name "save-button"
                                :value "Save")))))))
    ;; View mode
    (t
     (let ((task (task task-page))
           (list-url (route-url "tasks-list")))
       (with-html ()
         (:div :style "display: flex; flex-direction: column; gap: 1rem"
               (:h1 :style "display: flex; gap: 1rem; margin-bottom: 0"
                    (:b (if (done task)
                            "[DONE]"
                            "[TODO]"))
                    (:span :style "font-weight: normal"
                           (title task)))
               (:div (if (string= (description task) "")
                         "No defails on this task."
                         (description task)))
               (:div :style "display: flex; gap: 1rem"
                     (:a :href list-url
                         "Back to task list.")
                     (flet ((on-edit (&key &allow-other-keys)
                              (setf (edit-mode-p task-page) t)
                              (update task-page)))
       
                       (:form :onsubmit (make-js-form-action #'on-edit)
                         (:input :type "submit"
                                 :value "Edit"))))))))))


;;;;;; Application

(defapp tasks
  :prefix "/"
  :routes ((page ("/<int:task-id>" :name "task-details")
             (make-task-page task-id))
           (page ("/" :name "tasks-list")
             (make-task-list "First"
                             "Second"
                             "Third"))))



(defun start (&key (port 8080))
  (reblocks/server:start :port port
                         :apps '(tasks)))
