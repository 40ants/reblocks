(declaim (optimize (debug 3)))
(ql:quickload '(:weblocks :weblocks-ui :find-port))
(ql:quickload '(:weblocks-navigation-widget))

;; From quickstart: task, task-list.

(defpackage todo
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks-navigation-widget
                #:defroutes))
(in-package todo)


(defapp tasks)

(weblocks/debug:on)

(defvar *port* (find-port:find-port))

(defwidget task ()
    ((title
      :initarg :title
      :initform nil
      :accessor title)
     (done
      :initarg :done
      :initform nil
      :accessor done)
     (id
      :initarg :id
      :accessor id)))

(defparameter *store* (make-hash-table)
  "Dummy store for tasks: id -> task.")

(defparameter *counter* 0 "Simple counter for the hash table store.")

(defun make-task (title &key done)
  "Create a task and store it by its id."
  (let* ((id (incf *counter*))
         (task (make-instance 'task :title title :done done :id id)))
    (setf (gethash id *store*) task)
    task))

(defun get-task (id)
  (gethash id *store*))

;; Pretty printing:
;; #<TASK 1 "task with id", done: NIL>
;; instead of
;; #<TASK {1008C462B3}>
(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (with-accessors ((title title)
                     (done done)
                     (id id))
        task
      (format stream "~a \"~a\", done: ~a" id title done))))

(defun make-task-list (&rest rest)
  "Create some tasks from titles."
  (let ((tasks (loop for title in rest
                  collect (make-task title))))
    (make-instance 'task-list :tasks tasks)))

(defmethod add-task ((task-list task-list) title)
  (push (make-task title)
        (tasks task-list))
  (update task-list))

(defmethod toggle ((task task))
  (setf (done task)
        (if (done task)
            nil
            t))
  (update task))

(defmethod render ((task task))
  (with-html
    (:p (:input :type "checkbox"
                :checked (done task)
                :onclick (make-js-action
                          (lambda (&key &allow-other-keys)
                            (toggle task))))
        (:span (if (done task)
                   (with-html
                     (:s (title task)))
                   (:a :href (format nil "/tasks/~a" (id task))
                       (title task)))))))

(defwidget task-list ()
  ((tasks
    :initarg :tasks
    :accessor tasks)
   (custom
    :initarg :custom
    :accessor custom)))

(defmethod render ((task-list task-list))
  (with-html
    (:h1 "Tasks")
    (loop for task in (tasks task-list) do
      (render task))
    (with-html-form (:POST (lambda (&key title &allow-other-keys)
                                   (add-task task-list title)))
      (:input :type "text"
              :name "title"
              :placeholder "Task's title")
      (:input :type "submit"
              :value "Add"))))

;; Router

(defwidget task-page ()
  ((task
    :initarg :task
    :initform nil
    :accessor task-page-task)))

(defmethod render ((task-page task-page))
  (let ((task (task-page-task task-page)))
    (with-html
      (:div "Task " (id task))
      (:h1 (title task))
      (:div (if (done task) "Done!" "To Do."))
      (:div "Lorem ipsum…"))))

(defun not-found ()
  "Show a 404 not found page."
  (with-html
    (:div "Task not found.")))

(defun make-task-page ()
  (let* ((path (weblocks/request:get-path))
         (id (first (ppcre:all-matches-as-strings "\\d+" path)))
         (task (gethash (parse-integer id) *store*)))
    (if task
        (make-instance 'task-page :task task)
        (not-found))))

;; The router must return a widget.
(defroutes tasks-routes
  ("/tasks/\\d+" (make-task-page))
  ("/tasks/" (make-task-list "Make my first Weblocks app"
                             "Deploy it somewhere"
                             "Have a profit")))

(defmethod weblocks/session:init ((app tasks))
  (declare (ignorable app))
  (make-tasks-routes))

(weblocks/server:start :port *port*)

(defun reset ()
  (setf *counter* 0)
  (weblocks/debug:reset-latest-session))
