;; Part 1
(declaim (optimize (debug 3)))

(ql:quickload '(:weblocks :reblocks-ui :find-port))
(defpackage todo
  (:use #:cl
        #:reblocks-ui/form
        #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp))
(in-package todo)


(defapp tasks)

(reblocks/debug:on)

(defvar *port* (find-port:find-port))

(reblocks/server:start :port *port*)

;; Part 2: defining tasks


(defwidget task ()
    ((title
      :initarg :title
      :accessor title)
     (done
      :initarg :done
      :initform nil
      :accessor done)))

(defmethod render ((task task))
  (with-html
    (:span (if (done task)
               (with-html
                 (:s (title task)))
               (title task)))))


(defun make-task (title &key done)
  "Create a task."
  (make-instance 'task :title title :done done))

(defwidget task-list ()
  ((tasks
    :initarg :tasks
    :accessor tasks)))

(defmethod render ((widget task-list))
  (with-html
    (:h1 "Tasks")
    (:ul
     (loop for task in (tasks widget) do
          (:li (render task))))))

(defun make-task-list (&rest rest)
  "Create some tasks from titles."
  (let ((tasks (loop for title in rest
                  collect (make-task title))))
    (make-instance 'task-list :tasks tasks)))

(defmethod reblocks/session:init ((app tasks))
  (declare (ignorable app))
  (make-task-list "Make my first Weblocks app"
                  "Deploy it somewhere"
                  "Have a profit"))

(reblocks/debug:reset-latest-session)


;; Part 3: add-task

(defmethod add-task ((task-list task-list) title)
  (push (make-task title)
        (tasks task-list))
  (update task-list))


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

(reblocks/debug:reset-latest-session)


;; Part 4: toggle

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
                   (title task))))))


(reblocks/debug:reset-latest-session)
