(uiop:define-package #:weblocks/widget
  (:use #:cl)
  (:import-from #:weblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:weblocks/widgets/mop
                #:widget-class)
  (:import-from #:weblocks/commands
                #:add-command)
  (:import-from #:weblocks/request
                #:ajax-request-p)
  (:import-from #:weblocks/widgets/dom
                #:dom-id
                #:dom-object-mixin)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:cl-strings
                #:join)
  (:import-from #:spinneret
                #:get-html-path)

  ;; Just dependencies
  (:import-from #:log)

  (:export #:defwidget
           #:render
           #:get-css-classes
           #:get-html-tag
           #:mark-dirty
           #:update
           #:widget
           #:create-widget-from))
(in-package weblocks/widget)


(defclass widget (dom-object-mixin)
  ((propagate-dirty :accessor widget-propagate-dirty
                    :initform nil
                    :initarg :propagate-dirty
                    :documentation "A list of widgets which will be made
                    dirty when this widget is made dirty via a POST
                    request. This slot allows setting up dependencies
                    between widgets that will make multiple widgets
                    update automatically during AJAX requests.")
   (continuation :accessor widget-continuation
                 :initform nil
                 :documentation "Stores the continuation object for
                 widgets that were invoked via one of the do-*
                 functions ('do-page', etc.). When 'answer' is called
                 on a widget, this value is used to resume the
                 computation."))
  #+lispworks (:optimize-slot-access nil)
  (:metaclass widget-class)
  (:documentation "Base class for all widget objects."))


(defmacro defwidget (name direct-superclasses &body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds WEBLOCKS/WIDGETS/MOP:WIDGET-CLASS metaclass specification and
inherits from WEBLOCKS/WIDGET:WIDGET if no DIRECT-SUPERCLASSES are provided."
  `(progn
     (defclass ,name ,(remove-duplicates
                       (or direct-superclasses
                           '(widget)))
       ,@body
       (:metaclass widget-class))))


(defgeneric render (widget)
  (:documentation "Define this method to render widget's content.

                   Use WEBLOCKS/HTML:WITH-HTML macro to render HTML.
                   You can use any other templating engine, just ensure
                   it writes output to WEBLOCKS/HTML:*STREAM*

                   Outer DIV wrapper will be added automaticall. It will
                   have CSS tags returned by GET-CSS-CLASSES."))


(defgeneric get-html-tag (widget)
  (:documentation "This method should return a keyword, like :div or :article.
                   By default, it returns :div.
                   If we are inside a table, it returns :tr"))


(defmethod get-html-tag ((widget t))
  (case (first (get-html-path))
    (:table
     :tr)
    (:tr
     :td)
    (t
     :div)))


(defgeneric get-css-classes (widget)
  (:documentation "Returns a list of classes for the widget.
                   Classes may be a strings or a keywords.
                   By default, :widget and keyworded class name are returned.
                   Use (append (list :new-class) (call-next-method))
                   to add new classes."))


(defmethod get-css-classes ((widget t))
  (list :widget
        (make-keyword
         (class-name (class-of widget)))))


(defun get-css-classes-as-string (widget)
  (let* ((classes (get-css-classes widget))
         (stringified (loop for cls in classes
                            collect (etypecase cls
                                      (string cls)
                                      (keyword (string-downcase (symbol-name cls)))))))
    (join stringified :separator " ")))


;; (defgeneric mark-dirty (w &key propagate)
;;   (:documentation
;;    "Default implementation adds a widget to a list of dirty
;; widgets. Normally used during an AJAX request. If there are any
;; widgets in the 'propagate-dirty' slot of 'w' and 'propagate' is true
;; \(the default\), these widgets are added to the dirty list as well.

;; Note that this function is automatically called when widget slots are
;; modified, if slots are marked have affects-dirty-status-p flag.

;; Returns NIL if the widget is already dirty or T and the results
;; of calling MARK-DIRTY on the list of dependents \(propagate-dirty\)."))


;; (defmethod mark-dirty ((w widget) &key (propagate t))
;;   (unless (weblocks:widget-dirty-p w)
;;     (pushnew w weblocks::*dirty-widgets*)
;;     ;; NOTE: we have to check for unbound slots because this function
;;     ;; may get called at initialization time before those slots are bound
;;     (values t (when (and propagate (slot-boundp w 'propagate-dirty))
;;                 (mapc #'mark-dirty
;;                       (remove nil (widget-propagate-dirty w)))))))


(defgeneric update (w &key inserted-after inserted-before)
  (:documentation "This method should be called to update widget on a client.

Usually this required as a result of an action execution.

In the old weblocks there was a mark-dirty method. This one replaces it.
To make everything easier, the new protocol excludes \"propagation\". If you
need to update other widgets, please define an \"update\" method for your widget.
You can use :before or :after modifiers, to keep the current behavior and to add
propagation code."))


(defmethod update ((w widget) &key inserted-after inserted-before)
  (log:debug "Updating widget" w inserted-after inserted-before)
  (cond
    ((and inserted-after inserted-before)
     (error "Arguments inserted-after and inserted-before can't be used together."))
    (inserted-after (add-command
                     :insert-widget
                     :widget (with-html-string
                               (render w))
                     :after (dom-id inserted-after)))
    (inserted-before (add-command
                      :insert-widget
                      :widget (with-html-string
                                (render w))
                      :before (dom-id inserted-before)))
    (t (add-command
        :update-widget
        :dom-id (dom-id w)
        :widget (with-html-string
                  (render w))))))


(defgeneric create-widget-from (object)
  (:documentation "Methods of this generic should return an instance of subclass of weblocks/widget:widget
                   The most obvious cases are transformation of strings and functions into the widget, but
                   these methods are already supplied by Weblocks."))


(defmethod create-widget-from ((object widget))
  "If input is already a widget, then it is returned as is."
  object)




