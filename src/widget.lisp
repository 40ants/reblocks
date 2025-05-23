(uiop:define-package #:reblocks/widget
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:reblocks/widgets/mop
                #:widget-class)
  (:import-from #:reblocks/commands
                #:add-commands
                #:get-collected-commands
                #:with-collected-commands
                #:add-command)
  (:import-from #:reblocks/widgets/dom
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
           #:update
           #:widget
           #:create-widget-from))
(in-package #:reblocks/widget)


(defclass widget (dom-object-mixin)
  ()
  #+lispworks
  (:optimize-slot-access nil)
  (:metaclass widget-class)
  (:documentation "Base class for all widget objects."))


(defmacro defwidget (name direct-superclasses &body body)
  "A macro used to define new widget classes. Behaves exactly as
defclass, except adds REBLOCKS/WIDGETS/MOP:WIDGET-CLASS metaclass specification and
inherits from REBLOCKS/WIDGET:WIDGET if no DIRECT-SUPERCLASSES are provided."
  `(progn
     (defclass ,name ,(remove-duplicates
                       (or direct-superclasses
                           '(widget)))
       ,@body
       (:metaclass widget-class))))


(defgeneric render (widget)
  (:documentation "Define this method to render widget's content.

                   Use REBLOCKS/HTML:WITH-HTML macro to render HTML.
                   You can use any other templating engine, just ensure
                   it writes output to REBLOCKS/HTML:*STREAM*

                   Outer DIV wrapper will be added automaticall, see GET-HTML-TAG generic-function.
                   It will have CSS tags returned by GET-CSS-CLASSES generic-function."))


(defgeneric get-html-tag (widget)
  (:documentation "This method determines the enclosing tag of the widget.

The return value should either be a keyword like `:div`,
which will be the enclosing tag, or a list of the form `(:tag . attributes)`,
where `:tag` is the enclosing tag (like `:div`) and attributes is a property list.

The attributes property list has keywords for keys, corresponding to
the attribute name and the values are the values of the attribute.

For example:

- `:div` - generates `<div ...> widget content </div>`;
- `(:div :display \"flex\")` - generates `(<div ... :display \"flex\">widget content</div>`.

Note on attributes: in the attribute list the following attributes can
not be specified, they will be ignored:

- `:class` - Use the GET-CSS-CLASSES generic-function to specify these;
- `:id`    - This is the value of the dom-id slot of the widget,
             normally automatically managed by reblocks.

The default implementation returns
- `:td`  - inside a table row;
- `:tr`  - inside a table;
- `:div` - by default."))


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
                   Use `(list* :new-class (call-next-method))`
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


(defgeneric update (w &key inserted-after inserted-before removed)
  (:documentation "This method should be called to update widget on a client.

Usually this required as a result of an action execution.

In the original Weblocks there was a mark-dirty method. This one replaces it.
To make everything easier, the new protocol excludes \"propagation\". If you
need to update other widgets, please define an \"update\" method for your widget.
You can use :before or :after modifiers, to keep the current behavior and to add
propagation code.

There are a few optional arguments to control the way how does widget should be updated.
They can be useful to not update the whole parent widget.

- If one of INSERTED-AFTER or INSERTED-BEFORE is given, it should be a widget
  object to be used as an anchor. This can be useful to not rerender all children
  of some \"list\" widget when you adding a new element.
- When REMOVED argument is T the widget will be removed from the DOM tree.
"))


(defmethod update ((w widget) &key inserted-after inserted-before removed)
  (log:debug "Updating widget" w inserted-after inserted-before)
  (when (and inserted-after inserted-before)
    (error "Arguments inserted-after and inserted-before can't be used together."))
  
  (let* ((update-commands nil)
         (rendered-widget
           ;; Here we'll save all commands related to the widget
           ;; into a separate list and will add them to the main
           ;; commands list after the main command for adding rendered widget.
           ;; This way if any Javascript will be executed only after the widget update
           ;; on the frontend.
           (unless removed
             (with-collected-commands ()
               (prog1
                   (with-html-string ()
                     (render w))
                 (setf update-commands
                       (get-collected-commands)))))))
    (cond
      (removed
       (add-command :remove-widget
                    :dom-id (dom-id w)))
      (inserted-after (add-command
                       :insert-widget
                       :widget rendered-widget
                       :after (dom-id inserted-after)))
      (inserted-before (add-command
                        :insert-widget
                        :widget rendered-widget
                        :before (dom-id inserted-before)))
      (t (add-command
          :update-widget
          :dom-id (dom-id w)
          :widget rendered-widget)))
    
    (add-commands update-commands)
    (values)))


(defgeneric create-widget-from (object)
  (:documentation "Methods of this generic should return an instance of subclass of reblocks/widget:widget
                   The most obvious cases are transformation of strings and functions into the widget, but
                   these methods are already supplied by Reblocks.

                   If REBLOCKS/PAGE:INIT-PAGE returns an object, then CREATE-WIDGET-FROM will be called on it
                   to create the root widget."))


(defmethod create-widget-from ((object t))
  "If this method is called, that means that object is not of a proper type. Say it
with an explicit error message."
  (error "We tried to create a widget from ~A but this object type is not recognized. Did you forget to create a widget?~&There is no applicable method for REBLOCKS/WIDGET:CREATE-WIDGET-FROM."
         object))

(defmethod create-widget-from ((object symbol))
  "If input is a symbol named a widget class, then we'll MAKE-INSTANCE of this class."
  (unless (subtypep object 'widget)
    (error "~S is not subclass of the REBLOCKS/WIDGET:WIDGET"
           object))
  
  (make-instance object))


(defmethod create-widget-from ((object widget))
  "If input is already a widget, then it is returned as is."
  object)




