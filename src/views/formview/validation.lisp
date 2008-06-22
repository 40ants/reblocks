
(in-package :weblocks)

(export '(validate-object-form-view validate-form-view-field))

(defgeneric validate-object-form-view (object view parsed-values)
  (:documentation "Called by the framework during form deserialization
to validate a form view. Default implementation validates each field
by calling 'validate'form-view-field'.

If this function succeeds validating the form it returns
true. Otherwise returns nil as the first value, and an association
list of fields and errors as the second value.

'object' - the object the form is being deserialized into.
'view' - form view object being deserialized.
'parsed-values' - an association list of field-info structures and
parsed values.")
  (:method (object (view form-view) parsed-values)
    (let ((validates t)
	  errors)
      (dolist (info-value-pair parsed-values)
	(destructuring-bind (field-info . parsed-value)
	    info-value-pair
	  (multiple-value-bind (validatesp error)
	      (let ((field (field-info-field field-info))
		    (object (field-info-object field-info)))
		(validate-form-view-field (view-field-slot-name field)
					  object field view parsed-value))
	    (unless validatesp
	      (setf validates nil)
	      (push-end (cons (field-info-field field-info) error) errors)))))
      (if validates
	  t
	  (values nil errors)))))

(defgeneric validate-form-view-field (slot-name object field view parsed-value)
  (:documentation "Called by 'validate-object-form-view' during form
deserialization to validate a form view field. Default implementation
ensures that the parsed value satisfies any type requirements declared
on the slot (unless the field has a custom writer, in which case this
condition is waived), and functions declared on the 'satisfies' slot
of the form-view-field.

If the field is validated, the function returns true. Otherwise,
returns nil as the first value and an error message as the second
value. Default implementation obtains the error message by calling
'parser-error-message' on the field's parser.

'slot-name' - the name of the slot being validated.
'object' - object being validated.
'field' - form-view-field being validated.
'view' - form-view being validated.
'parsed-value' - the value parsed from the request.")
  (:method (slot-name object (field form-view-field) (view form-view) parsed-value)
    (let* ((slot-esd (find-slot-esd (class-of object) slot-name))
	   (slot-type (if slot-esd
			  (slot-definition-type slot-esd)
			  t))
	   (validators (remove nil
			       (cons (unless (slot-boundp field 'writer)
				       (lambda (value)
					 (typep value slot-type)))
				     (ensure-list (form-view-field-satisfies field))))))
      (loop
	 for validator in validators
	 do (multiple-value-bind (result error) (funcall validator parsed-value)
	      (unless result
		(return (values nil (or error
					(parser-error-message (form-view-field-parser field)))))))
	 finally (return t)))))
