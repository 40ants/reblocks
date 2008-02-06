
(in-package :weblocks-demo)

;;; Employee
(defclass employee (person)
  ((company :accessor employee-company
	    :initarg :company
	    :type company)
   (contract :accessor employee-contract
	     :initarg :contract)))

;;; Grid View
(defview employee-grid-view (:type grid :inherit-from 'person-grid-view)
  (company :reader (compose #'company-name #'employee-company)))

;;; Data View
(defview employee-data-view (:type data :inherit-from 'person-data-view)
  (company :reader (compose #'company-name #'employee-company))
  contract)

;;; Form View
(defview employee-form-view (:type form :inherit-from 'person-form-view)
  (company :present-as (dropdown :choices #'all-companies
				 :label-key #'company-name)
	   :parse-as (object-id :class-name 'company)
	   :reader (compose #'object-id #'employee-company)
	   :requiredp t)
  (contract :present-as (radio :choices '(:full-time :part-time :consultant :intern))
	    :parse-as keyword))



