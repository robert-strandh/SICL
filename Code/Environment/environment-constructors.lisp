(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors for locations.

(defun make-global-location (name)
  (make-instance 'global-location :name name))

(defun make-special-location (name)
  (make-instance 'special-location :name name))

(defun make-constant-variable-entry (name definition)
  (make-instance 'constant-variable-entry
		 :name name
		 :definition definition))

(defun make-special-variable-entry (name &optional defined-p)
  (make-instance 'special-variable-entry
		 :name name
		 :location (make-special-location name)
		 :defined-p defined-p))

(defun make-symbol-macro-entry (name expansion)
  (let ((expander (lambda (form environment)
		    (declare (ignore form environment))
		    expansion)))
    (make-instance 'symbol-macro-entry
		   :name name
		   :definition expander)))

(defun make-function-entry (name &optional
					  (lambda-list :none)
					  ast
					  parameters)
  (declare (cl:type function-name name))
  (make-instance 'function-entry
		 :name name
		 :lambda-list lambda-list
		 :ast ast
		 :parameters parameters
		 :location (make-global-location name)))

(defun make-macro-entry (name expander)
  (make-instance 'macro-entry
		 :name name
		 :definition expander))

(defun make-compiler-macro-entry (base-entry expander)
  (make-instance 'compiler-macro-entry
		 :base-entry base-entry
		 :definition expander))

(defun make-type-entry (name expander)
  (make-instance 'type-entry
		 :name name
		 :definition expander))

(defun make-type-declaration-entry (location-entry type)
  (make-instance 'type-declaration-entry
		 :base-entry location-entry
		 :type type))

(defun make-inline-declaration-entry (base-entry)
  (make-instance 'inline-declaration-entry
		 :base-entry base-entry))

(defun make-notinline-declaration-entry (base-entry)
  (make-instance 'notinline-declaration-entry
		 :base-entry base-entry))

(defun make-dynamic-extent-declaration-entry (location-entry)
  (make-instance 'dynamic-extent-declaration-entry
		 :location (location location-entry)))

(defun make-ignore-declaration-entry (location-entry)
  (make-instance 'ignore-declaration-entry
		 :location (location location-entry)))

(defun make-ignorable-declaration-entry (location-entry)
  (make-instance 'ignorable-declaration-entry
		 :location (location location-entry)))

(defun make-optimize-declaration-entry (quality &optional (value 3))
  (make-instance 'optimize-declaration-entry
		 :quality quality
		 :value value))

(defun make-declaration-declaration-entry (name)
  (make-instance 'declaration-declaration-entry
		 :name name))

