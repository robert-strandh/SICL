(cl:in-package #:sicl-type)

(defgeneric expand-type-descriptor (type-descriptor environment))

(defgeneric expand-atomic (type environment))

(defgeneric expand-compound (type-descriptor head rest environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on EXPAND-ATOMIC.

(defmethod expand-atomic (type environment)
  (let* ((global-env (sicl-genv:global-environment environment))
	 (expander (sicl-genv:type-expander type global-env))
	 (type-class (sicl-environment:find-class type environment)))
    (cond ((not (null expander))
	   ;; We found an expander.  Expand the type call TYPEP
	   ;; recursively with the expanded type.
	   (expand-type-descriptor (funcall expander type) environment))
	  ((not (null type-class))
	   type)
	  (t
	   ;; The type has no expander associated with it and the type
	   ;; is not also a class.  Furthermore, there was no method
	   ;; on EXPAND-ATOMIC specialized to the name of the type.  
	   ;; This can only mean that TYPE is not a valid type.
	   (error "unknown type ~s" type)))))

(defmethod expand-atomic ((type (eql 'atom)) environment)
  (declare (ignore environment))
  type)

(defmethod expand-atomic ((type (eql 'short-float)) environment)
  (if (same-float-type-p type 'single-float)
      'single-float
      'short-float))

(defmethod expand-atomic ((type (eql 'single-float)) environment)
  type)

(defmethod expand-atomic ((type (eql 'double-float)) environment)
  (if (same-float-type-p type 'single-float)
      'single-float
      'double-float))

(defmethod expand-atomic ((type (eql 'long-float)) environment)
  (if (same-float-type-p type 'double-float)
      (if (same-float-type-p type 'single-float)
	  'single-float
	  'double-float)
      'long-float))

(defmethod expand-atomic ((type (eql 'keyword)) environment)
  (declare (ignore environment))
  type)

(defmethod expand-atomic ((type (eql 'nil)) environment)
  nil)

(defmethod expand-atomic ((type (eql 'simple-array)) environment)
  (declare (ignore environment))
  type)
