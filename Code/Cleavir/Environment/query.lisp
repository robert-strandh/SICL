(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-INFO.

(defgeneric variable-info (environment symbol))

(defmethod variable-info :around (environment symbol)
  (let ((result (call-next-method)))
    (loop while (null result)
	  do (restart-case (error 'no-variable-info
				  :name symbol)
	       (consider-special ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
		 (return-from variable-info
		   (make-instance 'special-variable-info
		     :name symbol)))
	       (substitute (new-symbol)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read)))
		 (setq result (variable-info environment new-symbol)))))
    result))

(defclass lexical-variable-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)
   (%type :initform t :initarg :type :reader type)
   (%ignore :initform nil :initarg :ignore :reader ignore)
   (%dynamic-extent :initform nil :initarg :dynamic-extent :reader dynamic-extent)))

(defclass special-variable-info ()
  ((%name :initarg :name :reader name)
   (%type :initform t :initarg :type :reader type)
   (%ignore :initarg nil :initarg :ignore :reader ignore)))

(defclass constant-variable-info ()
  ((%name :initarg :name :reader name)
   (%value :initarg :value :reader value)))

(defclass symbol-macro-info ()
  ((%name :initarg :name :reader name)  
   (%type :initform t :initarg :type :reader type)
   (%expansion :initarg :expansion :reader expansion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-INFO.

(defgeneric function-info (environment function-name))

(defmethod function-info :around (environment function-name)
  (let ((result (call-next-method)))
    (loop while (null result)
	  do (restart-case (error 'no-function-info
				  :name function-name)
	       (consider-global ()
		 :report (lambda (stream)
			   (format stream
				   "Treat it as the name of a global function."))
		 (return-from function-info
		   (make-instance 'global-function-info
		     :name function-name)))
	       (substitute (new-function-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read)))
		 (setq result (function-info environment new-function-name)))))
    result))

(defclass local-function-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)
   (%type :initform t :initarg :type :reader type)
   (%inline :initform nil :initarg :inline :reader inline)
   (%ignore :initform nil :initarg :ignore :reader ignore)
   (%dynamic-extent :initform nil
		    :initarg :dynamic-extent
		    :reader dynamic-extent)))
  
(defclass global-function-info ()
  ((%name :initarg :name :reader name)
   (%type :initform t :initarg :type :reader type)
   (%inline :initform nil :initarg :inline :reader inline)
   (%compiler-macro :initform nil :initarg :compiler-macro :reader compiler-macro)
   (%ignore :initform nil :initarg :ignore :reader ignore)
   (%dynamic-extent :initform nil
		    :initarg :dynamic-extent
		    :reader dynamic-extent)))
  
(defclass local-macro-info ()
  ((%name :initarg :name :reader name)
   (%expander :initarg :expander :reader expander)))

(defclass global-macro-info ()
  ((%name :initarg :name :reader name)
   (%expander :initarg :expander :reader expander)
   (%compiler-macro :initform nil
		    :initarg :compiler-macro
		    :reader compiler-macro)))

(defclass special-operator-info ()
  ((%name :initarg :name :reader name)))

(defgeneric block-info (environment symbol))

(defclass block-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defgeneric tag-info (environment tag))

(defclass tag-info ()
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defgeneric optimize-info (environment))

(defclass optimize-info ()
  ((%speed :initform 3 :initarg :speed :reader speed)
   (%debug :initform 3 :initarg :debug :reader debug)
   (%space :initform 3 :initarg :space :reader space)
   (%safety :initform 3 :initarg :safety :reader safety)
   (%compilation-speed :initform 3
		       :initarg :compilation-speed
		       :reader compilation-speed)))

(defgeneric type-expand (environment type))
