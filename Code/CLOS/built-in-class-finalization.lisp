(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a built-in class.

(defun compute-built-in-slots (class)
  (let* ((superclasses (class-precedence-list class))
	 (direct-slots (mapcar #'direct-slots superclasses))
	 (concatenated (reduce #'append direct-slots))
	 (reverse-slots (reverse direct-slots))
	 (reverse-concatenated (reduce #'append reverse-slots))
	 (names (mapcar #'slot-definition-name reverse-concatenated))
	 (unique (remove-duplicates names :from-end t))
	 (slots (loop for name in unique
		      collect (compute-effective-slot-definition-default
			       name
			       (remove name concatenated
				       :key #'slot-definition-name
				       :test-not #'eql)
			       'standard-effective-slot-definition)))
	 (next-location 1))
    (loop for slot in slots
	  do (when (eq (slot-definition-allocation slot) :instance)
	       (setf (s-location slot)
		     next-location)
	       (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-BUILT-IN-INHERITANCE.

(defun finalize-built-in-inheritance (class)
  (setf (c-precedence-list class) (compute-class-precedence-list class))
  (setf (c-slots class) (compute-built-in-slots class))
  (setf (c-default-initargs class) (compute-default-initargs-default class))
  (setf (c-finalized-p class) t))
