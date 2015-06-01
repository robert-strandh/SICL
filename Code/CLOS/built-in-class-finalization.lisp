(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a built-in class.

(defun compute-built-in-slots (class)
  (let* ((superclasses (precedence-list class))
	 (direct-slots (mapcar #'class-direct-slots superclasses))
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
	       (setf (slot-definition-location slot)
		     next-location)
	       (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-BUILT-IN-INHERITANCE.

(defun finalize-built-in-inheritance (class)
  (setf (precedence-list class) (compute-class-precedence-list class))
  (let* ((effective-slots (compute-built-in-slots class))
	 (slot-count
	   (count-list :instance effective-slots
		       :test #'eq :key #'slot-definition-allocation)))
    (setf (instance-size class) (+ slot-count 1))
    (setf (c-slots class) effective-slots))
  (setf (class-default-initargs class) (compute-default-initargs-default class))
  (setf (class-finalized-p class) t))
