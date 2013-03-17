(in-package #:sicl-clos)

(defgeneric finalize-inheritance (class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the class precedence list

;;; For a given class, compute a precedence relation.  
;;; This relation is represented as a list of cons cells, 
;;; where the class in the CAR of the cell takes precedence over
;;; the class in the CDR of the cell. For a class C with a list of 
;;; direct superclasses (S1 S2 ... Sn), the relation contains 
;;; the elements (C . S1) (S1 . S2) ... (Sn-1 . Sn).
(defun compute-relation (class)
  (loop for prev = class then super
        for super in (class-direct-superclasses class)
        collect (cons prev super)))

(defgeneric compute-class-precedence-list (class))

(defmethod compute-class-precedence-list ((class class))
  ;; Make sure all the direct superclasses are already finalized so
  ;; that we can use their precedence lists.
  (loop for super in (class-direct-superclasses class)
	do (unless (class-finalized-p super)
	    (finalize-inheritance super)))
  (let* ((all-supers (cons class
			   (remove-duplicates
			    (reduce #'append
				    (mapcar #'class-precedence-list
					    (class-direct-superclasses class))))))
	 (relation (loop for class in all-supers
		         append (compute-relation class)))
	 (reverse-result '()))
    (flet ((find-candidate ()
	     (let ((candidates
		     ;; Start by looking for all remaining classes that
		     ;; depend on no other class according to the relation.
		     (loop for super in all-supers
			   unless (find super relation :key #'cdr)
			     collect super)))
	       ;; If no such class exists, we have a circular dependence,
	       ;; and so we can't compute the class precedence list.
	       (when (null candidates)
		 ;; FIXME: do this better
		 (error "can't compute class precedence list"))
	       (if (null (cdr candidates))
		   ;; A unique candiate, return it.
		   (car candidates)
		   ;; Several candidates.  Look for the last class in the
		   ;; result computed so far (first in the reversed result)
		   ;; that has one of the candidates as a direct superclass.
		   ;; Return that candidate.  
		   ;; There can be at most one, because within a list of
		   ;; superclasses, there is already a dependency, so that
		   ;; two different classes in a single list of superclasses
		   ;; can not both be candidates.
		   ;; There must be at least one, because ... 
		   ;; (FIXME: prove it!)
		   (loop for element in reverse-result
		         do (loop for candidate in candidates
			          do (when (member candidate
						   (class-direct-superclasses
						    element))
				       (return-from find-candidate
					 candidate))))))))
      (loop until (null all-supers)
	    do (let ((candidate (find-candidate)))
		 (push candidate reverse-result)
		 (setf all-supers (remove candidate all-supers))
		 (setf relation (remove candidate relation :key #'car)))))
    (reverse reverse-result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a class.

(defgeneric compute-effective-slot-definition
    (class name direct-slot-definitions))

;;; Implement the behavior of compute-effective-slot-definition
;;; for standard-class and funcallable-standard-class.
(defun compute-effective-slot-definition-aux
    (class name direct-slot-definitions)
  (let (allocation initargs initform initfunction type location)
    (setf allocation
	  (slot-definition-allocation (first direct-slot-definitions)))
    ;; When allocation is :CLASS, we use the CONS cell of the 
    ;; direct slot definition that determined this allocation as
    ;; the location of the final slot.  If not, the location is
    ;; set to NIL and will be assigned by the appropriate :around
    ;; method. 
    (setf location
	  (if (eq allocation :class)
	      (slot-definition-storage (first direct-slot-definitions))
	      nil))
    (setf initargs
	  (reduce #'union
		  (mapcar #'slot-definition-initargs direct-slot-definitions)))
    (let ((first-init (find-if-not #'null direct-slot-definitions
				   :key #'slot-definition-initfunction)))
      (unless (null first-init)
	(setf initform (slot-definition-initform first-init)
	      initfunction (slot-definition-initfunction first-init))))
    (setf type
	  `(and ,@(mapcar #'slot-definition-type direct-slot-definitions)))
    (let ((slot-definition-class (effective-slot-definition-class class)))
      (if (null initfunction)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :location location 
			 :initargs initargs
			 :type type)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :location location 
			 :initargs initargs
			 :initform initform
			 :initfunction initfunction
			 :type type)))))

(defmethod compute-effective-slot-definition ((class standard-class)
					      name
					      direct-slot-definitions)
  (compute-effective-slot-definition-aux
   class
   name
   direct-slot-definitions))

(defmethod compute-effective-slot-definition ((class funcallable-standard-class)
					      name
					      direct-slot-definitions)
  (compute-effective-slot-definition-aux
   class
   name
   direct-slot-definitions))

(defun compute-slots-aux (class)
  (let* ((superclasses (class-precedence-list class))
	 (direct-slots (mapcar #'class-direct-slots superclasses))
	 (concatenated (reduce #'append direct-slots))
	 (reverse-slots (reverse direct-slots))
	 (reverse-concatenated (reduce #'append reverse-slots))
	 (names (mapcar #'slot-definition-name reverse-concatenated))
	 (unique (remove-duplicates names :from-end t)))
    (loop for name in unique
	  collect (compute-effective-slot-definition
		   class
		   name
		   (remove name concatenated
			   :key #'slot-definition-name
			   :test-not #'eql)))))

(defgeneric compute-slots (class))

(defmethod compute-slots ((class standard-class))
  (compute-slots-aux class))

(defmethod compute-slots ((class funcallable-standard-class))
  (compute-slots-aux class))

(defmethod compute-slots :around ((class standard-class))
  (let ((slots (call-next-method))
	(next-location 0))
    (loop for slot in slots
	  do (when (eq (slot-definition-allocation slot) :instance)
	       (setf (slot-definition-location slot)
		     next-location)
	       (incf next-location)))
    slots))

(defmethod compute-slots :around ((class funcallable-standard-class))
  (let ((slots (call-next-method))
	(next-location 0))
    (loop for slot in slots
	  do (when (eq (slot-definition-allocation slot) :instance)
	       (setf (slot-definition-location slot)
		     next-location)
	       (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DEFAULT-INITARGS.

(defgeneric compute-default-initargs (class))

(defun compute-default-initargs-aux (class)
  (remove-duplicates
   (reduce #'append
	   (mapcar #'class-direct-default-initargs
		   (class-precedence-list class)))
   :test #'eq
   :key #'car
   :from-end t))

(defmethod compute-default-initargs ((class standard-class))
  (compute-default-initargs-aux class))

(defmethod compute-default-initargs ((class funcallable-standard-class))
  (compute-default-initargs-aux class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-INHERITANCE.

(defun finalize-inheritance-aux (class)
  (setf (c-precedence-list class) (compute-class-precedence-list class))
  (setf (c-slots class) (compute-slots class))
  (setf (c-default-initargs class) (compute-default-initargs class))
  ;; We set FINALIZED-P to TRUE before allocating the prototype
  ;; instance, because ALLOCATE-INSTANCE checks that the class is
  ;; finalized and if not, calls FINALIZE-INSTANCE, and we would have
  ;; an infinite recursion.
  (setf (c-finalized-p class) t)
  (setf (c-prototype class)
	(allocate-instance class)))

(defmethod finalize-inheritance ((class standard-class))
  (finalize-inheritance-aux class))

(defmethod finalize-inheritance ((class funcallable-standard-class))
  (finalize-inheritance-aux class))

