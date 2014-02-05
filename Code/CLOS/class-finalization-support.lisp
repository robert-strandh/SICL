(cl:in-package #:sicl-clos)

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

(defun compute-class-precedence-list-default (class)
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

;;; Implement the behavior of compute-effective-slot-definition for
;;; standard-class and funcallable-standard-class.  By passing
;;; slot-definition-class as an argument rather than looking it up in
;;; this function, we can use this function for built-in classes as well. 
(defun compute-effective-slot-definition-default
    (name direct-slot-definitions slot-definition-class)
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
	  :type type))))

(defun compute-slots-default (class)
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

(defun compute-slots-around-default (slots)
  (let ((next-location 0))
    (loop for slot in slots
	  do (when (eq (slot-definition-allocation slot) :instance)
	       (setf (s-location slot)
		     next-location)
	       (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DEFAULT-INITARGS.

(defun compute-default-initargs-default (class)
  (remove-duplicates
   (reduce #'append
	   ;; We use the reader DIRECT-DEFAULT-INITARGS rather than
	   ;; CLASS-DIRECT-DEFAULT-INITARGS so that this function
	   ;; works for built-in classes as well as standard classes.
	   (mapcar #'direct-default-initargs
		   (class-precedence-list class)))
   :test #'eq
   :key #'car
   :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-INHERITANCE.

(defun finalize-inheritance-default (class)
  (setf (c-precedence-list class) (compute-class-precedence-list class))
  (setf (c-slots class) (compute-slots class))
  (setf (c-default-initargs class) (compute-default-initargs class))
  ;; We set FINALIZED-P to TRUE before allocating the prototype
  ;; instance, because ALLOCATE-INSTANCE checks that the class is
  ;; finalized and if not, calls FINALIZE-INSTANCE, and we would have
  ;; an infinite recursion.
  (setf (c-finalized-p class) t)
  ;; FIXME: allocate a prototype here, maybe?
  ;;(setf (c-prototype class)
  ;;      (allocate-instance class)))
  )
