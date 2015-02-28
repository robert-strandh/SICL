(cl:in-package #:sicl-additional-types)

;;; These definitions exist mostly for correctness.
;;; You don't want to use them in performance-critical code becase
;;; they are quite costly.
(defun circular-list-p (object)
  (and (consp object)
       (progn
	 ;; First we attempt to just traverse the list as usual,
	 ;; assuming that it is fairly short.  If we reach the end,
	 ;; then that's great, and we return NIL.
	 (loop for remaining = object then (cdr remaining)
	       for count from 0 to 100
	       while (consp remaining)
	       finally (when (atom remaining)
			 (return-from circular-list-p nil)))
	 ;; Come here if the list has more than a few CONS cells.  We
	 ;; traverse it again, this time entering each CONS cell in a
	 ;; hash table.  Stop when we reach the end of the list, or when
	 ;; we see the same CONS cell twice.
	 (let ((table (make-hash-table :test #'eq)))
	   (loop for remaining = object then (cdr remaining)
		 while (consp remaining)
		 until (gethash remaining table)
		 do (setf (gethash remaining table) t)
		 finally (return (consp remaining)))))))
  
(defun dotted-list-p (object)
  (and (not (circular-list-p object))
       (consp object)
       (not (null (cdr (last object))))))

;;; Nothing says that an association list can't be circular.
(defun association-list-p (object)
  (or (null object)
      (and (consp object)
	   (let ((table (make-hash-table :test #'eq)))
	     (loop for remaining = object then (cdr remaining)
		   while (consp remaining)
		   until (gethash remaining table)
		   do (unless (consp (car remaining))
			(return nil))
		      (setf (gethash remaining table) t)
		   finally (return (listp remaining)))))))

;;; Nothing says that a property list can't be circular.
(defun property-list-p (object)
  (or (null object)
      (and (consp object)
	   (let ((table (make-hash-table :test #'eq)))
	     (loop for remaining = object then (cdr remaining)
		   for i from 0
		   while (consp remaining)
		   until (gethash remaining table)
		   do (setf (gethash remaining table) t)
		   finally (return (or (consp remaining)
				       (and (null remaining)
					    (evenp i)))))))))

(deftype circular-list ()
  `(satisfies circular-list-p))

(deftype dotted-list ()
  `(satisfies dotted-list-p))

(deftype proper-list ()
  `(and list (not circular-list) (not dotted-list)))
  
(deftype association-list ()
  `(satisfies association-list-p))

(deftype property-list ()
  `(satisfies association-list-p))

;;; This type is introduced just for documentation purposes.
(deftype generalized-boolean () t)

;;; A general predicate is a function that takes an 
;;; arbitrary object, and returns a generalized boolean.
;;; We also require that a general predicate is pure, i.e.
;;; that it no observable effect on the environment, and 
;;; that its return value depends only on its argument.
(deftype general-predicate () '(function (t) generalized-boolean))

;;; A symbol predicate is like a general predicate, but its argument
;;; must be of type symbol. 
(deftype symbol-predicate () '(function (symbol) generalized-boolean))

;;; This function is used in the definition of the type 
;;; character-designator 
(defun name-of-length-1 (symbol)
  (= 1 (length (symbol-name symbol))))

(deftype character-designator ()
  '(or character (string 1) (and symbol (satisfies name-of-length-1))))

;;; Used for function arguments that apply a key function
;;; before comparing two objects.
(deftype keyfun () '(function (t) t))

(deftype keyfun-designator () '(or keyfun symbol))

;;; Used for one-argument tests
(deftype testfun1 () '(function (t) generalized-boolean))

(deftype testfun1-designator () '(or testfun1 symbol))

;;; Used for two-argument tests
(deftype testfun2 () '(function (t t) generalized-boolean))

(deftype testfun2-designator () '(or testfun2 symbol))

;;; Maybe remove this one?
(deftype nonnegative-fixnum () '(and fixnum unsigned-byte))

(defun setf-function-name-p (object)
  (and (consp object)
       (consp (cdr object))
       (null (cddr object))
       (eq (car object) 'setf)
       (symbolp (cadr object))))

(deftype setf-function-name ()
  `(satisfies setf-function-name-p))

(deftype function-name ()
  `(or symbol setf-function-name))

(deftype function-designator () '(or function symbol))

(deftype extended-function-designator () '(or function function-name))

(deftype string-designator () '(or character symbol string))

(deftype pathname-designator () '(or string stream pathname))

(deftype package-designator () '(or (string-designator package)))

(deftype byte-specifier () t)

(deftype radix () '(integer 2 36))

(defun end-is-not-less-than-start-p (pair)
  (>= (cdr pair) (car pair)))

(deftype bunding-indexes (&optional length)
  `(or (cons (integer 0 (,length)) null)
       (and (cons (integer 0 (,length)) (integer 0 (,length)))
	    (satisfies end-is-not-less-than-start-p))))
