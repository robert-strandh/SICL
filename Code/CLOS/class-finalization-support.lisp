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

(defun compute-class-precedence-list-assuming-superclasses-finalized (class)
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
                 (error 'unable-to-compute-class-precedence-list
                        :offending-class class))
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

(defun compute-class-precedence-list-default (class)
  ;; Make sure all the direct superclasses are already finalized so
  ;; that we can use their precedence lists.
  (loop for super in (class-direct-superclasses class)
        do (unless (class-finalized-p super)
             (finalize-inheritance super)))
  (compute-class-precedence-list-assuming-superclasses-finalized class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a class.

;;; Implement the behavior of compute-effective-slot-definition for
;;; standard-class and funcallable-standard-class.  By passing
;;; slot-definition-class as an argument rather than looking it up in
;;; this function, we can use this function for built-in classes as well.
;;;
;;; In this function, we use alternative readers for slot-definition
;;; slots.  Thus, we use INITARGS rather than
;;; SLOT-DEFINITION-INITARGS, INITFUNCTION rather than
;;; SLOT-DEFINITION-INITFUNCTION.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default action for the primary method on COMPUTE-SLOTS.
;;;
;;; The AMOP says that this function returns the resulting list of
;;; slots in an "unspecified order".  Analyzing the text of the AMOP a
;;; bit more, we understand that the order can be specified by the
;;; implementation, that this order determines the LOCATION of each
;;; directly accessible slot in that the default :AROUND method on
;;; COMPUTE-SLOTS allocates locations with increasing locations
;;; according to this order.
;;;
;;; Here, we want the slots to appear in an instance with slots
;;; defined in more general classes first, so that is what this
;;; function does.
;;;
;;; The AMOP says that lists of direct slot definitions are grouped by
;;; name into separate lists, and that each list is then sorted
;;; according to the class precedence list.  However, we can not use
;;; CLASS-PRECEDENCE-LIST to access the precedence list, because the
;;; AMOP also stipulates that this function signals an error if the
;;; class is not finalized, and computing the effective slots is part
;;; of the class finalization protocol.  For that reason, we use the
;;; alternative reader PRECEDENCE-LIST that works just like the normal
;;; CLASS-PRECEDENCE-LIST, except that it just accesses the slot, and
;;; does not signal an error if the class has not been finalized.
;;;
;;; The AMOP does not say what is supposed to happen if this generic
;;; function is called if the class precedence list has not first been
;;; computed and made available.  Here (at least for now), we make the
;;; assumption that the application programmer is not supposed to call
;;; this function directly, so that it is only called from
;;; FINALIZE-INHERITANCE, and that the application programmer is only
;;; allowed to write methods on it.

;;; FIXME: We are probably not doing this quite right.  We should
;;; probably use the class precedence list directly to sort the direct
;;; slot definitions within a list with the same slot name.
(defun compute-slots-default (class)
  (let* (;; Do NOT call CLASS-PRECEDENCE-LIST here (see comment
         ;; above).  Instead use alternative reader PRECEDENCE-LIST.
         (superclasses (precedence-list class))
         ;; We can't use CLASS-DIRECT-SLOTS here, because according to
         ;; the AMOP it must return the empty list for built-in
         ;; classes, and we need to inherit slots from built-in
         ;; classes as well.  For that reason, we use a different
         ;; reader named DIRECT-SLOTS which does the same thing as
         ;; CLASS-DIRECT-SLOTS except that for built-in classes, it
         ;; retuns the direct slots of the built-in class.
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
                           :test-not #'eq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default action for :AROUND method on COMPUTE-SLOTS.
;;;
;;; The AMOP specifies that:
;;;
;;;    "For a given class, the locations increase consecutively, in
;;;     the order that the directly accessible slots appear in the
;;;     list of effective slots."
;;;
;;; It is not entirely clear what list of effective slots is meant,
;;; but we take this to mean that it is the list of effective slots
;;; that is both returned by the primary method and associated with
;;; the class.

(defun compute-slots-around-default (slots)
  (let ((next-location 2))
    (loop for slot in slots
          do (when (eq (slot-definition-allocation slot) :instance)
               (setf (slot-definition-location slot)
                     next-location)
               (incf next-location)))
    slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DEFAULT-INITARGS.
;;;
;;; The AMOP does not say what is supposed to happen if this generic
;;; function is called if the class precedence list has not first been
;;; computed and made available.  Here (at least for now), we make the
;;; assumption that the application programmer is not supposed to call
;;; this function directly, so that it is only called from
;;; FINALIZE-INHERITANCE, and that the application programmer is only
;;; allowed to write methods on it.

(defun compute-default-initargs-default (class)
  (remove-duplicates
   (reduce #'append
           ;; We use the reader DIRECT-DEFAULT-INITARGS rather than
           ;; CLASS-DIRECT-DEFAULT-INITARGS so that this function
           ;; works for built-in classes as well as standard classes.
           ;;
           ;; Furthermore, we use the alternative reader named
           ;; PRECEDENCE-LIST rather than CLASS-PRECEDENCE-LIST,
           ;; because the latter signals an error if the class is not
           ;; finalized.
           (mapcar #'class-direct-default-initargs
                   (precedence-list class)))
   :test #'eq
   :key #'car
   :from-end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FINALIZE-INHERITANCE.
;;;
;;; The AMOP says that class finalization is done in three steps:
;;;
;;;   1. Compute the class precedence list and associate it with the
;;;      class so that it is returned by CLASS-PRECEDENCE-LIST.
;;;
;;;   2. Compute the effective slots and associate them with the
;;;      class, so that they are returned by CLASS-SLOTS.
;;;
;;;   3. Compute the default initargs and associate them with the
;;;      class, so that they are returned by CLASS-DEFAULT-INIARGS.
;;;
;;; The problem with this scenario is that steps 2 and 3 would call
;;; CLASS-PRECEDENCE-LIST to access the class precedence list computed
;;; in step 1.  However, the AMOP also specifies that the generic
;;; function CLASS-PRECEDENCE-LIST signals an error if the class is
;;; not finalized, which it will not be until step 3 is completed!
;;;
;;; The only solution we can see is for steps 2 and 3 to access the
;;; class precedence list using way other than calling the function
;;; CLASS-PRECEDENCE-LIST; either with a different reader, or by using
;;; SLOT-VALUE.  SLOT-VALUE is unappealing because it might be slow,
;;; so we choose the first solution, and define an ACCESSOR called
;;; PRECEDENCE-LIST.
;;;
;;; The AMOP says that ALLOCATE-INSTANCE calls CLASS-FINALIZED-P to
;;; check whether the class is finalized, and if it is not, then it
;;; calls FINALIZE-INHERITANCE.  We use this information to define
;;; FINALIZE-INHERITANCE to always finalize the class, independently
;;; of whether it is already finalized.  In the worst case, some
;;; function other than ALLOCATE-INSTANCE might forget to test whether
;;; the class is finalized before calling FINALIZE-INHERITANCE in
;;; which case nothing bad happens other than a performance
;;; degradation.
;;;
;;; The AMOP does not tell use whether the fact that a class is
;;; finalized implies that all its superclasses are finalized as well,
;;; and it MIGHT make sense to allow that.  On the other hand, all the
;;; steps involved in finalizing a class require basically the same
;;; processing in the superclasses, so we do not allow this, and
;;; instead require all the superclasses of a finalized class to be
;;; finalized as well.

;;; We define this function as a separate abstraction, because during
;;; bootstrapping, it might not necessarily be desirable to call
;;; CLASS-FINALIZED-P, and during bootstrapping, we have full control
;;; over class finalization, so we can make sure it is done so that it
;;; can be assumed that all the superclasses of some class C are all
;;; finalized before an attempt is made to finalize C itself.
(defun finalize-inheritance-assuming-superclasses-finalized (class)
  ;; While we can assume superclasses to be finalized, we can't call
  ;; COMPUTE-CLASS-PRECEDENCE-LIST-ASSUMING-SUPERCLASSES-FINALIZED
  ;; because COMPUTE-CLASS-PRECEDENCE-LIST may have overriding methods
  ;; on it that we are not aware of here.
  (setf (precedence-list class) (compute-class-precedence-list class))
  (let* ((effective-slots (compute-slots class))
         (slot-count
           (count :instance effective-slots
                  :test #'eq :key #'slot-definition-allocation)))
    (setf (instance-size class) slot-count)
    (setf (class-slots class) effective-slots))
  (setf (class-default-initargs class) (compute-default-initargs class))
  ;; We set FINALIZED-P to TRUE before allocating the prototype
  ;; instance, because ALLOCATE-INSTANCE checks that the class is
  ;; finalized and if not, signals an error.
  (setf (class-finalized-p class) t)
  (setf (class-prototype class)
        (allocate-class-prototype class)))

(defun finalize-inheritance-default (class)
  ;; Make sure all the direct superclasses are already finalized.
  ;; Since FINALIZE-INHERITANCE does not itself test whether the class
  ;; is finalized before finalizing it, we do it in this loop so as to
  ;; avoid calling FINALIZE-INHERITANCE unnecessarily.
  (loop for super in (class-direct-superclasses class)
        do (unless (class-finalized-p super)
             (finalize-inheritance super)))
  (finalize-inheritance-assuming-superclasses-finalized class))
