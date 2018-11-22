(cl:in-package #:sicl-clos)

;;; The algorithm implemented here is taken directly from this section
;;; in "Common Lisp, the Language, second edition":
;;; http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node274.html

;;; We extract the topological sorting code to a separate file that
;;; does not refer to any CLOS elements directly.  That way, this code
;;; can be executed by the host at bootstrapping time.  We are just
;;; given an object, presumably a class, and a function of one
;;; argument that takes that object and returns a list of other object
;;; (presumably classes) that are considered the direct superclasses
;;; of that class.  From that, we compute a class precedence list
;;; according to the rules specified in the section cited above.

;;; Recall from the section cited above that a "local precedence
;;; order" is a total ordering of a class C and its direct
;;; superclasses.  It takes the form (C C1 C2 ... Cn) where C is the
;;; class itself and C1, C2, ..., Cn are its direct superclasses in
;;; the order specified in the DEFCLASS form.  Notice that some class
;;; may be included in the local precedence order that is not
;;; explicitly mentioned in the DEFCLASS form.  In particular, if the
;;; metaclass is STANDARD-CLASS, then the class STANDARD-OBJECT is
;;; automatically a direct superclass, even though it is not
;;; explicitly mentioned in the DEFCLASS form.

;;; We define a "precedence relation" to be a set (represented as a
;;; list) if pairs (Ca . Cb).  Each pair encodes the "must precede"
;;; relation between two classes, Ca and Cb.  A "local precedence
;;; relation" is a precedence relation determined from the local
;;; precedence order as follows: Let (C C1 C2 ... Cn) be the local
;;; precedence order of the class C.  The local precedence relation is
;;; then computed as: ((C . C1) (C1 . C2) ... (Cn-1 . Cn))

;;; We define the "total precedence relation" of some class C to be
;;; the union of the local precedence relation of C and the local
;;; precedence relation of each of its (direct or indirect)
;;; superclasses.

;;; Given a local precedence order, compute the corresponding local
;;; precedence relation.
(defun compute-local-precedence-relation (local-precedence-order)
  (loop for (ca cb) on local-precedence-order
        until (null cb)
        collect (cons ca cb)))

;;; From a list of local precedence relations, compute the total
;;; precedence relation.
(defun compute-total-precedence-relation (local-precedence-relations)
  (reduce #'append local-precedence-relations :from-end t))

;;; Given a total set of classes Sc (represented as a list) and a
;;; precedence relation R, return a set (represented as a list) of
;;; classes in Sc that have no predecessors according R.  In other
;;; words, find every C in Sc such that there is no element (D . C) in
;;; R.
(defun find-candidates (classes precedence-relation)
  (loop for class in classes
        unless (find class precedence-relation :key #'cdr :test #'eq)
          collect class))

;;; Given a class C and a precedence relation R, return a new
;;; precedence relation that has all the elements of R except the
;;; elements of type (C . D).
(defun filter-precedence-relation (class precedence-relation)
  (remove class precedence-relation :key #'car :test #'eq))

;;; Given a class C, a class D, and a set (represented as a list) of
;;; local precedence orders, return true if and only if D is a direct
;;; superclass of C.
(defun direct-superclass-p
    (class putative-direct-superclass local-precedence-orders)
  (loop for local-precedence-order in local-precedence-orders
        thereis (and (eq (first local-precedence-order) class)
                     (member putative-direct-superclass
                             (rest local-precedence-order)
                             :test #'eq))))

;;; The following function is used to make a deterministic choice
;;; among several possible candidate classes for the next inclusion in
;;; the class precedence list.  CANDIDATES is a set (represented as a
;;; list) of the possible candidates.  REVERSE-PARTIAL-PRECEDENCE-LIST
;;; is the precedence list computed so far, except that the elements
;;; are in reverse order.  LOCAL-PRECEDENCE-ORDERS is a set
;;; (represented as a list) of all the local precedence orders that we
;;; use to determine whether some class is a direct subclass of some
;;; other class.  We find the first element E of the list
;;; REVERSE-PARTIAL-PRECEDENCE-LIST such that there is a candidate C
;;; such that C is a direct superclass of E.  We then return E.
(defun choose-between-candidates
    (candidates reverse-partial-precedence-list local-precedence-orders)
  (loop for putative-direct-superclass in reverse-partial-precedence-list
        thereis (loop for candidate in candidates
                      when (direct-superclass-p
                            candidate
                            putative-direct-superclass
                            local-precedence-orders)
                        return candidate
                      finally (return nil))))

;;; Given a set (represented as a list) of local precedence orders of
;;; a class C and all its superclasses, compute the class precedence
;;; list of C.  This function is named COMPUTE-PRECEDENCE-LIST to
;;; distinguish it from the MOP standard function
;;; COMPUTE-CLASS-PRECEDENCE-LIST.  If no precedence list can be
;;; computed, return NIL.
(defun compute-precedence-list (local-precedence-orders)
  (let* ((all-classes (mapcar #'first local-precedence-orders))
         (remaining-classes all-classes)
         (local-precedence-relations
           (mapcar #'compute-local-precedence-relation local-precedence-orders))
         (total-precedence-relation
           (compute-total-precedence-relation local-precedence-relations))
         (precedence-relation total-precedence-relation)
         (reverse-partial-precedence-list '()))
    (loop until (null total-precedence-relation)
          for candidates = (find-candidates remaining-classes precedence-relation)
          until (null candidates)
          do (let ((candidate (if (null (rest candidates))
                                  (first candidates)
                                  (choose-between-candidates
                                   candidates
                                   reverse-partial-precedence-list
                                   local-precedence-orders))))
               (push candidate reverse-partial-precedence-list)
               (setf remaining-classes
                     (remove candidate remaining-classes :test #'eq))
               (setf precedence-relation
                     (filter-precedence-relation candidate precedence-relation)))
          finally (return (if (null precedence-relation)
                              (reverse reverse-partial-precedence-list)
                              nil)))))

;;; Given a class C and a function for computing its direct
;;; superclasses, return a set (represented as a list) of the local
;;; precedence orders of C and all its superclasses.
(defun compute-local-precedence-orders (class direct-superclasses-function)
  (let ((remaining-classes (list class))
        (local-precedence-orders '()))
    (loop until (null remaining-classes)
          do (let* ((class (pop remaining-classes))
                    (direct-superclasses
                      (funcall direct-superclasses-function class)))
               (push (cons class direct-superclasses)
                     local-precedence-orders)
               (loop for superclass in direct-superclasses
                     unless (member superclass local-precedence-orders
                                    :test #'eq :key #'car)
                     do (push superclass remaining-classes))))
    local-precedence-orders))

;;; Given a class C and a function for computing its direct
;;; superclasses, compute the class precedence list of C, or return
;;; NIL if no precedence list can be computed.
(defun compute-precedence-list-of-class (class direct-superclasses-function)
  (compute-precedence-list
   (compute-local-precedence-orders class direct-superclasses-function)))
