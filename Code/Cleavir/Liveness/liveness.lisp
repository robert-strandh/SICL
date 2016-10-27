(cl:in-package #:cleavir-liveness)

;;;; Liveness analysis.
;;;;
;;;; Each instruction I in a flow chart representing some program
;;;; defines a PROGRAM POINT, which is located IMMEDIATELY BEFORE I.
;;;; A variable V is LIVE at some program point P if and only if there
;;;; exists an execution path from P to the end of the flow chart on
;;;; which V is used before it is defined.
;;;;
;;;; Computing liveness is a dataflow problem that is best computed
;;;; from the end of the program to the beginning of the program.  Let
;;;; I be some instruction with successors S1, S2, ... Sn.  And also
;;;; let B(Si) be the set of live variables at the program point
;;;; defined by Si, for 1 <= i <= n.  To compute the set B(I) of live
;;;; variables of the program point defined by I, first compute A(I)
;;;; as the union of every B(Si).  Then intersect A(I) with the
;;;; variables written by I, and finally compute the union of the
;;;; resulting set with the variables used by I.
;;;;
;;;; Initially the sets A(I) and B(I) for each node are empty.  The
;;;; computation is iterated until a fixpoint is reached.
;;;;
;;;; The literature suggests using bitvectors for this computation,
;;;; where a particular item has a unique index in the bitvector.
;;;; However, to avoid storing the unique indices in the items, we
;;;; would have to maintain a hash table and a vector for the mapping
;;;; from items to indices and from indices to items respectively.
;;;; Instead we attempt to maintain the sets as Lisp lists.  If
;;;; performance becomes a problem, we might revisit this decision.

(defclass liveness ()
  ((%btable :initform (make-hash-table :test #'eq) :reader btable)
   (%atable :initform (make-hash-table :test #'eq) :reader atable)))

(defun set-equal (set1 set2)
  (and (subsetp set1 set2 :test #'eq) (subsetp set2 set1 :test #'eq)))

(defparameter *liveness-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "LIVENESS-METER"))

;;; The return value is an instance of the class LIVENESS.  This
;;; return value should be considered to be an opaque object, only to
;;; be used as the first argument of the functions LIVE-BEFORE and
;;; LIVE-AFTER.
(defun liveness (start-node)
  (cleavir-meter:with-meter (m *liveness-meter*)
    (let ((liveness (make-instance 'liveness))
          stack)
      (flet ((variable-p (input)
               (or (typep input 'cleavir-ir:lexical-location)
                   (typep input 'cleavir-ir:values-location)))
             (successor-btable (successor)
               (gethash successor (btable liveness))))
        (cleavir-ir:map-instructions-arbitrary-order
         (lambda (instruction)
           (cleavir-meter:increment-size m)
           (when (null (cleavir-ir:successors instruction))
             (push instruction stack)))
         start-node)
        ;; traverse iteratively
        (do ((node #1=(pop stack) #1#))
            ((null node) liveness)
          ;; Compute the union of the items that are live before each
          ;; of the successors of NODE.
          (let ((live (reduce #'union
                              (mapcar #'successor-btable
                                      (cleavir-ir:successors node))
                              :initial-value nil)))
            (multiple-value-bind (current present-p)
                (gethash node (atable liveness))
              (unless (and present-p (set-equal live current))
                ;; Something has changed.  Propagate!
                (setf
                 (gethash node (atable liveness)) live
                 ;; Remove from the set the items that are
                 ;; written by NODE.
                 live (set-difference live (cleavir-ir:outputs node))
                 ;; Add to the set the items used by NODE that
                 ;; are registers or lexical locations.
                 live (union live (remove-if-not #'variable-p
                                                 (cleavir-ir:inputs node)))
                 (gethash node (btable liveness)) live
                 stack (append stack (cleavir-ir:predecessors node)))))))))))

(defun live-before (liveness node)
  (gethash node (btable liveness)))

(defun live-after (liveness node)
  (gethash node (atable liveness)))

;;  LocalWords:  liveness dataflow fixpoint bitvector bitvectors
