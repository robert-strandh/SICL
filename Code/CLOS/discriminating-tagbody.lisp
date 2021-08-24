(cl:in-package #:sicl-clos)

;;; However, in SICL, we might turn those functions into generic
;;; functions.  It would therefore be best to define simplified
;;; versions of them that only work on lists, and to use those
;;; versions here.

;;; A TRANSFER is similar to a transition.  It has a label and a
;;; target, but the target is a symbol.
(defun make-transfer (label target)
  (cons label target))

(defun transfer-label (transfer)
  (car transfer))

(defun transfer-target (transfer)
  (cdr transfer))

;; An INTERVAL is a sequence of consecutive integers represented as a
;; CONS of two numbers.  The START of an interval is equal to the
;; first integer of the sequence and is represented as the CAR of the
;; CONS cell, and the END of the interval is 1+ of the last integer of
;; the sequence, and is reprented as the CDR of the CONS cell.

(defun make-interval (start end)
  (cons start end))

(defun interval-start (interval)
  (car interval))

(defun interval-end (interval)
  (cdr interval))

(defun (setf interval-end) (new-value interval)
  (setf (cdr interval) new-value))

;; A TRANSFER-GROUP is similar to a transfer.  It has an interval of
;; labels and a target, which is a symbol.

(defun make-transfer-group (interval target)
  (cons interval target))

(defun transfer-group-interval (transfer-group)
  (car transfer-group))

(defun transfer-group-target (transfer-group)
  (cdr transfer-group))

;;; From a list of transfers, compute a list of transfer groups by
;;; grouping together transfers with adjacent labels that have the
;;; same target.
(defun make-transfer-groups (transfers)
  (loop with trcs = (loop for trc in transfers
                          collect (make-transfer (unique-number (transfer-label trc))
                                                 (transfer-target trc)))
        with trs = (sort trcs #'< :key #'transfer-label)
        with first = (car trs)
        with rest = (cdr trs)
        with result = (list (make-transfer-group
                             (make-interval (transfer-label first)
                                            (1+ (transfer-label first)))
                             (transfer-target first)))
        for tr in rest
        do (if (and (eq (transfer-target tr)
                        (transfer-group-target (car result)))
                    (= (transfer-label tr)
                       (interval-end (transfer-group-interval (car result)))))
               (incf (interval-end (transfer-group-interval (car result))))
               (push (make-transfer-group
                      (make-interval (transfer-label tr)
                                     (1+ (transfer-label tr)))
                      (transfer-target tr))
                     result))
        finally (return (reverse result))))

;;; Given a list of transfer groups, compute a nested IF form.  The
;;; argument VAR contains a symbol that is tested against the transfer
;;; labels in the intervals of the transfer groups.  The leaves of the
;;; IF form are all of the form (GO <tag>) where <tag> is the target
;;; symbol of the transfer group, or the symbol which is the value of
;;; the parameter DEFAULT if there is no target for some value of VAR.
;;; The parameters OPEN-INF-P and OPEN-SUP-P indicate whether there is
;;; some action to take for label values that less than the start of
;;; the first interval and greater than or equal to the end of the
;;; last interval respectively.
(defun compute-test-tree (var default transfer-groups open-inf-p open-sup-p)
  (let ((length (length transfer-groups)))
    (if (= length 1)
        (let* ((transfer-group (car transfer-groups))
               (interval (transfer-group-interval transfer-group))
               (start (interval-start interval))
               (end (interval-end interval))
               (target (transfer-group-target transfer-group)))
          (if open-inf-p
              (if open-sup-p
                  (if (= end (1+ start))
                      `(if (cleavir-primop:fixnum-equal ,var ,start)
                           (go ,target)
                           (go ,default))
                      `(if (cleavir-primop:fixnum-less ,var ,start)
                           (go ,default)
                           (if (cleavir-primop:fixnum-less ,var ,end)
                               (go ,target)
                               (go ,default))))
                  `(if (cleavir-primop:fixnum-less ,var ,start)
                       (go ,default)
                       (go ,target)))
              (if open-sup-p
                  `(if (cleavir-primop:fixnum-less ,var ,end)
                       (go ,target)
                       (go ,default))
                  `(go ,target))))
        (let* ((half (floor length 2))
               (left (subseq transfer-groups 0 half))
               (right (subseq transfer-groups half))
               ;; FIXME: these cars and cdrs should be abstracted.
               (open-p (/= (interval-end
                            (transfer-group-interval (car (last left))))
                           (interval-start
                            (transfer-group-interval (car right))))))
          ;; FIXME: these cars and cdrs should be abstracted.
          `(if (cleavir-primop:fixnum-less ,var ,(interval-start (transfer-group-interval (car right))))
               ,(compute-test-tree var default left open-inf-p open-p)
               ,(compute-test-tree var default right nil open-sup-p))))))

;;; ARGUMENT-VAR is the name of a variable containing an argument.
;;; TRANSFERS is a list of transfers (recall that a transfer is a CONS
;;; cell of a label (i.e. a class metaobject) and the name of a
;;; TAGBODY tag).  We generate a tree of nested IF forms, testing VAR
;;; against the labels and generating a GO to the corresponding
;;; TAGBODY tag.  DEFAULT is a symbol indicating a default TAGBODY tag
;;; to transfer control to if the value of VAR is not any of the
;;; labels in TRANSFERS.

;;; Generate a test tree when it is known that the argument value is a
;;; standard object.  Then we can take the stamp of it and
;;; discriminate based on intervals of stamps.
(defun test-tree-from-standard-object-transfers
    (argument-var default transfers)
  (if (null transfers)
      `(go ,default)
      (let ((transfer-groups (make-transfer-groups transfers)))
        ;; T and T might not be optimal for the last two arguments.
        (let ((stamp-var (gensym)))
          `(let ((,stamp-var (cleavir-primop:nook-read ,argument-var 0)))
             ,(compute-test-tree stamp-var default transfer-groups t t))))))

(defparameter *class-name-to-predicate-name*
  `((fixnum . cleavir-primop:fixnump)
    (cons . cleavir-primop:consp)
    (character . cleavir-primop:characterp)
    (single-float . cleavir-primop:single-float-p)))

;;; Generate a test tree when it is known that the argument value is a
;;; non-standard object.  Then we use primop predicates to test what
;;; class it is.
(defun test-tree-from-non-standard-object-transfers
    (argument-var default transfers)
  (loop with tree = `(go ,default)
        for (class . tag) in transfers
        for class-name = (class-name class)
        for predicate-name = (cdr (assoc class-name *class-name-to-predicate-name*))
        do (setf tree `(if (,predicate-name ,argument-var)
                           (go ,tag)
                           ,tree))
        finally (return tree)))

(defun test-tree-from-transfers (argument-var default transfers)
  (let ((standard-object-transfers '())
        (non-standard-object-transfers '()))
    (loop for transfer in transfers
          for transfer-class = (car transfer)
          do (if (or (eq transfer-class (find-class 'fixnum nil))
                     (eq transfer-class (find-class 'cons nil))
                     (eq transfer-class (find-class 'character nil))
                     (eq transfer-class (find-class 'single-float nil)))
                 (push transfer non-standard-object-transfers)
                 (push transfer standard-object-transfers)))
    `(if (cleavir-primop:standard-object-p ,argument-var)
         ,(test-tree-from-standard-object-transfers
           argument-var default standard-object-transfers)
         ,(test-tree-from-non-standard-object-transfers
           argument-var default non-standard-object-transfers))))

(defun test-trees-from-internal-layer-info (var default layer-info)
  (loop for state-info in layer-info
        collect (car state-info)
        collect (test-tree-from-transfers var default (cdr state-info))))

(defun actions-from-final-layer-info (layer-info)
  (loop for state-info in layer-info
        append (list (car state-info) (cdr state-info))))

;;; Create a TAGBODY form that implements the initial part of a
;;; discriminating function.  TRANSITION-INFO is the transition
;;; information extracted from a discriminating automaton.
;;; ARGUMENT-VARS is a list of variables containing the specialized
;;; required arguments to the generic function.
(defun compute-discriminating-tagbody (transition-info argument-vars)
  (let ((default (gensym)))
    `(tagbody
        ,@(append
           (loop for layer-info in (butlast transition-info)
                 for var in argument-vars
                 append (test-trees-from-internal-layer-info
                         var default layer-info))
           (actions-from-final-layer-info (car (last transition-info))))
        ,default)))
