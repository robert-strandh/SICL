;;; THIS FILE IS NOT CURRENTLY INCORPORATED INTO ANY CLEAVIR SYSTEM

(defpackage #:cleavir-kildall-redundant
  (:use #:cl)
  (:export #:eliminate-superfluous-temporaries))

(in-package #:cleavir-kildall-redundant)

(defclass redundant-traverse
    (cleavir-kildall:forward-spread-traverse
     cleavir-kildall:map-pool-mixin)
  ())

;;;; NOTE TO FIXME: Seriously use union find or whatever.
;;;; Pools are maps from inputs to entries of form
;;;; (parent &rest children). Parent and children are inputs,
;;;; or parent can be NIL, if there is no parent.

;;; kildall methods

(defmethod cleavir-kildall:entry-pool ((s redundant-traverse) instruction)
  (check-type instruction cleavir-ir:enter-instruction)
  nil)

(defmethod cleavir-kildall:pool-meet ((s redundant-traverse) p1 p2)
  ;; like map-pool's meet, but object-meet needs the pools.
  (loop with result = (copy-alist p2)
        for pair in p1
        for (location1 . parent1) = pair
        for a = (assoc location1 result)
        when a
          do (setf (cdr a)
                   ;; if there's no overlap, it's just NIL.
                   (loop named outer
                         for left = (cdr a)
                           then (cdr (assoc left result))
                         while left
                         do (loop for right = parent1
                                    then (cdr (assoc right p1))
                                  while right
                                  when (eq left right)
                                    do (return-from outer left))))
        else do (push pair result)
        finally (return result)))

;;; also not efficient, but better than kildall, probably.
(defmethod cleavir-kildall:pool<= ((s redundant-traverse) p1 p2)
  (every (lambda (pair2)
           (let ((pair1 (assoc (car pair2) p1)))
             (and pair1 ; p1 has every variable p2 does
                  ;; and each parent is somewhere in p1's parents
                  ;; This is the most common case, and handles
                  ;; cdr = nil as well.
                  (or (eq (cdr pair1) (cdr pair2))
                      (loop with target = (cdr pair2)
                            for parent = (cdr pair1)
                              then (cdr (assoc parent p1))
                            while parent
                              thereis (eq parent target))))))
         p2))

(defun new-output (output new-parent pool)
  (let* ((a (assoc output pool)) ; maybe nil, so parent is nil. OK
         (parent (cdr a)))
    (mapcar (lambda (pair)
              ;; maybe replace parent
              (if (eq (cdr pair) output)
                  (cons (car pair) parent)
                  pair))
            pool)
    (if a
        ;; reassignment
        (setf (cdr a) new-parent)
        ;; new output, so add
        (setf pool (acons output new-parent pool)))
    pool))

(defmethod cleavir-kildall:transfer ((s redundant-traverse) i pool)
  ;; Add any constant inputs that aren't already in a class.
  (dolist (in (cleavir-ir:inputs i))
    (unless (or (cleavir-ir:variable-p in)
		(assoc in pool))
      (setf pool (acons in nil pool))))
  ;; For each output: Put it newly in with no parents. Any other
  ;; variable that previously had it as a parent should have its
  ;; parent as well.
  (dolist (out (cleavir-ir:outputs i))
    ;; ignore values locations (which cannot be assigned)
    (when (typep out 'cleavir-ir:lexical-location)
      (setf pool (new-output out nil pool))))
  pool)

(defmethod cleavir-kildall:transfer
    ((s redundant-traverse)
     (instruction cleavir-ir:assignment-instruction)
     pool)
  ;; Add any constant inputs that aren't already in a class.
  (dolist (in (cleavir-ir:inputs instruction))
    (unless (or (cleavir-ir:variable-p in)
		(assoc in pool))
      (setf pool (acons in nil pool))))
  ;; assign the output.
  (let ((in (first (cleavir-ir:inputs instruction)))
	(out (first (cleavir-ir:outputs instruction))))
    (new-output out in pool)))

(defun redundancies (initial-instruction)
  (let ((traverse (make-instance 'redundant-traverse)))
    (cleavir-kildall:kildall traverse initial-instruction)))

;;; find the earliest equivalent thing usable to replace the input.
(defun rfind (input pool)
  (let ((a (assoc input pool)))
    (if a
        (if (cdr a) ; more ancient input
            (rfind (cdr a) pool)
            ;; no parent: we are the oldest
            input)
        (error "redundancy fuckup"))))

;;; Change the inputs of all instructions to use the earliest
;;; equivalent input.
(defun reassign (initial-instruction redundancies)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop with pool = (gethash instruction redundancies)
	   for input-cons on (cleavir-ir:inputs instruction)
	   for input = (car input-cons)
           ;; again, ignore values locations
           when (typep input 'cleavir-ir:lexical-location)
	   do (setf (car input-cons) (rfind input pool))))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))

;;; Could be expanded to remove all non-side-effectful instructions
;;; that output to no-longer-used locations, but I don't think that
;;; situation actually arises.
(defun remove-assignments (initial-instruction)
  (let (death)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and
	      (typep instruction 'cleavir-ir:assignment-instruction)
	      (null (cleavir-ir:using-instructions (first (cleavir-ir:outputs instruction)))))
	 (push instruction death)))
     initial-instruction)
    (mapc #'cleavir-ir:delete-instruction death))
  nil)

(defun eliminate-superfluous-temporaries (initial-instruction)
  (reassign initial-instruction (redundancies initial-instruction))
  (remove-assignments initial-instruction)
  initial-instruction)
