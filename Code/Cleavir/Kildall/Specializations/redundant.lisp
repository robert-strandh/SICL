;;; THIS FILE IS NOT CURRENTLY INCORPORATED INTO ANY CLEAVIR SYSTEM

(defpackage #:cleavir-redundant
  (:use #:cl)
  (:export #:eliminate-superfluous-temporaries))

(in-package #:cleavir-redundant)

(defclass redundancy
    (cleavir-kildall:iterate-mixin
     cleavir-kildall:start-enter-mixin)
  ())

;;; Kildall methods.

(defun chase (location pool)
  (let ((pair (assoc location pool)))
    (if pair
        (chase (cdr pair) pool)
        location)))

(defmethod cleavir-kildall:transfer ((s redundancy) instruction)
  (let ((from-pool (cleavir-kildall:dictionary-pool instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (dolist (succ (cleavir-ir:successors instruction))
      (cond
        ((cleavir-kildall:pool-present-p s succ)
         (loop with update = nil
               with dest-pool
                 = (cleavir-kildall:dictionary-pool succ)
               for dest-pair in dest-pool
               do (let* ((key (car dest-pair))
                         (dest-value (cdr dest-pair))
                         (from-value
                           (if (find key outputs)
                               nil
                               (cdr (assoc key from-pool)))))
                    (unless (eq dest-value from-value)
                      (setf (cdr dest-pair) nil update t)))
               finally (when update
                         (cleavir-kildall:add-work succ))))
        (t (setf (cleavir-kildall:dictionary-pool succ)
                 (loop for (key . value) in from-pool
                       unless (or (find key outputs)
                                  (find value outputs))
                         collect (cons key value)))
           (cleavir-kildall:add-work succ))))))

(defmethod cleavir-kildall:transfer
    ((s redundancy)
     (instruction cleavir-ir:assignment-instruction))
  (let ((from-pool (cleavir-kildall:dictionary-pool instruction))
        (input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (dolist (succ (cleavir-ir:successors instruction))
      (cond
        ((cleavir-kildall:pool-present-p s succ)
         (loop with update = nil
               with dest-pool
                 = (cleavir-kildall:dictionary-pool succ)
               for dest-pair in dest-pool
               do (let* ((key (car dest-pair))
                         (dest-value (cdr dest-pair))
                         (from-value
                           (if (eq key output)
                               (chase input from-pool)
                               (cdr (assoc key from-pool)))))
                    (unless (eq dest-value from-value)
                      (setf (cdr dest-pair) nil update t)))
               finally (when update
                         (cleavir-kildall:add-work succ))))
        (t (setf (cleavir-kildall:dictionary-pool succ)
                 (acons output (chase input from-pool)
                        (loop for (key . value) in from-pool
                              unless (or (eq key output)
                                         (eq value output))
                                collect (cons key value))))
           (cleavir-kildall:add-work succ))))))

(defun redundancies (initial-instruction)
  (let ((traverse (make-instance 'redundancy)))
    (cleavir-kildall:kildall traverse initial-instruction)))

;;; Change the inputs of all instructions to use the earliest
;;; equivalent input.
(defun reassign (initial-instruction redundancies)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop with pool = (gethash instruction redundancies)
           for input-cons on (cleavir-ir:inputs instruction)
           for input = (car input-cons)
           ;; Again, ignore values locations.
           when (typep input 'cleavir-ir:lexical-location)
             do (let ((pair (assoc input pool)))
                  (when (and pair (cdr pair))
                    (setf (car input-cons) (cdr pair))))))
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
