(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS-LOCALLY
;;;
;;; Given an ENTER-INSTRUCTION, maps, depth first, all instructions
;;; it owns, i.e. all instructions reachable without return or
;;; unwind or descending into a call.

(defun map-instructions-locally (function enter-instruction)
  (check-type enter-instruction enter-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
        (instructions-to-process '()))
    (flet ((register-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (push instruction instructions-to-process))))
      (register-if-unvisited enter-instruction)
      (loop until (null instructions-to-process)
            do (let ((instruction (pop instructions-to-process)))
                 (funcall function instruction)
                 (unless (typep instruction 'unwind-instruction)
                   ;; we don't own the successor of unwinds.
                   (mapc #'register-if-unvisited
                         (successors instruction))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FILTER-INSTRUCTIONS-LOCALLY
;;;
;;; As FILTER-INSTRUCTIONS, but only operating on locally owned
;;; instructions.

(defun filter-instructions-locally (predicate enter-instruction)
  (let (result)
    (map-instructions-locally
     (lambda (instruction)
       (when (funcall predicate instruction)
         (push instruction result)))
     enter-instruction)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function LOCAL-INSTRUCTIONS-OF-TYPE
;;;
;;; As INSTRUCTIONS-OF-TYPE but with only local instructions.

(defun local-instructions-of-type (enter-instruction type)
  (filter-instructions-locally (lambda (i) (typep i type))
                               enter-instruction))
