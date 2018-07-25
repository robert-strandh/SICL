(cl:in-package #:cleavir-ir-visualizer)

;;; This function takes an ENTER-INSTRUCTION and finds the longest
;;; simple path from the enter instruction.  The algorithm is probably
;;; not very efficient, but we count on our graphs not to be very
;;; complex, so it should be fine.
;;;
;;; We simply do a depth first search from the ENTER-INSTRUCTION.  For
;;; each successor of some instruction, we do a recursive depth-first
;;; search for each successor and compare the lengths of the paths
;;; that each search generates.
(defun find-longest-simple-path (enter-instruction)
  (let (;; Define a hash table that holds instructions that are on the
        ;; current path we are exploring.
        (table (make-hash-table :test #'eq)))
    (labels 
        ((aux (instruction)
           (if (gethash instruction table)
               ;; We reached an instruction that is already on the
               ;; current path that we are exploring, so we return the
               ;; empty path.
               '()
               (loop initially (setf (gethash instruction table) t)
                     with longest-path = '()
                     for successor in (cleavir-ir:successors instruction)
                     for path = (aux successor)
                     when (> (length path) (length longest-path))
                       do (setf longest-path path)
                     finally (progn (setf (gethash instruction table) nil)
                                    (return (cons instruction longest-path)))))))
      (aux enter-instruction))))

