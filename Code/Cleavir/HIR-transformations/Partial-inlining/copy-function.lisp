(cl:in-package #:cleavir-partial-inlining)

;;; More restrained versions that don't make new outputs,
;;; since we're only doing this to catch locations that have ALREADY been copied.
;;; (I.e., closed over variables)
(defun translate-output-for-copy (output mapping)
  (or (find-in-mapping mapping output) output))
(defun translate-outputs-for-copy (outputs mapping)
  (loop for output in outputs
        collecting (translate-output-for-copy output mapping)))

;;; Returns a copy of a HIR function.
;;; This is required because an internal function could have things it's mapped
;;; over refer to inputs that were copied by inlining.

;;; TO-COPY is an enter instruction representing the function being copied.
;;; MAPPING is from inlining.

;;; The copy of TO-COPY is returned.
(defun copy-function (to-copy mapping)
  (let (;; used to fix everything up in the second step.
        (copies nil))
    ;; First loop: Copy everything, but don't hook up predecessors and successors.
    (loop with worklist = (list to-copy)
          until (null worklist)
          do (block next-loop
               (let ((instruction (pop worklist)))
                 (unless (find-in-mapping mapping instruction)
                   (typecase instruction
                     (cleavir-ir:enclose-instruction
                      (push (cleavir-ir:code instruction) worklist)))
                   (setf worklist
                         (append (cleavir-ir:successors instruction) worklist))
                   ;; now actually copy.
                   (let* ((inputs (cleavir-ir:inputs instruction))
                          (outputs (cleavir-ir:outputs instruction))
                          (new-inputs (translate-inputs inputs mapping))
                          (new-outputs
                            (translate-outputs-for-copy outputs mapping))
                          (copy (reinitialize-instance
                                 (cleavir-ir:clone-instruction instruction)
                                 :inputs new-inputs :outputs new-outputs)))
                     (push copy copies)
                     (add-to-mapping mapping instruction copy))))))
    ;; Second loop: hook up the predecessors and successors.
    (flet ((maybe-replace (instruction)
             (let ((copy (find-in-mapping mapping instruction)))
               (or copy instruction))))
      (loop for copy in copies
            ;; hook up CODEs first.
            when (typep copy 'cleavir-ir:enclose-instruction)
              do (setf (cleavir-ir:code copy)
                       (maybe-replace (cleavir-ir:code copy)))
            do (setf (cleavir-ir:predecessors copy)
                     (mapcar #'maybe-replace (cleavir-ir:predecessors copy))
                     (cleavir-ir:successors copy)
                     (mapcar #'maybe-replace (cleavir-ir:successors copy))))))
  (find-in-mapping mapping to-copy))
