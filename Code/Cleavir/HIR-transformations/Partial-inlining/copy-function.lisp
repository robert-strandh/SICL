(cl:in-package #:cleavir-partial-inlining)

(defvar *new-enter*)

(defun ensure-copy (mapping datum new-owner)
  (or (find-in-mapping mapping datum)
      (let ((new (typecase datum
                   (cleavir-ir:lexical-location
                    (cleavir-ir:make-lexical-location
                     (cleavir-ir:name datum)))
                   (t
                    ;; return immediately to avoid mapping consequences
                    (return-from ensure-copy datum)))))
        (add-to-mapping mapping datum new)
        (setf (location-owner new) new-owner)
        new)))

(defun translate-input-for-copy (input external-map internal-map stack)
  (let ((owner (location-owner input)))
    (cond ((member owner stack)
           ;; Originally I expected that the input would always be in the map,
           ;; but this does not seem to be the case, for reasons I'm not sure of.
           ;; I think the ownership test should make it be correct anyway though.
           ;; FIXME: Maybe remove input-output distinction here then.
           (ensure-copy internal-map input
                        (find-in-mapping *instruction-mapping* owner)))
          ((eq owner *original-enter-instruction*)
           (ensure-copy external-map input *target-enter-instruction*))
          (t input))))
(defun translate-inputs-for-copy (inputs external-map internal-map stack)
  (loop for input in inputs
        collect (translate-input-for-copy input external-map internal-map stack)))

(defun translate-output-for-copy (output external-map internal-map stack)
  (let ((owner (location-owner output)))
    (cond ((member owner stack)
           (ensure-copy internal-map output
                        (find-in-mapping *instruction-mapping* owner)))
          ((eq owner *original-enter-instruction*)
           (ensure-copy external-map output *target-enter-instruction*))
          (t output))))
(defun translate-outputs-for-copy (outputs external-map internal-map stack)
  (loop for output in outputs
        collect (translate-output-for-copy output external-map internal-map stack)))

;;; EXTERNAL-MAP is the mapping from inline.
;;; INTERNAL-MAP is a fresh mapping which is used only for internal locations.
;;; STACK is a list of ENTER-INSTRUCTIONS that we're in the middle of copying.
;;; If an output is owned by something in the stack, it ought to be copied,
;;; but if not, it's something closed over and must not be copied, unless it's
;;; owned by the function being inlined.
(defun copy-function-recur (enter enclose old-enter external-map internal-map stack)
  (let* ((copies nil)
         (stack (cons enter stack))
         (*new-enter* (cleavir-ir:clone-instruction enter))
         ;; Hook up the new enclose and enter instructions into the DAG.
         (dag-nodes (cleavir-hir-transformations:dag-nodes *function-dag*))
         (new-node (cleavir-hir-transformations:add-enclose-to-parents
                    enclose
                    (gethash old-enter dag-nodes))))
    (setf (gethash *new-enter* dag-nodes) (list new-node))
    (disconnect-predecessor *new-enter*)
    (push *new-enter* copies)
    ;; Set up ownership.
    (setf (instruction-owner *new-enter*) *new-enter*)
    ;; Put it in the mapping.
    (add-to-mapping *instruction-mapping* enter *new-enter*)
    ;; We set the outputs after building like this so that (a) they have their ownership correct,
    ;; and (b) (setf cleavir-ir:outputs) synchronizes the lambda list properly.
    ;; We still copy the outputs in clone-instruction above so that the (setf outputs) method
    ;; knows how to do the substitution.
    (setf (cleavir-ir:outputs *new-enter*)
          (translate-outputs-for-copy (cleavir-ir:outputs enter)
                                      external-map internal-map stack))
    ;; Make sure the dynamic environment of the enter is correct as well.
    (setf (cleavir-ir:dynamic-environment-location *new-enter*)
          ;; This output will have been translated same as the others.
          (cleavir-ir:dynamic-environment-output *new-enter*))
    ;; First loop: Copy all instructions in the function, but leave
    ;; predecessors and successors disconnected.
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (unless (eq instruction enter) ; FIXME: Ugly.
         (let* ((inputs (cleavir-ir:inputs instruction))
                (outputs (cleavir-ir:outputs instruction))
                (dynamic-environment (cleavir-ir:dynamic-environment-location instruction))
                (new-inputs (translate-inputs-for-copy inputs external-map internal-map stack))
                (new-outputs (translate-outputs-for-copy outputs external-map internal-map stack))
                (new-dynamic-environment
                  (translate-input-for-copy dynamic-environment external-map internal-map stack))
                (copy (cleavir-ir:clone-instruction instruction
                        :inputs new-inputs :outputs new-outputs
                        :dynamic-environment-location new-dynamic-environment)))
           (typecase instruction
             ((or cleavir-ir:enclose-instruction cleavir-ir:funcall-instruction)
              (pushnew instruction *destinies-worklist*)
              (pushnew copy *destinies-worklist*)))
           (disconnect-predecessor copy)
           (push copy copies)
           (setf (instruction-owner copy) *new-enter*)
           (add-to-mapping *instruction-mapping* instruction copy))))
     enter)
    ;; Second loop: Loop over the copies doing hookups.
    (flet ((maybe-replace (instruction)
             (let ((copy (find-in-mapping *instruction-mapping* instruction)))
               (or copy instruction))))
      (loop for copy in copies
            do (setf (cleavir-ir:predecessors copy)
                     (mapcar #'maybe-replace (cleavir-ir:predecessors copy))
                     (cleavir-ir:successors copy)
                     (mapcar #'maybe-replace (cleavir-ir:successors copy)))
            do (typecase copy
                 (cleavir-ir:enclose-instruction
                  ;; We have to do this in the second loop so that any
                  ;; UNWINDS in the inner function can be hooked up to copies.
                  (setf (cleavir-ir:code copy)
                        (copy-function-recur
                         (cleavir-ir:code copy) copy *new-enter* external-map internal-map stack)))
                 (cleavir-ir:unwind-instruction
                  (setf (cleavir-ir:destination copy)
                        (maybe-replace (cleavir-ir:destination copy)))))))
    *new-enter*))

;;; Returns a copy of a HIR function.
;;; This is required because an internal function could have things it's mapped
;;; over refer to inputs that were copied by inlining.
;;; ENTER is an enter instruction representing the function being copied.
;;; ENCLOSE is the enclosing instruction.
;;; MAPPING is the lexical variable mapping used during this inline.
;;; The copy of ENTER is returned.
(defun copy-function (enter enclose mapping)
  (copy-function-recur enter enclose *target-enter-instruction* mapping (make-hash-table :test #'eq) nil))
