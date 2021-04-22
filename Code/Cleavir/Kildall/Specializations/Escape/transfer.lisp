(in-package #:cleavir-escape)

(defmacro do-all-predecessors ((predname instruction) &body body)
  `(loop for ,predname in (cleavir-ir:predecessors ,instruction)
         do (tagbody ,@body)))

;;; Specific to escape.
(defmacro update (s instruction (&body lists) (&body singles))
  (let ((sinstruction (gensym "INSTRUCTION"))
        (ss (gensym "SPECIALIZATION"))
        (pred (gensym "PREDECESSOR")))
    `(let ((,ss ,s) (,sinstruction ,instruction))
       (with-pool-reader ,ss ,sinstruction from
         (do-all-predecessors (,pred ,sinstruction)
           (copy ,ss ,pred variable from ,lists ,singles))))))

;;; Default method: mark inputs as unknown.
(defmethod transfer ((s escape) instruction)
  (update s instruction
          (((cleavir-ir:inputs instruction)
            (object-meet s (from variable) +unknown+)))
          ()))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:return-instruction))
  (update s instruction
          ()
          (((first (cleavir-ir:inputs instruction)) +returned+))))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:assignment-instruction))
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (update s instruction
            ()
            ((input (from output))))))

(defmethod transfer
    ((s escape)
     (instruction cleavir-ir:fixed-to-multiple-instruction))
  (let ((inputs (cleavir-ir:inputs instruction))
        (output (first (cleavir-ir:outputs instruction))))
    (update s instruction
            ((inputs (from output))) ())))

(defmethod transfer
    ((s escape)
     (instruction cleavir-ir:multiple-to-fixed-instruction))
  (let ((input (first (cleavir-ir:inputs instruction)))
        (outputs (cleavir-ir:outputs instruction)))
    (update s instruction
            ()
            ((input (reduce (lambda (o1 o2) (object-meet s o1 o2))
                            outputs
                            :key #'from
                            ;; M->F with no outputs is possible.
                            :initial-value +none+))))))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:write-cell-instruction))
  (let ((cell (first (cleavir-ir:inputs instruction)))
        (datum (second (cleavir-ir:inputs instruction))))
    (update s instruction
            ()
            ((cell (object-meet s (from variable) +none+))
             (datum (object-meet s (from variable) +unknown+))))))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:read-cell-instruction))
  (let ((cell (first (cleavir-ir:inputs instruction))))
    (update s instruction ()
            ((cell (object-meet s (from variable) +none+))))))

(defmethod transfer
    ((s escape)
     (instruction cleavir-ir:set-symbol-value-instruction))
  (let ((symbol (first (cleavir-ir:inputs instruction)))
        (value (second (cleavir-ir:inputs instruction))))
    (update s instruction ()
            ((symbol (object-meet s (from variable) +none+))
             (value (object-meet s (from variable) +stored+))))))

(macrolet ((defharmless (inst-class)
             `(defmethod transfer
                  ((s escape) (instruction ,inst-class))
                (update s instruction
                        (((cleavir-ir:inputs instruction)
                          (object-meet s (from variable) +none+)))
                        ()))))
  (defharmless cleavir-ir:symbol-value-instruction)
  (defharmless cleavir-ir:fdefinition-instruction)
  (defharmless cleavir-ir:the-values-instruction)
  (defharmless cleavir-ir:the-instruction)
  (defharmless cleavir-ir:phi-instruction)
  (defharmless cleavir-ir:typeq-instruction)
  (defharmless cleavir-ir:eq-instruction))

(cleavir-policy:define-cleavir-policy-quality trust-dynamic-extent
    (member t nil)
  t)

(defmethod transfer
    ((s escape)
     (instruction cleavir-ir:dynamic-allocation-instruction))
  (let ((input (first (cleavir-ir:inputs instruction)))
        (trust-dynamic-extent
          (cleavir-policy:policy-value
           (cleavir-ir:policy instruction)
           'trust-dynamic-extent)))
    (update s instruction ()
            ((input
              (let ((from-val (from variable)))
                (cond ((escapes-p from-val)
                       ;; The dynamic-extent declaration was wrong, so
                       ;; the code is nonconforming.  Full warning.
                       ;; FIXME: Unhelpful without source info.
                       (warn 'incorrect-dynamic-extent
                             :instruction instruction)
                       ;; Ignore the incorrect declaration.
                       from-val)
                      (trust-dynamic-extent
                       ;; We trust DX declarations we can't verify, so
                       ;; remove "unknown" bit.
                       (without-unknown from-val))
                      (t #|no-op|# from-val))))))))

(defun transfer-call (s instruction)
  ;; Calling something gives it +called+.
  ;; Being an argument makes something +unknown+, at least until
  ;; DX information in the environment is preserved (FIXME).
  (let ((callee (first (cleavir-ir:inputs instruction)))
        (arguments (rest (cleavir-ir:inputs instruction))))
    ;; An object being called on itself is special-cased.
    (if (find callee arguments)
        (update s instruction
                ((arguments (object-meet s (from variable)
                                         +unknown+)))
                ((callee (object-meet
                          s (object-meet s (from variable)
                                         +unknown+)
                          +called+))))
        (update s instruction
                ((arguments (object-meet s (from variable)
                                         +unknown+)))
                ((callee (object-meet s (from variable)
                                      +called+)))))))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:funcall-instruction))
  (transfer-call s instruction))
(defmethod transfer
    ((s escape)
     (instruction cleavir-ir:multiple-value-call-instruction))
  (transfer-call s instruction))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:enter-instruction))
  ;;; If we're in an inner function, compute the DXness of it, and
  ;;; then pass to the enclose.
  (let ((enclose (enter-enclose instruction))
        (pool (maybe-instruction-pool s instruction)))
    (when enclose
      ;; For the closed over cells, we rely on the SEGREGATE-LEXICALS
      ;; generated code, i.e.: there is exactly one variable for each
      ;; cell, one FETCH for each variable, and each has an
      ;; immediate-input with the position of that cell in the inputs.
      (let* (fetches info)
        ;; THIS bizarre loop, added after the subsequent one,
        ;; is another kludge on top of that.
        ;; FIXME: Find a good way to determine size of the closure
        ;; from the enter instruction.
        (loop for inst = (first (cleavir-ir:successors instruction))
                then (first (cleavir-ir:successors inst))
              when (typep inst 'cleavir-ir:create-cell-instruction)
                do (setf instruction inst)
              else do (loop-finish))
        ;; This bizarre loop is required by the fact that
        ;; SEGREGATE-LEXICALS will for reasons unknown to me sometimes
        ;; generate ENCLOSE instructions that take N inputs with an
        ;; ENTER that only fetches N-1 cells, which will result in
        ;; their being two fetches but a fetch having an ID of 2.
        ;; Originally we just used the number of fetches for the
        ;; length of the array.
        (loop for inst
                = (first (cleavir-ir:successors instruction))
                  then (first (cleavir-ir:successors inst))
              for id-in = (second (cleavir-ir:inputs inst))
              for out = (first (cleavir-ir:outputs inst))
              while (typep inst 'cleavir-ir:fetch-instruction)
              ;; the cell #, and the variable with the cell.
              do (push (cons (cleavir-ir:value id-in) out) fetches)
              maximizing (cleavir-ir:value id-in) into max-id
              finally (setf info
                            (make-array (1+ max-id)
                                        :element-type 'indicator)))
        (dolist (fetch fetches)
          (destructuring-bind (id . out) fetch
            ;; Note that the cell outputs are not live at the ENTER,
            ;; so if dead variables are excluded this will need
            ;; rewriting.
            (setf (aref info id) (find-in-pool s out pool))))
        ;; Strictly speaking we "should" check that the info is <=,
        ;; but the enclose can do that with its predecessors, and we
        ;; don't need to worry about meets since there's only one
        ;; ENTER for any ENCLOSE.
        (setf (enclose-info enclose) info)))))

(defmethod transfer
    ((s escape) (instruction cleavir-ir:enclose-instruction))
  (let* ((info (enclose-info instruction))
         (pool (maybe-instruction-pool s instruction))
         (closure (first (cleavir-ir:outputs instruction)))
         (cells (cleavir-ir:inputs instruction))
         (closure-dx (find-in-pool s closure pool)))
    (if info
        (update s instruction
                ((cells
                  (object-meet
                   s (from variable)
                   (object-meet
                    s closure-dx
                    (aref info (position variable cells))))))
                ())
        ;; No info, so just use that of the closure.
        (update s instruction
                ((cells
                  (object-meet s (from variable) closure-dx)))
                ()))))
