(in-package #:cleavir-kildall-escape)

;;; KLUDGE: Values locations are sometimes shared between functions
;;; (when there's a nonlocal exit into a values context). This
;;; analysis relies on knowing all the users of outputs, so we have
;;; a problem. Might also come up with functions that don't return?
;;; Anyway, so we patch over this by calling anything we don't know
;;; users of unknown when the output is a values location.
;;; When it's lexical we use the normal one.
(defun find-values-in-pool (location pool)
  (cleavir-kildall:find-in-pool location pool +unknown+))

;;; default method: pass it along, marking inputs as unknown.
(defmethod cleavir-kildall:transfer ((s escape) instruction pool)
  (cleavir-kildall:pool-meet s
   pool
   (cleavir-kildall:alist->map-pool
    (loop for input in (cleavir-ir:inputs instruction)
          when (cleavir-ir:variable-p input)
            collect (cons input +unknown+)))))

;;; return returns.
(defmethod cleavir-kildall:transfer
    ((s escape) (instruction cleavir-ir:return-instruction) pool)
  (declare (ignore pool)) ; it's empty anyway.
  (cleavir-kildall:alist->map-pool
   (list (cons (first (cleavir-ir:inputs instruction)) +returned+))))

;;; Assignment methods override, rather than merge.
(defmethod cleavir-kildall:transfer
    ((s escape) (instruction cleavir-ir:assignment-instruction)
     pool)
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (if (cleavir-ir:variable-p input)
        (cleavir-kildall:replace-in-pool
         (cleavir-kildall:find-in-pool output pool) input pool)
        pool)))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:fixed-to-multiple-instruction)
     pool)
  (loop with output-info
          = (find-values-in-pool
             (first (cleavir-ir:outputs instruction))
             pool)
        for input in (cleavir-ir:inputs instruction)
        when (cleavir-ir:variable-p input)
          do (setf pool (cleavir-kildall:replace-in-pool
                         output-info input pool)))
  pool)

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:multiple-to-fixed-instruction)
     pool)
  (cleavir-kildall:replace-in-pool
   (reduce (lambda (o1 o2)
             (cleavir-kildall:object-meet s o1 o2))
           (cleavir-ir:outputs instruction)
           :key (lambda (location)
                  (cleavir-kildall:find-in-pool location pool))
           ;; M->F with no outputs are possible.
           :initial-value +none+)
   (first (cleavir-ir:inputs instruction))
   pool))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:write-cell-instruction)
     pool)
  (cleavir-kildall:pool-meet s
    pool
    (cleavir-kildall:alist->map-pool
     (list (cons (first (cleavir-ir:inputs instruction))
                 ;; writing only side-effects the cell...
                 ;; (but we do have to merge it, so that the pool is
                 ;;  complete (FIXME: you sure?))
                 +none+)
           (cons (second (cleavir-ir:inputs instruction))
                 ;; ...but has mysterious consequences for the value
                 +unknown+)))))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:read-cell-instruction)
     pool)
  (cleavir-kildall:pool-meet s
    pool
    (cleavir-kildall:alist->map-pool                        
     (list (cons (first (cleavir-ir:inputs instruction)) +none+)))))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:set-symbol-value-instruction)
     pool)
  (cleavir-kildall:pool-meet s
   pool
   ;; FIXME: better way to test cleavir-ir:variable-pness.
   (cleavir-kildall:alist->map-pool
    (loop for input in (cleavir-ir:inputs instruction)
          ;; symbol is none (who cares though), object is global
          for value in (list +none+ +stored+)
          when (cleavir-ir:variable-p input)
            collect (cons input value)))))

(macrolet ((defharmless (inst-class)
             `(defmethod cleavir-kildall:transfer
                  ((s escape) (instruction ,inst-class) pool)
                (harmless-pool s pool instruction))))
  (flet ((harmless-pool (s pool instruction)
           (cleavir-kildall:pool-meet s
            pool
            (cleavir-kildall:alist->map-pool
             (loop for i in (cleavir-ir:inputs instruction)
                   when (cleavir-ir:variable-p i)
                     collect (cons i +none+))))))
    (defharmless cleavir-ir:symbol-value-instruction)
    (defharmless cleavir-ir:fdefinition-instruction)
    (defharmless cleavir-ir:the-values-instruction)
    (defharmless cleavir-ir:the-instruction)
    ;; CAR and CDR aren't harmless, since conses can contain
    ;; themselves. But we also can't copy the output to the input,
    ;; since we don't actually know (car x) = x, it's unknown.
    (defharmless cleavir-ir:phi-instruction)
    (defharmless cleavir-ir:typeq-instruction)
    (defharmless cleavir-ir:eq-instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy TRUST-DYNAMIC-EXTENT
;;;
;;; If this is T, then if Cleavir cannot prove the dynamic-extentness
;;; of a variable, but it is declared DYNAMIC-EXTENT, it will mark
;;; use that information to mark things as dynamically allocatable.
;;; This is UNSAFE and improper declarations can cause memory faults.
;;; If this is NIL, Cleavir will only mark things it can prove are
;;; dynamically allocatable as such.
;;; In either case, if Cleavir can prove something is NOT dynamically
;;; allocatable, but it is declared as DYNAMIC-EXTENT, it will warn.

(cleavir-policy:define-cleavir-policy-quality
    trust-dynamic-extent (member t nil) t)

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:dynamic-allocation-instruction)
     pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (dx (cleavir-kildall:find-in-pool input pool +none+))
         (policy (cleavir-ir:policy instruction)))
    (cond ((escapes-p dx)
           ;; The dynamic-extent declaration was wrong, so the
           ;; code is nonconforming. Full warning.
           ;; FIXME: Unhelpful without source information, though.
           (warn 'incorrect-dynamic-extent
                 :instruction instruction)
           ;; Ignore the incorrect declaration.
           pool)
          ((cleavir-policy:policy-value policy
                                        'trust-dynamic-extent)
           ;; We trust DX declarations we can't verify, which means
           ;; removing the "unknown" bit.
           (cleavir-kildall:replace-in-pool
            (without-unknown dx) input pool))
          (t ; don't trust anything, so
           pool))))

(defun transfer-call (specialization instruction pool)
  (cleavir-kildall:pool-meet specialization
   pool
   ;; calling is calling, but (for now) being an argument dooms.
   ;; this stupid thing is so that (f #'f) properly dooms.
   (let ((callee (first (cleavir-ir:inputs instruction)))
         (arguments (rest (cleavir-ir:inputs instruction))))
     (cleavir-kildall:alist->map-pool
      (if (find callee arguments)
          (loop for input in arguments
                when (cleavir-ir:variable-p input)
                  collect (cons input +unknown+))
          (list* (cons callee +called+)
                 (loop for input in arguments
                       when (cleavir-ir:variable-p input)
                         collect (cons input +unknown+))))))))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:funcall-instruction)
     pool)
  (transfer-call s instruction pool))
(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:funcall-no-return-instruction)
     pool)
  (transfer-call s instruction pool))
(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:multiple-value-call-instruction)
     pool)
  (transfer-call s instruction pool))

;;; infos are arrays. the nth element is what happens to the nth
;;; closed-over cell. in the future it could track arguments too,
;;; but using that information requires a forward analysis.
(defmethod cleavir-kildall:compute-function-pool
    ((specialization escape) enter return)
  (declare (ignore return))
  ;; for the closed over cells, we rely on the segregate-lexicals
  ;; generated code, i.e.: there is exactly one variable for each
  ;; cell, one FETCH for each variable, and each has an
  ;; immediate-input with the position of that cell in the inputs.
  (let ((fetches (loop for inst
                         = (first (cleavir-ir:successors enter))
                           then (first (cleavir-ir:successors inst))
                       while (typep inst 'cleavir-ir:fetch-instruction)
                       collect (cons
                                inst
                                (instruction-pool inst *dictionary*))))
        (info (make-array (length fetches)
                          :element-type 'indicator)))
    (dolist (fetch fetches)
      (destructuring-bind (inst . pool) fetch
        (let ((id (cleavir-ir:value (second (cleavir-ir:inputs inst))))
              (out (first (cleavir-ir:outputs inst))))
          (setf (aref info id) (cleavir-kildall:find-in-pool
                                out pool)))))
    (cleavir-kildall:alist->map-pool
     (acons enter info nil))))

(defmethod cleavir-kildall:transfer
    ((s escape) (instruction cleavir-ir:enclose-instruction) pool)
  ;; a cell can be DXd if it is only input to DX closures that DX
  ;; their cells.
  ;; FIXME: this leads to cells getting the "call" bit.
  (let* ((info (cleavir-kildall:find-in-pool
                (cleavir-ir:code instruction)
                pool
                ;; default: assume the best, and it will be revised
                ;; down later.
                (make-array (length (cleavir-ir:inputs instruction))
                            :element-type 'indicator
                            :initial-element +none+)))
         (closure (first (cleavir-ir:outputs instruction)))
         (closure-dx (cleavir-kildall:find-in-pool closure pool)))
    (declare (type (simple-array indicator (*)) info))
    (cleavir-kildall:pool-meet s
      (cleavir-kildall:alist->map-pool
       (loop for indicator across info
             for input in (cleavir-ir:inputs instruction)
             collecting (cons input
                              (indicator-union closure-dx indicator))))
      pool)))
