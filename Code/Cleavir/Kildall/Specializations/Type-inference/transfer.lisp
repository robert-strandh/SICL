(cl:in-package #:cleavir-kildall-type-inference)

(defmethod cleavir-kildall:find-in-pool
    ((s type-inference) (location cleavir-ir:constant-input) pool)
  (declare (ignore pool))
  (approximate-type s `(eql ,(cleavir-ir:value location))))

(defmethod cleavir-kildall:find-in-pool
    ((s type-inference) (location cleavir-ir:immediate-input) pool)
  (declare (ignore pool))
  (approximate-type s `(eql ,(cleavir-ir:value location))))

(defmethod cleavir-kildall:find-in-pool
    ((s type-inference) (location cleavir-ir:load-time-value-input)
     pool)
  (declare (ignore pool))
  ;; FIXME: obviously nonideal, but i don't want to think about
  ;;  non-eql values and so forth.
  (if (and (cleavir-ir:read-only-p location)
           (consp (cleavir-ir:form location))
           (eq (first (cleavir-ir:form location)) 'quote))
      (approximate-type
       s `(eql ,(second (cleavir-ir:form location))))
      (approximate-type s 't)))

(defmethod cleavir-kildall:object1
    ((s type-inference) (v cleavir-ir:lexical-location))
  (approximate-type s 'nil))

(defmethod cleavir-kildall:object1
    ((s type-inference) (v cleavir-ir:values-location))
  (values-bottom))

;;; Default: Assume nothing (T) of the outputs.
(defmethod cleavir-kildall:transfer
    ((s type-inference) instruction)
  (let ((outputs (cleavir-ir:outputs instruction)))
    (cleavir-kildall:with-pool-reader s instruction from
      (dolist (succ (cleavir-ir:successors instruction))
        (cleavir-kildall:copy s succ variable from
              ((outputs (typecase variable
                          (cleavir-ir:lexical-location
                           (approximate-type s 't))
                          (cleavir-ir:values-location
                           (values-top))
                          (t (error "unknown output class")))))
              ())))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:assignment-instruction))
  (cleavir-kildall:with-pool-reader s instruction from
    (cleavir-kildall:copy
     s (first (cleavir-ir:successors instruction)) var from
     ()
     (((first (cleavir-ir:outputs instruction))
       (from (first (cleavir-ir:inputs instruction))))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference) (instruction cleavir-ir:the-instruction))
  (let* ((succ (first (cleavir-ir:successors instruction)))
         (input (first (cleavir-ir:inputs instruction)))
         (type (cleavir-ir:value-type instruction))
         (type-descriptor (approximate-type s type)))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
            ()
            ((input
              (binary-meet s (from variable) type-descriptor)))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference) (instruction cleavir-ir:aref-instruction))
  (let* ((succ (first (cleavir-ir:successors instruction)))
         (array (first (cleavir-ir:inputs instruction)))
         (index (second (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction)))
         (et (cleavir-ir:element-type instruction))
         (array-descriptor (approximate-type s `(array ,et)))
         ;; could use array dimensions or array-dimension-limit?
         (index-descriptor (approximate-type s 'fixnum))
         (element-pre (approximate-type s et))
         (element-descriptor (if (cleavir-ir:boxed-p instruction)
                                 element-pre
                                 (descriptor-unbox element-pre))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
            ()
            ((array (binary-meet s (from variable)
                                 array-descriptor))
             (index (binary-meet s (from variable)
                                 index-descriptor))
             (output element-descriptor))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference) (instruction cleavir-ir:aset-instruction))
  (destructuring-bind (array index object)
      (cleavir-ir:inputs instruction)
    (let* ((succ (first (cleavir-ir:successors instruction)))
           (et (cleavir-ir:element-type instruction))
           (array-desc (approximate-type s `(array ,et)))
           (index-desc (approximate-type s 'fixnum))
           (element-pre (approximate-type s et))
           (element-desc
             (if (cleavir-ir:boxed-p instruction)
                 element-pre
                 ;; if the array's elements are unboxed, the object
		 ;; being output must be unboxed.
                 (descriptor-unbox element-pre))))
      (cleavir-kildall:with-pool-reader s instruction from
        (cleavir-kildall:copy s succ variable from
           ()
           ((array (binary-meet s (from variable) array-desc))
            (index (binary-meet s (from variable) index-desc))
            (object
             (binary-meet s (from variable) element-desc))))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference) (instruction cleavir-ir:box-instruction))
  (let*((succ (first (cleavir-ir:successors instruction)))
        (input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction)))
        (et (cleavir-ir:element-type instruction))
        (output-desc (approximate-type s et))
        (input-desc (descriptor-unbox output-desc)))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
        ()
        ((input (binary-meet s (from variable) input-desc))
         (output (binary-meet s (from variable) output-desc)))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference) (instruction cleavir-ir:unbox-instruction))
  (let* ((succ (first (cleavir-ir:successors instruction)))
         (input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction)))
         (et (cleavir-ir:element-type instruction))
         (input-desc (approximate-type s et))
         (output-desc (descriptor-unbox input-desc)))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
        ()
        ((input (binary-meet s (from variable) input-desc))
         (output (binary-meet s (from variable) output-desc)))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:the-values-instruction))
  (let ((succ (first (cleavir-ir:successors instruction)))
        (input (first (cleavir-ir:inputs instruction)))
        (descriptor
          (approximate-values s
	    (cleavir-ir:required-types instruction)
	    (cleavir-ir:optional-types instruction)
	    (cleavir-ir:rest-type instruction))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
        ()
        ((input (values-binary-meet s (from variable)
                                    descriptor)))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:fixed-to-multiple-instruction))
  (cleavir-kildall:with-pool-reader s instruction from
    (let* ((succ (first (cleavir-ir:successors instruction)))
           (output (first (cleavir-ir:outputs instruction)))
           (descs (mapcar #'from (cleavir-ir:inputs instruction)))
           (values-desc (make-values s descs nil (bottom s))))
      (cleavir-kildall:copy s succ variable from
                            ()
                            ((output values-desc))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:multiple-to-fixed-instruction))
  (cleavir-kildall:with-pool-reader s instruction from
    (let ((succ (first (cleavir-ir:successors instruction)))
          (outputs (cleavir-ir:outputs instruction))
          (vtype (from (first (cleavir-ir:inputs instruction)))))
      (cleavir-kildall:copy s succ variable from
        ((outputs
          (values-nth s vtype (position variable outputs))))
        ()))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:return-instruction))
  (let ((enclose (cleavir-kildall:enter-enclose instruction)))
    (when enclose ; no enclose if it's the outermost
      (cleavir-kildall:with-pool-reader s instruction from
        (setf (cleavir-kildall:enclose-info enclose)
              (make-function-descriptor
               '* (from (first (cleavir-ir:inputs instruction))))))
      ;; We don't test for pool<= and all, but everything should
      ;; work out as long as functions have at most one return
      ;; instruction each.
      (cleavir-kildall:add-work enclose))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:enclose-instruction))
  (cleavir-kildall:with-pool-reader s instruction from
    (let ((succ (first (cleavir-ir:successors instruction)))
          (out (first (cleavir-ir:outputs instruction)))
          (info
            (or (cleavir-kildall:enclose-info instruction)
                ;; If we don't have info yet, assume the 1east
                ;; function type, (function * nil) presently.
                (make-function-descriptor '* (values-bottom)))))
      (cleavir-kildall:copy s succ variable from
                            () ((out info))))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:funcall-instruction))
  (let ((succ (first (cleavir-ir:successors instruction)))
        (callee (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
        ()
        ((output (return-values (from callee))))))))
(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:multiple-value-call-instruction))
  (let ((succ (first (cleavir-ir:successors instruction)))
        (callee (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
        ()
        ((output (return-values (from callee))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions with two successors.

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:typeq-instruction))
  (let* ((left-succ (first (cleavir-ir:successors instruction)))
         (right-succ (second (cleavir-ir:successors instruction)))
         (input (first (cleavir-ir:inputs instruction)))
         (type (cleavir-ir:value-type instruction))
         (left-desc (approximate-type s type))
         (right-desc (approximate-type s `(not ,type))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s left-succ variable from
                            ()
                            ((input (binary-meet s (from variable)
                                                 left-desc))))
      (cleavir-kildall:copy s right-succ variable from
                            ()
                            ((input (binary-meet s (from variable)
                                                 right-desc)))))))
