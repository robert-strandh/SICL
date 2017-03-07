(cl:in-package #:cleavir-kildall-type-inference)

(defun find-type (s location pool)
  (etypecase location
    ((or cleavir-ir:constant-input cleavir-ir:immediate-input)
     (approximate-type s `(eql ,(cleavir-ir:value location))))
    (cleavir-ir:load-time-value-input
     ;; FIXME: obviously nonideal, but i don't want to think about
     ;;  non-eql values and so forth.
     (if (and (cleavir-ir:read-only-p location)
              (consp (cleavir-ir:form location))
              (eq (first (cleavir-ir:form location)) 'quote))
         (approximate-type s
          `(eql ,(second (cleavir-ir:form location))))
         (approximate-type s 't)))
    (cleavir-ir:lexical-location
     (cleavir-kildall:find-in-pool location pool
                                   (approximate-type s 'nil)))
    (cleavir-ir:values-location
     (cleavir-kildall:find-in-pool location pool
                                   (values-bottom)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method. We need to ensure that all outputs have some
;;; non-bottom type or the arc will be dead, so with no assumptions
;;; we get T.

(defmethod cleavir-kildall:transfer
    ((s type-inference) instruction pool)
  (cleavir-kildall:pool-meet s
   pool
   (cleavir-kildall:alist->map-pool
    (loop for out in (cleavir-ir:outputs instruction)
          if (typep out 'cleavir-ir:lexical-location)
            collect (cons out (approximate-type s 't))
          else if (typep out 'cleavir-ir:values-location)
                 collect (cons out (values-top))
          else do (error "unknown output class")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions with one successor.

;;; For the assignment instruction, type information about the input
;;; is propagated to the output.
(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:assignment-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (find-type s (first (cleavir-ir:inputs instruction)) pool)
   (first (cleavir-ir:outputs instruction))
   pool))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:the-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type s input pool))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor (approximate-type s type)))
    (if (top-p s type-descriptor)
	pool ; don't bother
        (cleavir-kildall:replace-in-pool
         (binary-meet s type-descriptor input-type)
         input pool))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:aref-instruction) pool)
  ;; TODO: update array w/element type
  (let (;(array (first (cleavir-ir:inputs instruction)))
        (index (second (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type s
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     output (if (cleavir-ir:boxed-p instruction)
		element-descriptor
		(descriptor-unbox element-descriptor))
     index (binary-meet s
                        (find-type s index pool)
                        ;; could use array dimensions,
                        ;; or a more proper thing with
                        ;; ARRAY-TOTAL-SIZE
                        (approximate-type s 'fixnum)))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:aset-instruction) pool)
  ;; TODO: update array w/element type
  (let ((index (second (cleavir-ir:inputs instruction)))
	(object (third (cleavir-ir:inputs instruction)))
	(element-descriptor
	  (approximate-type s
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     object (binary-meet
             s
	     (find-type s object pool)
	     (if (cleavir-ir:boxed-p instruction)
		 element-descriptor
		 ;; if the array's elements are unboxed, the object
		 ;; being written must be unboxed.
		 (descriptor-unbox element-descriptor)))
     index (binary-meet s
                        (find-type s index pool)
                        ;; could use array dimensions,
                        ;; or a more proper thing with
                        ;; ARRAY-TOTAL-SIZE
                        (approximate-type s 'fixnum)))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:box-instruction) pool)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type s
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     input (binary-meet s (find-type s input pool)
                            (descriptor-unbox element-descriptor))
     output element-descriptor)))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:unbox-instruction) pool)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type s
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     input (binary-meet s (find-type s input pool)
                            element-descriptor)
     output (descriptor-unbox element-descriptor))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:the-values-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type s input pool))
	 (descriptor
	   (approximate-values s
	    (cleavir-ir:required-types instruction)
	    (cleavir-ir:optional-types instruction)
	    (cleavir-ir:rest-type instruction))))
    (if (values-top-p s descriptor)
	pool
	(cleavir-kildall:replace-in-pool
         (values-binary-meet s descriptor input-type)
         input pool))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:fixed-to-multiple-instruction) pool)
  (let* ((types (mapcar
		 (lambda (input) (find-type s input pool))
		 (cleavir-ir:inputs instruction)))
	 (values-type (make-values s types nil (bottom s)))
	 (output (first (cleavir-ir:outputs instruction))))
    (cleavir-kildall:replace-in-pool values-type output pool)))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:multiple-to-fixed-instruction)
     pool)
  (if (null (cleavir-ir:outputs instruction))
      ;; It seems weird, but this kind of instruction is sometimes
      ;; generated, and we need to ensure it doesn't just return
      ;; NIL from that loop.
      pool
      (loop with vtype = (find-type s
                          (first (cleavir-ir:inputs instruction))
                          pool)
            for n from 0
            for output in (cleavir-ir:outputs instruction)
            for bag = (cleavir-kildall:replace-in-pool
                       (values-nth s vtype n) output pool)
              then (cleavir-kildall:replace-in-pool
                    (values-nth s vtype n) output bag)
            finally (return bag))))

(defmethod cleavir-kildall:compute-function-pool
    ((s type-inference) enter enter-pool return return-pool)
  (declare (ignore enter-pool)) ; TODO: function lambda lists
  (let ((rvalue (find-type s (first (cleavir-ir:inputs return))
                           return-pool)))
    (cleavir-kildall:alist->map-pool
     (acons enter (make-function-descriptor '* rvalue)
            nil))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:enclose-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (cleavir-kildall:find-in-pool
    (cleavir-ir:code instruction) pool
    (make-function-descriptor '* (values-bottom)))
   (first (cleavir-ir:outputs instruction))
   pool))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:funcall-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (return-values
    (find-type s (first (cleavir-ir:inputs instruction)) pool))
   (first (cleavir-ir:outputs instruction))
   pool))
(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:multiple-value-call-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (return-values
    (find-type s (first (cleavir-ir:inputs instruction)) pool))
   (first (cleavir-ir:outputs instruction))
   pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions with two successors.

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:typeq-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type s input pool))
	 (type (cleavir-ir:value-type instruction))
         (left-descriptor (approximate-type s type))
         (right-descriptor (approximate-type s `(not ,type))))
    (values (cleavir-kildall:replace-in-pool
             (binary-meet s input-type left-descriptor)
             input pool)
            (cleavir-kildall:replace-in-pool
             (binary-meet s input-type right-descriptor)
             input pool))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:eq-instruction) pool)
  (let* ((left (first (cleavir-ir:inputs instruction)))
	 (left-type (find-type s left pool))
	 (right (second (cleavir-ir:inputs instruction)))
	 (right-type (find-type s right pool))
	 (meet (binary-meet s left-type right-type)))
    (values
     (cleavir-kildall:pool-subst pool left meet right meet)
     pool)))
