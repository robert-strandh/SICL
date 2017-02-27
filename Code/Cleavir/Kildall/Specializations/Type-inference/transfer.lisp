(cl:in-package #:cleavir-kildall-type-inference)

(defun find-type (location pool)
  (etypecase location
    ((or cleavir-ir:constant-input cleavir-ir:immediate-input)
     (approximate-type `(eql ,(cleavir-ir:value location))))
    (cleavir-ir:load-time-value-input
     ;; FIXME: obviously nonideal, but i don't want to think about
     ;;  non-eql values and so forth.
     (if (and (cleavir-ir:read-only-p location)
              (consp (cleavir-ir:form location))
              (eq (first (cleavir-ir:form location)) 'quote))
         (approximate-type
          `(eql ,(second (cleavir-ir:form location))))
         (approximate-type 't)))
    (cleavir-ir:lexical-location
     (cleavir-kildall:find-in-pool location pool
                                   (approximate-type 'nil)))
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
            collect (cons out (approximate-type 't))
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
   (find-type (first (cleavir-ir:inputs instruction)) pool)
   (first (cleavir-ir:outputs instruction))
   pool))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:the-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input pool))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor (approximate-type type)))
    (if (top-p type-descriptor)
	pool ; don't bother
        (cleavir-kildall:replace-in-pool
         (binary-meet type-descriptor input-type)
         input pool))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:aref-instruction) pool)
  ;; TODO: update array w/element type
  (let (;(array (first (cleavir-ir:inputs instruction)))
        (index (second (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     output (if (cleavir-ir:boxed-p instruction)
		element-descriptor
		(descriptor-unbox element-descriptor))
     index (binary-meet (find-type index pool)
                        ;; could use array dimensions,
                        ;; or a more proper thing with
                        ;; ARRAY-TOTAL-SIZE
                        (approximate-type 'fixnum)))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:aset-instruction) pool)
  ;; TODO: update array w/element type
  (let ((index (second (cleavir-ir:inputs instruction)))
	(object (third (cleavir-ir:inputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     object (binary-meet
	     (find-type object pool)
	     (if (cleavir-ir:boxed-p instruction)
		 element-descriptor
		 ;; if the array's elements are unboxed, the object
		 ;; being written must be unboxed.
		 (descriptor-unbox element-descriptor)))
     index (binary-meet (find-type index pool)
                        ;; could use array dimensions,
                        ;; or a more proper thing with
                        ;; ARRAY-TOTAL-SIZE
                        (approximate-type 'fixnum)))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:box-instruction) pool)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     input (binary-meet (find-type input pool)
                        (descriptor-unbox element-descriptor))
     output element-descriptor)))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:unbox-instruction) pool)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (cleavir-kildall:pool-subst
     pool
     input (binary-meet (find-type input pool) element-descriptor)
     output (descriptor-unbox element-descriptor))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:the-values-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input pool))
	 (descriptor
	   (approximate-values
	    (cleavir-ir:required-types instruction)
	    (cleavir-ir:optional-types instruction)
	    (cleavir-ir:rest-type instruction))))
    (if (values-top-p descriptor)
	pool
	(cleavir-kildall:replace-in-pool
         (values-binary-meet descriptor input-type) input pool))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:fixed-to-multiple-instruction) pool)
  (let* ((types (mapcar
		 (lambda (input) (find-type input pool))
		 (cleavir-ir:inputs instruction)))
	 (values-type (approximate-values types nil nil))
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
      (loop with vtype = (find-type
                          (first (cleavir-ir:inputs instruction))
                          pool)
            for n from 0
            for output in (cleavir-ir:outputs instruction)
            for bag = (cleavir-kildall:replace-in-pool
                       (values-nth vtype n) output pool)
              then (cleavir-kildall:replace-in-pool
                    (values-nth vtype n) output bag)
            finally (return bag))))

(defmethod cleavir-kildall:compute-function-pool
    ((specialization type-inference)
     enter enter-pool return return-pool)
  (declare (ignore enter-pool)) ; TODO: function lambda lists
  (let ((rvalue (find-type (first (cleavir-ir:inputs return))
                           return-pool)))
    (cleavir-kildall:alist->map-pool
     (acons enter (make-function-descriptor '* rvalue) nil))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:enclose-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (cleavir-kildall:find-in-pool (cleavir-ir:code instruction)
                                 pool
                                 (function-bottom))
   (first (cleavir-ir:outputs instruction))
   pool))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:funcall-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (return-values
    (find-type (first (cleavir-ir:inputs instruction)) pool))
   (first (cleavir-ir:outputs instruction))
   pool))
(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:multiple-value-call-instruction) pool)
  (cleavir-kildall:replace-in-pool
   (return-values
    (find-type (first (cleavir-ir:inputs instruction)) pool))
   (first (cleavir-ir:outputs instruction))
   pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions with two successors.

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:typeq-instruction) pool)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input pool))
	 (type (cleavir-ir:value-type instruction)))
    (multiple-value-bind (type-descriptor canon)
	(canonicalize-type type)
      (if canon
	  (values (cleavir-kildall:replace-in-pool
                   (binary-meet input-type type-descriptor)
                   input pool)
		  (cleavir-kildall:replace-in-pool
                   (difference input-type type-descriptor)
                   input pool))
	  ;; couldn't find a type descriptor, so infer nothing.
	  (values pool pool)))))

(defmethod cleavir-kildall:transfer
    ((s type-inference)
     (instruction cleavir-ir:eq-instruction) pool)
  (let* ((left (first (cleavir-ir:inputs instruction)))
	 (left-type (find-type left pool))
	 (right (second (cleavir-ir:inputs instruction)))
	 (right-type (find-type right pool))
	 (meet (binary-meet left-type right-type)))
    (values
     (cleavir-kildall:pool-subst pool left meet right meet)
     pool)))
