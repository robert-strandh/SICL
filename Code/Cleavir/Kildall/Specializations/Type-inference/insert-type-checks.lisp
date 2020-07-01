(in-package #:cleavir-kildall-type-inference)

;;; First this thing that just deletes THE and THE-VALUES to make
;;; things easier for lower compiler levels. Should be run after
;;; type inference is complete.

(defun delete-the (initial-instruction)
  (let (death)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i '(or cleavir-ir:the-instruction
                        cleavir-ir:the-values-instruction))
         (push i death)))
     initial-instruction)
    (mapc #'cleavir-ir:delete-instruction death))
  initial-instruction)

;;;; This bit is currently very inadequate, in that it will lose a
;;;; lot of information in the name of safety, and insert unneeded
;;;; checks. A better strategy for insertion would be to convert
;;;; all THEs into checks, and then remove every such check that
;;;; doesn't surround a typeq that was already there that can be
;;;; removed due to redundancy. But that's quite complicated.

;;; TODO: SBCL has a middle setting on its insert-type-checks that
;;; downgrades types into easily checkable ones like FIXNUM.
;;; So a sanity check but not wholly correct. Might be nice.
(cleavir-policy:define-cleavir-policy-quality
    insert-type-checks boolean t)

;;; Given a type specifier, return a new
;;; specifier suitable for TYPEP, i.e. "for discrimination" in CLHS
;;; terms. The only standard types not for discrimination are
;;; (function ...) and types including them, but an implementation
;;; could add more.
(defgeneric discriminator (env typespec))

;; for convenience we just assume symbols and classes are okay.
(defmethod discriminator (env typespec)
  (let ((typespec (cleavir-env:type-expand env typespec)))
    (etypecase typespec
      (symbol typespec)
      (cons (discriminator-compound
             env (first typespec) typespec))
      (class typespec))))

;;; Like the above but for specializing on compound types.
(defgeneric discriminator-compound (env head typespec))

(defmethod discriminator-compound (env (head (eql 'and)) typespec)
  (let ((discriminators
          (mapcar (lambda (ty) (discriminator env ty))
                  (rest typespec))))
    (if (every #'eql (rest typespec) discriminators)
        typespec
        `(and ,@discriminators))))

(defmethod discriminator-compound (env (head (eql 'or)) typespec)
  (let ((discriminators
          (mapcar (lambda (ty) (discriminator env ty))
                  (rest typespec))))
    (if (every #'eql (rest typespec) discriminators)
        typespec
        `(or ,@discriminators))))

(defmethod discriminator-compound (env (head (eql 'not)) typespec)
  (let ((inner (discriminator env (second typespec))))
    (if (eql inner (second typespec))
        typespec
        `(not ,inner))))

(defmethod discriminator-compound (env (head (eql 'cons)) typespec)
  (destructuring-bind (&optional (car '*) (cdr '*)) typespec
    (let ((car-d (if (eql car '*) '* (discriminator env car)))
          (cdr-d (if (eql cdr '*) '* (discriminator env cdr))))
      (if (and (eql car-d car) (eql cdr-d cdr))
          typespec
          `(cons ,car-d ,cdr-d)))))

(macrolet ((easy (head)
             `(defmethod discriminator-compound
                  (env (head (eql ',head)) typespec)
                (declare (ignore env))
                typespec))
           (easies (&rest heads)
             `(progn ,@(loop for head in heads
                             collecting `(easy ,head)))))
  (easies base-string bit-vector complex double-float eql float
          integer long-float member mod rational real satisfies
          short-float signed-byte simple-base-string
          simple-bit-vector simple-string simple-vector
          single-float string unsigned-byte))

(defmethod discriminator-compound (env (head (eql 'array)) typespec)
  (discriminate-array env typespec))
(defmethod discriminator-compound (env (head (eql 'simple-array))
                                   typespec)
  (discriminate-array env typespec))
(defmethod discriminator-compound (env (head (eql 'vector)) typespec)
  (discriminate-array env typespec))

(defun discriminate-array (env typespec)
  (destructuring-bind (head &optional (element-type '*) (dims '*))
      typespec
    ;; we sort of assume that uaet of a (function ...) won't be a
    ;; (function ...).
    (if (eq element-type '*)
        typespec
        (let ((uaet
                (upgraded-array-element-type element-type env)))
          (if (eql uaet element-type)
              typespec
              `(,head ,uaet ,dims))))))

(defmethod discriminator-compound (env (head (eql 'function))
                                   typespec)
  (declare (ignore env))
  ;; the point of all of this.
  'function)

;; make some HIR that will signal a type error, that DATUM
;;  (a lexical location) is not of type EXPECTED.
;; The instructions will be self-contained and have the right
;;  predecessor, but changing the predecessor's successor must be
;;  done elsewhere.
(defun make-type-error (predecessor datum expected)
  ;; FIXME: programmatic HIR could probably be made nicer.
  (let* ((cleavir-ir:*policy* (cleavir-ir:policy predecessor))
         (cleavir-ir:*dynamic-environment*
           (cleavir-ir:dynamic-environment predecessor))
	 (fdef (cleavir-ir:new-temporary))
	 (fdefinition-instruction
	   (cleavir-ir:make-fdefinition-instruction
	    (cleavir-ir:make-load-time-value-input ''cl:error)
	    fdef))
	 (fnr-instruction
	   (cleavir-ir:make-funcall-no-return-instruction
	    (list fdef
		  (cleavir-ir:make-load-time-value-input ''cl:type-error)
		  (cleavir-ir:make-load-time-value-input '':datum)
		  datum
		  (cleavir-ir:make-load-time-value-input '':expected-type)
		  (cleavir-ir:make-load-time-value-input
		   `',expected)))))
    ;; Update/initialize data.
    (push fdefinition-instruction (cleavir-ir:defining-instructions fdef))
    (push fnr-instruction (cleavir-ir:using-instructions fdef))
    (push fnr-instruction (cleavir-ir:using-instructions datum))
    (cleavir-ir:insert-instruction-before fdefinition-instruction
					  fnr-instruction)
    (setf (cleavir-ir:predecessors fdefinition-instruction)
	  (list predecessor))
    fdefinition-instruction))

;;; maybe convert a THE instruction into a TYPEQ instruction,
;;; depending on policy
(defun the->typeq (the-instruction env)
  (let ((policy (cleavir-ir:policy the-instruction)))
    (when (cleavir-policy:policy-value policy 'insert-type-checks)
      (let ((type (discriminator
                   env
                   (cleavir-ir:value-type the-instruction))))
        (change-class
         the-instruction 'cleavir-ir:typeq-instruction
         :value-type type
         :successors (list
                      (first
                       (cleavir-ir:successors the-instruction))
                      (make-type-error
                       the-instruction
                       (first (cleavir-ir:inputs the-instruction))
                       type)))))))

;;; THE-VALUES checks aren't used because we can't check them very
;;; much (since we can't count values with HIR as it exists) and
;;; because the-values is only generated in values contexts, i.e.
;;; it never goes the-values to M->F, so typeqing the required
;;; values wouldn't help anyway.
;;; So we just delete THE-VALUES if insert-type-checks is on.

(defun the-values->typeqs (the-values-instruction env)
  (declare (ignore env))
  (let ((policy (cleavir-ir:policy the-values-instruction)))
    (when (cleavir-policy:policy-value policy 'insert-type-checks)
      (cleavir-ir:delete-instruction the-values-instruction))))

;;; FIXME: There should be a halfway point where checks are inserted
;;; but the original type information is preserved. This is unsafe
;;; (if we can't check all declarations) but only a little.
(defun thes->typeqs (initial-instruction env)
  (let (thes tvs)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (cond ((typep i 'cleavir-ir:the-instruction)
              (push i thes))
             ((typep i 'cleavir-ir:the-values-instruction)
              (push i tvs))))
     initial-instruction)
    (mapc (lambda (i) (the->typeq i env)) thes)
    (mapc (lambda (i) (the-values->typeqs i env)) tvs)))
