(in-package #:cleavir-typed-transforms)

;;;; Transforms in this file don't actually use type-inference, but
;;;; they're closely related. They could potentially be moved.

;;; remove all THE and THE-VALUES instructions.
;;; This can be performed after a thes->typeqs in safe code to
;;; eliminate straggling the-values (might change later),
;;; or in unsafe code.
(defun delete-the (initial)
  (let (death-row)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i '(or cleavir-ir:the-instruction
			cleavir-ir:the-values-instruction))
	 (push i death-row)))
     initial)
    (mapc #'cleavir-ir:delete-instruction death-row))
  initial)

;;; TODO: SBCL has a middle setting on its insert-type-checks that
;;; downgrades types into easily checkable ones like FIXNUM.
;;; So a sanity check but not wholly correct. Might be nice.
(cleavir-policy:define-cleavir-policy-quality
    insert-type-checks (member t nil) t)

;;; maybe convert a THE instruction into a TYPEQ instruction,
;;; depending on policy
(defun the->typeq (the-instruction)
  (let ((policy (cleavir-ir:policy the-instruction)))
    (when (cleavir-policy:policy-value policy 'insert-type-checks)
      ;; FIXME: programmatic HIR could probably be made nicer.
      (let* ((cleavir-ir:*policy* policy) ; copy from base instr
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
		      (first (cleavir-ir:inputs the-instruction))
		      (cleavir-ir:make-load-time-value-input '':expected-type)
		      (cleavir-ir:make-load-time-value-input
		       `',(cleavir-ir:value-type the-instruction))))))
	(cleavir-ir:insert-instruction-before fdefinition-instruction
					      fnr-instruction)
	(setf (cleavir-ir:predecessors fdefinition-instruction)
	      (list the-instruction))
	(change-class
	 the-instruction 'cleavir-ir:typeq-instruction
	 :successors (list (first
			    (cleavir-ir:successors the-instruction))
			   fdefinition-instruction))))))

;; convert all of them. intended for safe code, before type inference.
(defun thes->typeqs (initial)
  (let (thes)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-instruction)
	 (push i thes)))
     initial)
    (mapc #'the->typeq thes)
    ;; we only added some branches, so we don't need to reinitialize etc.
    initial))
