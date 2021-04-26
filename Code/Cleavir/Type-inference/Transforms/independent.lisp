(in-package #:cleavir-typed-transforms)

;;;; Transforms in this file don't actually use type-inference, but
;;;; they're closely related. They could potentially be moved.

;;; Remove all THE and THE-VALUES instructions.
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

;;; Make some HIR that will signal a type error, that DATUM
;;;  (a lexical location) is not of type EXPECTED.
;;; The instructions will be self-contained and have the right
;;;  predecessor, but changing the predecessor's successor must be
;;;  done elsewhere. (Because you're probably change-class-ing it.)
(defun make-type-error (predecessor datum expected)
  ;; FIXME: programmatic HIR could probably be made nicer.
  (let* ((cleavir-ir:*policy* (cleavir-ir:policy predecessor))
	 (fdef (cleavir-ir:new-temporary))
	 (fdefinition-instruction
	   (cleavir-ir:make-fdefinition-instruction
	    (cleavir-ir:make-load-time-value-input ''cl:error)
	    fdef))
	 (fnr-instruction
	   (cleavir-ir:make-funcall-instruction
	    (list fdef
		  (cleavir-ir:make-load-time-value-input ''cl:type-error)
		  (cleavir-ir:make-load-time-value-input '':datum)
		  datum
		  (cleavir-ir:make-load-time-value-input '':expected-type)
		  (cleavir-ir:make-load-time-value-input
		   `',expected))
            (list (cleavir-ir:make-values-location))
            (cleavir-ir:make-unreachable-instruction))))
    (cleavir-ir:insert-instruction-before fdefinition-instruction
					  fnr-instruction)
    (setf (cleavir-ir:predecessors fdefinition-instruction)
	  (list predecessor))
    fdefinition-instruction))

;;; TODO: SBCL has a middle setting on its insert-type-checks that
;;; downgrades types into easily checkable ones like FIXNUM.
;;; So a sanity check but not wholly correct. Might be nice.
(cleavir-policy:define-cleavir-policy-quality
    insert-type-checks (member t nil) t)

;;; Maybe convert a THE instruction into a TYPEQ instruction,
;;; depending on policy
(defun the->typeq (the-instruction)
  (let ((policy (cleavir-ir:policy the-instruction)))
    (when (cleavir-policy:policy-value policy 'insert-type-checks)
      (change-class
       the-instruction 'cleavir-ir:typeq-instruction
       :successors (list
		    (first
		     (cleavir-ir:successors the-instruction))
		    (make-type-error
		     the-instruction
		     (first (cleavir-ir:inputs the-instruction))
		     (cleavir-ir:value-type the-instruction)))))))

;;; Convert all of them. intended for safe code, before type
;;; inference.
(defun thes->typeqs (initial)
  (let (thes)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-instruction)
	 (push i thes)))
     initial)
    (mapc #'the->typeq thes)
    ;; We only added some branches, so we don't need to reinitialize
    ;; etc.
    initial))
