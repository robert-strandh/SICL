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

;; make some HIR that will signal a type error, that DATUM
;;  (a lexical location) is not of type EXPECTED.
;; The instructions will be self-contained and have the right
;;  predecessor, but changing the predecessor's successor must be
;;  done elsewhere. (Because you're probably change-class-ing it.)
(defun make-type-error (predecessor datum expected)
  ;; FIXME: programmatic HIR could probably be made nicer.
  (let* ((cleavir-ir:*policy* (cleavir-ir:policy predecessor))
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

;;; maybe convert a THE instruction into a TYPEQ instruction,
;;; depending on policy
(defun the->typeq (the-instruction)
  (change-class
   the-instruction 'cleavir-ir:typeq-instruction
   :successors (list (first
		      (cleavir-ir:successors the-instruction))
		     (make-type-error
		      the-instruction
		      (first (cleavir-ir:inputs the-instruction))
		      (cleavir-ir:value-type the-instruction)))))

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

;; convert FUNCALL to FUNCALL-NO-RETURN where possible
;; if SAFE-P, cause a type error if they do return
;; (which will still cut it off from successors. good, yes?)
(defun mark-noreturns (initial safe-p)
  ;; this doesn't actually use inferred type information.
  ;; it COULD, but i expect that calling a variable function that
  ;;  must not return is not super common.
  ;; the main value of this transform is to chop impossible
  ;;  execution paths; we want to do this as early as possible to
  ;;  improve the performance of other analyses.
  ;; there could be another stage after type inference to do it for
  ;;  variable functions if necessary.
  (let (thevalueses)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-values-instruction)
	 (push i thevalueses)))
     initial)
    (dolist (i thevalueses)
      (when (member-if (lambda (type) (subtypep type nil))
		     (cleavir-ir:required-types i))
	;; we have a noreturn, can proceed
	(let ((call (first (cleavir-ir:predecessors i))))
	  ;; this condition should be always true from AST-to-HIR,
	  ;;  but we might as well check.
	  ;; in english, the THE-VALUES is the direct successor of
	  ;;  a FUNCALL and defines the types of its results.
	  (when (and (= (length (cleavir-ir:predecessors i)) 1)
		     (typep call 'cleavir-ir:funcall-instruction)
		     (eq (first (cleavir-ir:inputs i))
			 (first (cleavir-ir:outputs call))))
	    ;; ok, the actual transform
	    (if safe-p
		;; FIXME: abstract this?
		(setf (cleavir-ir:successors call)
		      ;; FIXME: "expected a value of type NIL" is
		      ;;  not a good error message.
		      (list (make-type-error
			     i (first (cleavir-ir:inputs i)) nil)))
		(change-class
		 i 'cleavir-ir:funcall-no-return-instruction
		 :successors nil :outputs nil)))))))
  ;; paths are dead. make sure the IR is consistent.
  (cleavir-ir:set-predecessors initial)
  (cleavir-ir:reinitialize-data initial)
  initial)
