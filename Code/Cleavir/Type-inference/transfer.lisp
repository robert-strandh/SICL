(cl:in-package #:cleavir-type-inference)

(defgeneric one-successor-transfer (instruction input-bag))

(defgeneric two-successors-transfer (instruction input-bag))

(defmethod one-successor-transfer :around (instruction input-bag)
  (let* ((result (call-next-method))
	 (successor (first (cleavir-ir:successors instruction)))
	 (key (cons instruction successor))
	 (existing (gethash key *dictionary*))
	 (filtered-result (filter result existing)))
    (unless (bag-equal filtered-result existing)
      (push successor *work-list*)
      (setf (gethash key *dictionary*) filtered-result))))

(defmethod one-successor-transfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result (update output t result))
	finally (return result)))

(defmethod two-successors-transfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result (update output t result))
	finally (return (values result result))))

(defmethod two-successors-transfer :around (instruction input-bag)
  (loop with results = (multiple-value-list (call-next-method))
	for result in results
	for successor in (cleavir-ir:successors instruction)
	for key = (cons instruction successor)
	for existing = (gethash key *dictionary*)
	for filtered-result = (filter result existing)
	unless (bag-equal filtered-result existing)
	  do (push successor *work-list*)
	     (setf (gethash key *dictionary*) filtered-result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ONE-SUCCESSOR-TRANSFER.

;;; For the assignment instruction, type information about the input
;;; is propagated to the output.
(defmethod one-successor-transfer
    ((instruction cleavir-ir:assignment-instruction) input-bag)
  (update (first (cleavir-ir:outputs instruction))
	  (find-type (first (cleavir-ir:inputs instruction)) input-bag)
	  input-bag))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:short-float-unbox-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'unboxed-short-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:short-float-box-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'short-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:single-float-unbox-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'unboxed-single-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:single-float-box-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'single-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:double-float-unbox-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'unboxed-double-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:double-float-box-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'double-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:long-float-unbox-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'unboxed-long-float input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:long-float-box-instruction) input-bag)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (update output 'long-float input-bag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TWO-SUCCESSORS-TRANSFER.

(defmethod two-successors-transfer
    ((instruction cleavir-ir:typeq-instruction) input-bag)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input input-bag))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor (canonicalize-type type)))
    (if (null type-descriptor)
	;; This situation happens when the value type of the
	;; instruction is not type equal to any of the types that we
	;; recognize.  We handle this case by assuming that the type
	;; of the input is possible in both branches.
	(values input-bag input-bag)
	(values (update input
			(binary-meet type-descriptor input-type)
			input-bag)
		(update input
			(binary-join type-descriptor input-type)
			input-bag)))))

(defmethod two-successors-transfer
    ((instruction cleavir-ir:fixnump-instruction) input-bag)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input input-bag)))
    (values (update input
		    (binary-meet 'fixnum input-type)
		    input-bag)
	    (update input
		    (binary-join 'fixnum input-type)
		    input-bag))))

(defmethod two-successors-transfer
    ((instruction cleavir-ir:consp-instruction) input-bag)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input input-bag)))
    (values (update input
		    (binary-meet 'cons input-type)
		    input-bag)
	    (update input
		    (binary-join 'cons input-type)
		    input-bag))))
