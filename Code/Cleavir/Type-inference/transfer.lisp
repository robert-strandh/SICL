(cl:in-package #:cleavir-type-inference)

(defgeneric one-successor-transfer (instruction input-bag))

(defgeneric two-successors-transfer (instruction input-bag))

(defmethod one-successor-transfer :around (instruction input-bag)
  (let* ((result (call-next-method))
	 (successor (first (cleavir-ir:successors instruction)))
	 (existing (arc-bag instruction successor *dictionary*))
	 (filtered-result (filter result existing)))
    (unless (bag-equal filtered-result existing)
      (push successor *work-list*)
      (setf (arc-bag instruction successor *dictionary*)
	    filtered-result))))

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
	for existing = (arc-bag instruction successor *dictionary*)
	for filtered-result = (filter result existing)
	unless (bag-equal filtered-result existing)
	  do (push successor *work-list*)
	     (setf (arc-bag instruction successor *dictionary*)
		   filtered-result)))

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
    ((instruction cleavir-ir:enclose-instruction) input-bag)
  ;; hypothetically this method could wait for the results of type
  ;;  inference on the function (and give up only for recursion)
  ;; or include a lambda list. but we don't care yet.
  (update (first (cleavir-ir:outputs instruction))
	  (approximate-type 'function)
	  input-bag))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:nop-instruction) input-bag)
  input-bag)

(defmethod one-successor-transfer
    ((instruction cleavir-ir:use-instruction) input-bag)
  input-bag)

(defmethod one-successor-transfer
    ((instruction cleavir-ir:the-instruction) input-bag)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input input-bag))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor (approximate-type type)))
    (if (top-p type-descriptor)
	input-bag ; don't bother
	(update input (binary-meet type-descriptor input-type)
		input-bag))))

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
	 (type (cleavir-ir:value-type instruction)))
    (multiple-value-bind (type-descriptor canon)
	(canonicalize-type type)
      (if canon
	  (values (update input
			  (binary-meet input-type type-descriptor)
			  input-bag)
		  (update input
			  (difference input-type type-descriptor)
			  input-bag))
	  ;; couldn't find a type descriptor, so infer nothing.
	  (values input-bag input-bag)))))

(defmethod two-successors-transfer
    ((instruction cleavir-ir:eq-instruction) input-bag)
  (let* ((left (first (cleavir-ir:inputs instruction)))
	 (left-type (find-type left input-bag))
	 (right (second (cleavir-ir:inputs instruction)))
	 (right-type (find-type right input-bag))
	 (meet (binary-meet left-type right-type)))
    (values
     (update left meet (update right meet input-bag))
     input-bag)))
