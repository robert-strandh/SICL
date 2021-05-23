(cl:in-package #:cleavir-type-inference)

;;; Called for effect. Update the inference information from the
;;; instruction.
(defgeneric process-instruction (instruction))

;; default method. TODO: better dispatch here?
(defmethod process-instruction (instruction)
  (let ((input (instruction-input instruction *dictionary*)))
    (ecase (length (cleavir-ir:successors instruction))
      (0 nil)
      (1 (one-successor-transfer instruction input))
      (2 (two-successors-transfer instruction input)))))

;;; These functions all take an instruction, and the bag of
;;; descriptors in place as the input of that instruction.
;;; That is, the bag is a bag-join of all incoming arcs' bags.

;;; Returns one bag.
(defgeneric one-successor-transfer (instruction input-bag))

;;; Returns two bags, one for each branch.
(defgeneric two-successors-transfer (instruction input-bag))

(defmethod one-successor-transfer :around (instruction input-bag)
  (let* ((result (call-next-method))
	 (successor (cleavir-ir:first-successor instruction))
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
	when (typep output 'cleavir-ir:values-location)
	  do (setf result (update output (values-top) result))
	finally (return result)))

(defmethod two-successors-transfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result (update output t result))
	when (typep output 'cleavir-ir:values-location)
	  do (setf result (update output (values-top) result))
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
    ((instruction cleavir-ir:aref-instruction) input-bag)
  ;; TODO: update array
  (let ((index (second (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (update output
	    (if (cleavir-ir:boxed-p instruction)
		element-descriptor
		(descriptor-unbox element-descriptor))
	    (update index
		    (binary-meet (find-type index input-bag)
				 ;; could use array dimensions,
				 ;; or a more proper thing with
				 ;; ARRAY-TOTAL-SIZE
				 (approximate-type 'fixnum))
		    input-bag))))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:aset-instruction) input-bag)
  ;; TODO: update array
  (let ((index (second (cleavir-ir:inputs instruction)))
	(object (third (cleavir-ir:inputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (update object
	    (binary-meet
	     (find-type object input-bag)
	     (if (cleavir-ir:boxed-p instruction)
		 element-descriptor
		 ;; if the array's elements are unboxed, the object
		 ;; being written must be unboxed.
		 (descriptor-unbox element-descriptor)))
	    (update index
		    (binary-meet (find-type index input-bag)
				 ;; could use array dimensions,
				 ;; or a more proper thing with
				 ;; ARRAY-TOTAL-SIZE
				 (approximate-type 'fixnum))
		    input-bag))))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:box-instruction) input-bag)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (update input
	    (binary-meet (find-type input input-bag)
			 (descriptor-unbox element-descriptor))
	    (update output
		    element-descriptor
		    input-bag))))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:unbox-instruction) input-bag)
  (let ((input (first (cleavir-ir:inputs instruction)))
	(output (first (cleavir-ir:outputs instruction)))
	(element-descriptor
	  (approximate-type
	   (cleavir-ir:element-type instruction))))
    (update input
	    (binary-meet (find-type input input-bag)
			 element-descriptor)
	    (update output
		    (descriptor-unbox element-descriptor)
		    input-bag))))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:the-values-instruction) input-bag)
  (let* ((input (first (cleavir-ir:inputs instruction)))
	 (input-type (find-type input input-bag))
	 (descriptor
	   (approximate-values
	    (cleavir-ir:required-types instruction)
	    (cleavir-ir:optional-types instruction)
	    (cleavir-ir:rest-type instruction))))
    (if (values-top-p descriptor)
	input-bag ; don't bother
	(update input (values-binary-meet descriptor input-type)
		input-bag))))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:fixed-to-multiple-instruction)
     input-bag)
  (let* ((types (mapcar
		 (lambda (input) (find-type input input-bag))
		 (cleavir-ir:inputs instruction)))
	 (values-type (approximate-values types nil nil))
	 (output (first (cleavir-ir:outputs instruction))))
    (update output values-type input-bag)))

(defmethod one-successor-transfer
    ((instruction cleavir-ir:multiple-to-fixed-instruction)
     input-bag)
  (loop with vtype = (find-type
		      (first (cleavir-ir:inputs instruction))
		      input-bag)
	for n from 0
	for output in (cleavir-ir:outputs instruction)
	for bag = (update output (values-nth vtype n) input-bag)
	  then (update output (values-nth vtype n) bag)
	finally (return bag)))

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
