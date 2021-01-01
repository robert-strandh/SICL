(cl:in-package #:cleavir-ir)

(defun all (&rest arguments)
  (notany #'null arguments))

(defun none (&rest arguments)
  (every #'null arguments))

(defun all-or-none (&rest arguments)
  (or (apply #'all arguments)
      (apply #'none arguments)))

(defun combine (combination &rest arguments)
  (if (apply #'none arguments)
      combination
      (copy-list arguments)))

;;; This macro looks a bit messy.  The reason for that is that I could
;;; not figure out a better way of accomplishing what it does.
;;;
;;; Essentially, we want to simulate the possibility of giving keyword
;;; aguments naming individual inputs, outputs, and successors when an
;;; instruction is created or when it has its class changed.  The best
;;; way of doing that seemed to have an :AROUND method on
;;; SHARED-INITIALIZE that checks whether one of these individual
;;; keyword arguments has been given, and if so, translates them to a
;;; list of inputs, outputs, or successors, before calling
;;; CALL-NEXT-METHOD.
;;;
;;; Things get complicated though, because if neither (say) any
;;; individual input name was given, NOR the :INPUTS keyword argument,
;;; then CALL-NEXT-METHOD should not be given an explicit :INPUTS,
;;; so as to preserve the existing slot value.
;;;
;;; The best thing I could think of was to distinguish eight cases
;;; depending on whether any individual input argument, any individual
;;; output argument, and any individual successor was given, and for
;;; each of the eight cases, call CALL-NEXT-METHOD differently.
;;;
;;; Also we do some error checking.  If any individual keyword
;;; argument was given, then they must all be given, and the aggregate
;;; one must not be given.
;;;
;;; Although this macro is a bit messy, it saves a lot of code
;;; elsewhere.  So until I can think of a better way of doing it, I
;;; still think it is a win.
(defmacro normalize-arguments
    (instruction-class-name input-names output-names successor-names)
  `(defmethod shared-initialize :around
     ((instruction ,instruction-class-name)
      slot-names
      &rest keys
      &key
      (inputs nil inputs-p) ,@input-names
      (outputs nil outputs-p) ,@output-names
      (successors nil successors-p) ,@successor-names)
     (declare (ignore inputs outputs successors))
     (cond ((or ,@input-names)
            (assert (and ,@input-names))
            (assert (not inputs-p))
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 keys))))))
           (t
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 keys)))))))))
