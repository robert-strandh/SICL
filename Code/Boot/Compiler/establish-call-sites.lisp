(cl:in-package #:sicl-boot-compile-and-tie)

(defgeneric instruction (call-site))

(defclass call-site (sicl-compiler:call-site)
  ((%instruction :initarg :instruction :reader instruction)))

(defun call-site-name (instruction)
  (typecase instruction
    (cleavir-ir:named-call-instruction
     (cleavir-ir:callee-name instruction))
    (cleavir-ir:enclose-instruction
     'sicl-run-time:enclose)
    (cleavir-ir:catch-instruction
     'sicl-run-time:augment-with-block/tagbody-entry)
    (cleavir-ir:dynamic-catch-instruction
     'sicl-run-time:augment-with-catch-entry)
    (cleavir-ir:bind-instruction
     'sicl-run-time:augment-with-special-variable-entry)
    (cleavir-ir:unwind-protect-instruction
     'sicl-run-time:augment-with-unwind-protect-entry)
    (cleavir-ir:unwind-instruction
     'sicl-run-time:unwind)
    (cleavir-ir:initialize-values-instruction
     ;; FIXME: Use a better function.
     'error)
    (cleavir-ir:multiple-value-call-instruction
     'sicl-run-time:call-with-values)
    (cleavir-ir:save-values-instruction
     'sicl-run-time:save-values)
    (cleavir-ir:restore-values-instruction
     'sicl-run-time:restore-values)
    (sicl-ir:patch-literal-instruction
     'sicl-run-time:resolve-load-time-value)))

(defun establish-call-site (instruction)
  (change-class instruction
                'sicl-ir:named-call-instruction
                :function-cell-cell (list nil))
  (make-instance 'call-site
    :name (call-site-name instruction)
    :instruction instruction))

(defun establish-call-sites (ir)
  (let ((call-sites '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:named-call-instruction)
         (push (establish-call-site instruction)
               call-sites)))
     ir)
    call-sites))
