(in-package #:cleavir-hir-transformations)

;;;; This utility eliminates VALUES-LOCATIONs which are never used as
;;;; such, only as the inputs or outputs to M->F or F->M instructions.

;;; Given a values location, see if it can be replaced by ordinary
;;; lexical locations and assignments. If so, do that.
(defun maybe-convert-values-location (location)
  (let ((defines (cleavir-ir:defining-instructions location))
        (uses (cleavir-ir:using-instructions location)))
    (when (and (every (lambda (define)
                        (typep define 'cleavir-ir:fixed-to-multiple-instruction))
                      defines)
               (every (lambda (use)
                        (typep use 'cleavir-ir:multiple-to-fixed-instruction))
                      uses))
      ;; First, spawn as many lexical locations as needed to pass all
      ;; useful information.
      (let* ((passing-count (min (loop for define in defines
                                       maximize (length (cleavir-ir:inputs define)))
                                 (loop for use in uses
                                       maximize (length (cleavir-ir:outputs use)))))
             (lexical-locations
               (loop repeat passing-count
                     collect (cleavir-ir:new-temporary))))
        (dolist (define defines)
          (let ((cleavir-ir:*origin* (cleavir-ir:origin define))
                (cleavir-ir:*policy* (cleavir-ir:policy define))
                (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment define)))
            (loop for input in (cleavir-ir:inputs define)
                  for location in lexical-locations
                  do (cleavir-ir:insert-instruction-before
                      (cleavir-ir:make-assignment-instruction input location define)
                      define)))
          (cleavir-ir:delete-instruction define))
        (dolist (use uses)
          (let* ((outputs (cleavir-ir:outputs use))
                 (defaulting-count (max (- (length outputs) passing-count) 0))
                 (cleavir-ir:*origin* (cleavir-ir:origin use))
                 (cleavir-ir:*policy* (cleavir-ir:policy use))
                 (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment use))
                 (nil-datum (cleavir-ir:make-load-time-value-input nil t)))
            (loop for output in (cleavir-ir:outputs use)
                  for location in (append lexical-locations (make-list defaulting-count :initial-element nil-datum))
                  do (cleavir-ir:insert-instruction-before
                      (cleavir-ir:make-assignment-instruction location output)
                      use)))
          (cleavir-ir:delete-instruction use))))))
