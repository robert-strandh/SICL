(cl:in-package #:sicl-argument-processing)

(defun extract-required-parameters (lambda-list)
  (let ((first-lambda-list-keyword-position
          (position-if (lambda (x) (member x lambda-list-keywords))
                       lambda-list)))
    (if (null first-lambda-list-keyword-position)
        lambda-list
        (subseq lambda-list 0 first-lambda-list-keyword-position))))

(defun extract-optional-parameters (lambda-list)
  (let ((optional-position (position '&optional lambda-list :test #'eq)))
    (if (null optional-position)
        '()
         (let ((next-lambda-list-keyword-position
                 (position-if (lambda (x) (member x lambda-list-keywords))
                              lambda-list
                              :start (1+ optional-position))))
           (if (null next-lambda-list-keyword-position)
               (subseq lambda-list (1+ optional-position))
               (subseq lambda-list
                       (1+ optional-position)
                       next-lambda-list-keyword-position))))))

(defun extract-rest-parameter (lambda-list)
  (let ((rest-position (position '&rest lambda-list :test #'eq)))
    (if (null rest-position)
        nil
        (nth (1+ rest-position) lambda-list))))

(defun extract-key-parameters (lambda-list)
  (let ((key-position (position '&key lambda-list :test #'eq)))
    (if (null key-position)
        (values '() nil nil)
        (let ((allow-other-keys-p (eq (first (last lambda-list))
                                      '&allow-other-keys)))
          (if allow-other-keys-p
              (values (subseq lambda-list
                              (1+ key-position)
                              (1- (length lambda-list)))
                      t
                      t)
              (values (subseq lambda-list (1+ key-position))
                      t
                      nil))))))

(defun process-enter-instruction (enter-instruction)
  (let* ((lambda-list (cleavir-ir:lambda-list enter-instruction))
         (required-parameters (extract-required-parameters lambda-list))
         (optional-parameters (extract-optional-parameters lambda-list))
         (rest-parameter (extract-rest-parameter lambda-list))
         (successor (cleavir-ir:first-successor enter-instruction))
         (no-more-arguments-branch successor)
         (more-arguments-branch successor)
         (argument-count-location
           (make-instance 'cleavir-ir:lexical-location :name (gensym "argc")))
         (dynamic-environment-location
           (cleavir-ir:dynamic-environment-location enter-instruction)))
    (setf (cleavir-ir:predecessors successor) '())
    (multiple-value-bind (key-parameters key-p allow-other-keys-p)
        (extract-key-parameters lambda-list)
      (when key-p
        (multiple-value-bind (first last)
            (initialize-keyword-parameters-to-nil
             (mapcar #'rest key-parameters) dynamic-environment-location)
          (setf (cleavir-ir:successors last)
                (list no-more-arguments-branch))
          (setf no-more-arguments-branch first))
        (multiple-value-bind (first last)
            (process-keyword-arguments
             key-parameters
             argument-count-location
             dynamic-environment-location
             (+ (length required-parameters) (length optional-parameters))
             allow-other-keys-p)
          (setf (cleavir-ir:successors last)
                (list more-arguments-branch))
          (setf more-arguments-branch first)))
      (unless (null rest-parameter)
        (multiple-value-bind (first last)
            (create-rest-parameter
             argument-count-location
             rest-parameter
             dynamic-environment-location
             (+ (length required-parameters) (length optional-parameters)))
          (setf (cleavir-ir:successors last)
                (list more-arguments-branch))
          (setf more-arguments-branch first)))
      (if (and (null rest-parameter) (null key-p) (null optional-parameters))
          (setf more-arguments-branch successor)
          (multiple-value-bind (first no-more-last more-last)
              (initialize-optional-parameters
               optional-parameters
               argument-count-location
               dynamic-environment-location
               (+ (length required-parameters) (length optional-parameters)))
            (setf (cleavir-ir:successors no-more-last)
                  (list no-more-arguments-branch))
            (setf (cleavir-ir:successors more-last)
                  (list more-arguments-branch))
            (setf more-arguments-branch first)))
      (unless (null rest-parameter)
        (setf more-arguments-branch
              (make-instance 'cleavir-ir:assignment-instruction
                :input (make-instance 'cleavir-ir:constant-input :value nil)
                :output rest-parameter
                :successor more-arguments-branch
                :dynamic-environment-location dynamic-environment-location)))
      (multiple-value-bind (first last)
          (initialize-required-parameters
           required-parameters dynamic-environment-location)
        (setf (cleavir-ir:successors last)
              (list more-arguments-branch))
        (setf more-arguments-branch first))
      (when (and (null rest-parameter) (null key-p))
        (multiple-value-bind (first last)
            (check-maximum-argument-count
             argument-count-location
             (+ (length required-parameters) (length optional-parameters))
             dynamic-environment-location)
          (setf (cleavir-ir:successors last)
                (list more-arguments-branch))
          (setf more-arguments-branch first)))
      (unless (null required-parameters)
        (multiple-value-bind (first last)
            (check-minimum-argument-count
             argument-count-location
             (length required-parameters)
             dynamic-environment-location)
          (setf (cleavir-ir:successors last)
                (list more-arguments-branch))
          (setf more-arguments-branch first)))
      (setf more-arguments-branch
            (make-instance 'cleavir-ir:compute-argument-count-instruction
              :inputs '()
              :output argument-count-location
              :successor more-arguments-branch
              :dynamic-environment-location dynamic-environment-location))
      (setf (cleavir-ir:successors enter-instruction)
            (list more-arguments-branch)))))

(defun process-parameters (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:enter-instruction)
       (process-enter-instruction instruction)))
   top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for successor in (cleavir-ir:successors instruction)
           do (pushnew instruction (cleavir-ir:predecessors successor))))
   top-level-enter-instruction))
