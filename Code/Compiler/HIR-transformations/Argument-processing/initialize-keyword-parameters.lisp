(cl:in-package #:sicl-argument-processing)

(defun check-even-number-of-keyword-arguments
    (argument-count-location dynamic-environment-location first-index)
  (let* ((remaining-argument-count-location
           (make-instance 'cleavir-ir:lexical-location :name (gensym "rem-ac")))
         (quotient-location
           (make-instance 'cleavir-ir:lexical-location :name (gensym "quotient")))
         (remainder-location
           (make-instance 'cleavir-ir:lexical-location :name (gensym "remainder")))
         (constant-input-2
           (make-instance 'cleavir-ir:constant-input :value 2))
         (constant-input-0
           (make-instance 'cleavir-ir:constant-input :value 0))
         (first-index-input (make-instance 'cleavir-ir:constant-input :value first-index))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location))
         (error-branch
           (call-error 'odd-number-of-keyword-arguments
                       dynamic-environment-location
                       nil ; FIXME: Pass a better value for the origin
                       (make-instance 'cleavir-ir:constant-input :value :argument-count)
                       remaining-argument-count-location)))
    (let ((first (make-instance 'cleavir-ir:fixnum-equal-instruction
                   :inputs (list constant-input-0 remainder-location)
                   :successors (list nop error-branch)
                   :dynamic-environment-location dynamic-environment-location)))
      (setf first
            (make-instance 'cleavir-ir:fixnum-divide-instruction
              :inputs (list remaining-argument-count-location constant-input-2)
              :outputs (list quotient-location remainder-location)
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:fixnum-sub-instruction
              :inputs (list argument-count-location first-index-input)
              :output remaining-argument-count-location
              :successors (list first first)
              :dynamic-environment-location dynamic-environment-location))
      (values first nop))))

(defun check-keywords-valid
    (keywords argument-count-location dynamic-environment-location first-index)
  (let* ((keyword-location (make-instance 'cleavir-ir:lexical-location :name (gensym "keyword")))
         (first-index-input (make-instance 'cleavir-ir:constant-input :value first-index))
         (index-location (make-instance 'cleavir-ir:lexical-location :name (gensym "index")))
         (constant-input-2
           (make-instance 'cleavir-ir:constant-input :value 2))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location))
         (first
           (call-error 'invalid-keyword
                       dynamic-environment-location
                       nil ; FIXME: Pass a better value for the origin
                       (make-instance 'cleavir-ir:constant-input :value :keyword)
                       keyword-location)))
    (let ((add-2 (make-instance 'cleavir-ir:fixnum-add-instruction
                   :inputs (list index-location constant-input-2)
                   :output index-location
                   :dynamic-environment-location dynamic-environment-location)))
      (loop for keyword in (cons :allow-other-keys (reverse keywords))
            for input = (make-instance 'cleavir-ir:constant-input :value keyword)
            do (setf first
                     (make-instance 'cleavir-ir:eq-instruction
                       :inputs (list input keyword-location)
                       :successors (list add-2 first)
                       :dynamic-environment-location dynamic-environment-location)))
      (setf first
            (make-instance 'cleavir-ir:argument-instruction
              :input index-location
              :output keyword-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:fixnum-less-instruction
              :inputs (list index-location argument-count-location)
              :successors (list first nop)
              :dynamic-environment-location dynamic-environment-location))
      (setf (cleavir-ir:successors add-2) (list first first))
      (setf first
            (make-instance 'cleavir-ir:assignment-instruction
              :input first-index-input
              :output index-location
              :successor first
              :dynamic-environment-location dynamic-environment-location)))
    (values first nop)))

(defun check-for-allow-other-keys
    (argument-count-location dynamic-environment-location first-index)
  (let* ((keyword-location (make-instance 'cleavir-ir:lexical-location :name (gensym "keyword")))
         (value-location (make-instance 'cleavir-ir:lexical-location :name (gensym "value")))
         (first-index-input (make-instance 'cleavir-ir:constant-input :value first-index))
         (index-location (make-instance 'cleavir-ir:lexical-location :name (gensym "index")))
         (temp-location (make-instance 'cleavir-ir:lexical-location :name (gensym "temp")))
         (constant-input-1
           (make-instance 'cleavir-ir:constant-input :value 1))
         (constant-input-2
           (make-instance 'cleavir-ir:constant-input :value 2))
         (constant-input-nil
           (make-instance 'cleavir-ir:constant-input :value nil))
         (constant-input-allow-other-keys
           (make-instance 'cleavir-ir:constant-input :value :allow-other-keys))
         (add-2 (make-instance 'cleavir-ir:fixnum-add-instruction
                  :inputs (list index-location constant-input-2)
                  :output index-location
                  :dynamic-environment-location dynamic-environment-location))
         (nop-false (make-instance 'cleavir-ir:nop-instruction
                      :dynamic-environment-location dynamic-environment-location))
         (nop-true (make-instance 'cleavir-ir:nop-instruction
                     :dynamic-environment-location dynamic-environment-location))
         (first nop-true))
    (setf first
          (make-instance 'cleavir-ir:eq-instruction
            :inputs (list value-location constant-input-nil)
            :successors (list nop-false first)
            :dynamic-environment-location dynamic-environment-location))
    (setf first
          (make-instance 'cleavir-ir:argument-instruction
            :input temp-location
            :output value-location
            :successor first
            :dynamic-environment-location dynamic-environment-location))
    (setf first
          (make-instance 'cleavir-ir:fixnum-add-instruction
            :inputs (list constant-input-1 index-location)
            :output temp-location
            :successors (list first first)
            :dynamic-environment-location dynamic-environment-location))
    (setf first
          (make-instance 'cleavir-ir:eq-instruction
            :inputs (list constant-input-allow-other-keys keyword-location)
            :successors (list first add-2)
            :dynamic-environment-location dynamic-environment-location))
    (setf first
          (make-instance 'cleavir-ir:argument-instruction
            :input index-location
            :output keyword-location
            :successor first
            :dynamic-environment-location dynamic-environment-location))
    (setf first
          (make-instance 'cleavir-ir:fixnum-not-greater-instruction
            :inputs (list argument-count-location index-location)
            :successors (list nop-false first)
            :dynamic-environment-location dynamic-environment-location))
    (setf (cleavir-ir:successors add-2) (list first first))
    (setf first
          (make-instance 'cleavir-ir:assignment-instruction
            :input first-index-input
            :output index-location
            :successor first
            :dynamic-environment-location dynamic-environment-location))
    (values first nop-false nop-true)))

(defun initialize-one-keyword-parameter
    (keyword
     argument-count-location
     keyword-value-location
     supplied-p-location
     dynamic-environment-location
     first-index)
  (let* ((keyword-location (make-instance 'cleavir-ir:lexical-location :name (gensym "keyword")))
         (first-index-input (make-instance 'cleavir-ir:constant-input :value first-index))
         (index-location (make-instance 'cleavir-ir:lexical-location :name (gensym "index")))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location))
         (temp-location (make-instance 'cleavir-ir:lexical-location :name (gensym "temp")))
         (constant-input-1
           (make-instance 'cleavir-ir:constant-input :value 1))
         (constant-input-2
           (make-instance 'cleavir-ir:constant-input :value 2))
         (constant-input-nil
           (make-instance 'cleavir-ir:constant-input :value nil))
         (constant-input-t
           (make-instance 'cleavir-ir:constant-input :value t))
         (constant-input-keyword
           (make-instance 'cleavir-ir:constant-input :value keyword))
         (add-2 (make-instance 'cleavir-ir:fixnum-add-instruction
                  :inputs (list index-location constant-input-2)
                  :output index-location
                  :dynamic-environment-location dynamic-environment-location))
         (false-branch nop)
         (true-branch nop))
    (setf true-branch
          (make-instance 'cleavir-ir:assignment-instruction
            :input constant-input-nil
            :output supplied-p-location
            :successor true-branch
            :dynamic-environment-location dynamic-environment-location))
    (setf true-branch
          (make-instance 'cleavir-ir:assignment-instruction
            :input constant-input-nil
            :output keyword-value-location
            :successor true-branch
            :dynamic-environment-location dynamic-environment-location))
    (setf false-branch
          (make-instance 'cleavir-ir:assignment-instruction
            :input constant-input-t
            :output supplied-p-location
            :successor false-branch
            :dynamic-environment-location dynamic-environment-location))
    (setf false-branch
          (make-instance 'cleavir-ir:argument-instruction
            :input temp-location
            :output keyword-value-location
            :successor false-branch
            :dynamic-environment-location dynamic-environment-location))
    (setf false-branch
          (make-instance 'cleavir-ir:fixnum-add-instruction
            :inputs (list constant-input-1 index-location)
            :output temp-location
            :successors (list false-branch false-branch)
            :dynamic-environment-location dynamic-environment-location))
    (setf false-branch
          (make-instance 'cleavir-ir:eq-instruction
            :inputs (list constant-input-keyword keyword-location)
            :successors (list false-branch add-2)
            :dynamic-environment-location dynamic-environment-location))
    (setf false-branch
          (make-instance 'cleavir-ir:argument-instruction
            :input index-location
            :output keyword-location
            :successor false-branch
            :dynamic-environment-location dynamic-environment-location))
    (let ((first (make-instance 'cleavir-ir:fixnum-not-greater-instruction
                   :inputs (list argument-count-location index-location)
                   :successors (list true-branch false-branch)
                   :dynamic-environment-location dynamic-environment-location)))
      (setf (cleavir-ir:successors add-2)
            (list first first))
      (setf first
            (make-instance 'cleavir-ir:assignment-instruction
              :input first-index-input
              :output index-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (values first nop))))

(defun process-keyword-arguments
    (parameters
     argument-count-location
     dynamic-environment-location
     first-index
     allow-other-keys-p)
  (let* ((nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location))
         (first nil))
    (if allow-other-keys-p
        (setf first nop)
        (multiple-value-bind (first-a last-a)
            (check-keywords-valid (mapcar #'first parameters)
                                  argument-count-location
                                  dynamic-environment-location
                                  first-index)
          (setf (cleavir-ir:successors last-a)
                (list nop))
          (multiple-value-bind (first-b last-b-false last-b-true)
              (check-for-allow-other-keys argument-count-location
                                          dynamic-environment-location
                                          first-index)
            (setf (cleavir-ir:successors last-b-false)
                  (list first-a))
            (setf (cleavir-ir:successors last-b-true)
                  (list nop))
            (setf first first-b))))
    (loop for (keyword value-location supplied-p-location) in parameters
          do (multiple-value-bind (first-c last-c)
                 (initialize-one-keyword-parameter keyword
                                                   argument-count-location
                                                   value-location
                                                   supplied-p-location
                                                   dynamic-environment-location
                                                   first-index)
               (setf (cleavir-ir:successors last-c)
                     (list first))
               (setf first first-c)))
    (multiple-value-bind (first-d last-d)
        (check-even-number-of-keyword-arguments argument-count-location
                                                dynamic-environment-location
                                                first-index)
      (setf (cleavir-ir:successors last-d)
            (list first))
      (setf first first-d))
    (values first nop)))
