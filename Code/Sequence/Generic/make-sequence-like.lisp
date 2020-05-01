(cl:in-package #:sicl-sequence)

(defmethod make-sequence-like
    ((list list) length &key (initial-element nil initial-element-p)
                             (initial-contents nil initial-contents-p))
  (declare (method-properties inlineable))
  (cond ((and initial-element-p initial-contents-p)
         (error "Both ~S and ~S supplied to ~D."
                :initial-element :initial-contents 'make-sequence-like))
        (initial-element-p
         (make-list length :initial-element initial-element))
        (initial-contents-p
         (unless (= (length initial-contents) length)
           (error "Length mismatch in ~S." 'make-sequence-like))
         (let ((result (make-list length)))
           (replace result initial-contents)
           result))
        (t
         (make-list (list-length list)))))

(seal-domain #'make-sequence-like '(list t))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod make-sequence-like
    ((vectoroid #1#) length &key (initial-element nil initial-element-p)
                                 (initial-contents nil initial-contents-p))
    (declare (method-properties inlineable))
    (cond ((and initial-element-p initial-contents-p)
           (error "Both ~S and ~S supplied to ~D."
                  :initial-element :initial-contents 'make-sequence-like))
          (initial-element-p
           (make-array length :element-type (array-element-type vectoroid)
                              :initial-element initial-element))
          (initial-contents-p
           (make-array length :element-type (array-element-type vectoroid)
                              :initial-contents initial-contents))
          (t
           (make-array length :element-type (array-element-type vectoroid))))))

(seal-domain #'make-sequence-like '(vector t))
