(cl:in-package #:cleavir-literals)

(defclass similarity-table ()
  (;; The EQL table is used to ensure that each object is only scanned once.
   ;; This works because if two objects are EQL, they are also similar in
   ;; the sense of CLHS 3.2.4.2.2.
   (%eql-table :initform (make-hash-table :test #'eq) :reader eql-table)
   ;; The EQUAL and EQUALP tables are used to coalesce literal objects
   ;; where the rules of similarity are less restrictive, e.g., specialized
   ;; arrays or hash tables.
   (%equal-table :initform (make-hash-table :test #'equal) :reader equal-table)
   (%equalp-table :initform (make-hash-table :test #'equalp) :reader equalp-table)))

(defmacro literal-record-cache (object)
  `(gethash ,object (eql-table *similarity-table*)))

(defmethod load-time-literal :around (client object environment)
  (multiple-value-bind (literal-record present-p)
      (literal-record-cache object)
    (cond ((not present-p)
           (call-next-method))
          (present-p
           (unless (creation-form-finalized-p literal-record)
             (error 'circular-dependencies-in-creation-form
                    :object object
                    :creation-form (creation-form literal-record)))
           (lexical-ast literal-record)))))
