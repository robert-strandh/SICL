(cl:in-package #:cleavir-value-hoisting)

;;; Scanning is the process of associating every externalizable object and
;;; load time value form with a suitable constructor.  Constructors of
;;; similar objects are coalesced in the process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning

(defmethod scan-hir (client (hir null))
  (values))

(defmethod scan-hir (client (hir cleavir-ir:instruction))
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction) do
       (scan-datum client datum)))
   hir))

;;; One thing to keep in mind is that it is entirely permissible for a
;;; Cleavir datum to appear as a literal object in a file.  So it is
;;; important to clearly distinguish the case of scanning a datum and the
;;; case of scanning a literal object.  I am writing this down here because
;;; I already forgot about this issue twice.

(defmethod scan-datum (client (datum cleavir-ir:datum))
  (values))

(defmethod scan-datum (client (constant-input cleavir-ir:constant-input))
  (let ((value (cleavir-ir:value constant-input)))
    (scan-literal-object client value)))

(defmethod scan-literal-object (client object)
  (multiple-value-bind (constructor present-p)
      (constructor object)
    (cond ((not present-p)
           (let ((constructor
                   (multiple-value-bind (creation-form initialization-form)
                       (make-load-form-using-client client object *environment*)
                     (make-constructor client creation-form initialization-form))))
             (setf (constructor object) constructor)
             (scan-hir client (creation-thunk constructor))
             (setf (creation-form-finalized-p constructor) t)
             (scan-hir client (initialization-thunk constructor))
             ;; Attempt to coalesce this constructor, i.e., replace it with
             ;; an existing constructor of a similar object.
             (multiple-value-bind (equal-keys equalp-keys)
                 (similarity-keys client object)
               (loop for equal-key in equal-keys do
                 (coalesce-using-equal object equal-key))
               (loop for equalp-key in equalp-keys do
                 (coalesce-using-equalp object equalp-key)))))
          ((not (creation-form-finalized-p constructor))
           (error 'circular-dependencies-in-creation-form
                  :object object
                  :creation-form (creation-form constructor))))))
