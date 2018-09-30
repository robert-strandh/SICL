(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Scanning is the process of associating every externalizable object and
;;; load time value form with a suitable constructor.  Constructors of
;;; similar objects are coalesced in the process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning

(defmethod scan-hir ((hir null) system)
  (values))

(defmethod scan-hir ((hir cleavir-ir:instruction) system)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction) do
       (simplify-datum datum system)
       (scan-datum datum system)))
   hir))

;;; One thing to keep in mind is that it is entirely permissible for a
;;; Cleavir datum to appear as a literal object in a file.  So it is
;;; important to clearly distinguish the case of scanning a datum and the
;;; case of scanning a literal object.  I am writing this down here because
;;; I already forgot about this issue twice.

(defmethod scan-datum ((datum cleavir-ir:datum) system)
  (values))

(defmethod scan-datum ((constant-input cleavir-ir:constant-input) system)
  (let ((value (cleavir-ir:value constant-input)))
    (scan-literal-object value system)))

(defmethod scan-datum ((load-time-value-input cleavir-ir:load-time-value-input) system)
  (multiple-value-bind (constructor present-p)
      (constructor load-time-value-input)
    (cond ((not present-p)
           (let* ((form (cleavir-ir:form load-time-value-input))
                  (constructor (make-constructor form nil system)))
             (setf (constructor load-time-value-input) constructor)
             (scan-hir (creation-thunk constructor) system)
             (setf (creation-form-finalized-p constructor) t)))
          ((not (creation-form-finalized-p constructor))
           (error 'circular-dependencies-in-load-time-value-form
                  :form (creation-form constructor))))))

(defmethod scan-literal-object (object system)
  (multiple-value-bind (constructor present-p)
      (constructor object)
    (cond ((not present-p)
           (let ((constructor
                   (multiple-value-bind (creation-form initialization-form)
                       (make-load-form-using-client object system)
                     (make-constructor creation-form initialization-form system))))
             (setf (constructor object) constructor)
             (scan-hir (creation-thunk constructor) system)
             (setf (creation-form-finalized-p constructor) t)
             (scan-hir (initialization-thunk constructor) system)
             ;; Attempt to coalesce this constructor, i.e., replace it with
             ;; an existing constructor of a similar object.
             (loop for equalp-key in (equalp-keys object system) do
               (coalesce object equalp-key))))
          ((not (creation-form-finalized-p constructor))
           (error 'circular-dependencies-in-creation-form
                  :object object
                  :creation-form (creation-form constructor))))))
