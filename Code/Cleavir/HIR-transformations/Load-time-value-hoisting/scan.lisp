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
       (scan-datum datum system)))
   hir))

;;; One thing to keep in mind is that it is entirely permissible for a
;;; Cleavir datum to appear as a literal object in a file.  So it is
;;; important to clearly distinguish between the constructor of a datum and
;;; the constructor of a literal object.  I am writing this down here
;;; because I already forgot about this issue twice.

(defmethod scan-datum ((immediate-input cleavir-ir:immediate-input) system)
  (values))

(defmethod scan-datum ((constant-input cleavir-ir:constant-input) system)
  (let ((value (cleavir-ir:value constant-input)))
    (unless (immedate-p value system)
      (scan-literal-object value system))))

(defmethod scan-datum ((load-time-value-input cleavir-ir:load-time-value-input) system)
  (multiple-value-bind (constructor present-p) (constructor load-time-value-input)
    (cond ((not present-p)
           (let* ((creation-form (cleavir-ir:form load-time-value-input))
                  (creation-thunk (compile-form creation-form system))
                  (constructor (make-instance 'constructor
                                 :creation-form creation-form
                                 :creation-thunk creation-thunk)))
             (setf (constructor load-time-value-input) constructor)
             (scan-hir creation-thunk system)
             (setf (creation-form-finalized-p constructor) t)))
          ((not (creation-form-finalized-p constructor))
           (error 'circular-dependencies-in-load-time-value-form
                  :form (creation-form constructor))))))

(defmethod scan-literal-object (object system)
  (multiple-value-bind (constructor present-p) (constructor object)
    (cond ((not present-p)
           (let* ((constructor (make-constructor object system)))
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
