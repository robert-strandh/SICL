(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Coalescing of Similar Objects

(defclass similarity-table ()
  ;; The EQL table is used to ensure that each object is only scanned once.
  ;; This works because if two objects are EQL, they are also similar in
  ;; the sense of CLHS 3.2.4.2.2.
  ((%eql-table :initform (make-hash-table :test #'eq) :reader eql-table)
   ;; The EQUAL and EQUALP tables are used to coalesce literal objects
   ;; where the rules of similarity are less restrictive, e.g., specialized
   ;; arrays or hash tables.
   (%equal-table :initform (make-hash-table :test #'equal) :reader equal-table)
   (%equalp-table :initform (make-hash-table :test #'equalp) :reader equalp-table)))

(defun make-similarity-table ()
  (make-instance 'similarity-table))

(defmacro constant-record-cache (object)
  `(gethash ,object (eql-table *similarity-table*)))

(defun coalesce (client object constant-record)
  (with-accessors ((eql-table eql-table)
                   (equal-table equal-table)
                   (equalp-table equalp-table)) *similarity-table*
    (multiple-value-bind (equal-keys equalp-keys)
        (similarity-keys client object)
      (flet ((coalesce-using-table (key table)
               (multiple-value-bind (similar-constant-record present-p)
                   (gethash key table)
                 (if present-p
                     (setf (gethash object eql-table) similar-constant-record)
                     (setf (gethash key table) constant-record)))))
        (dolist (equal-key equal-keys)
          (coalesce-using-table equal-key equal-table))
        (dolist (equalp-key equalp-keys)
          (coalesce-using-table equalp-key equalp-table))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant Records

(defclass constant-record ()
  (;; The lexical AST that holds the result of calling the creation form.
   (%lexical-ast :initarg :lexical-ast :reader lexical-ast)
   (%creation-form :initarg :creation-form :reader creation-form)
   (%creation-form-finalized-p :initform nil :accessor creation-form-finalized-p)))

(defgeneric constant-lexical-ast (client object environment))

(defmethod constant-lexical-ast :around (client object environment)
  (multiple-value-bind (constant-record present-p)
      (constant-record-cache object)
    (cond ((not present-p)
           (call-next-method))
          (present-p
           (unless (creation-form-finalized-p constant-record)
             (error 'circular-dependencies-in-creation-form
                    :object object
                    :creation-form (creation-form constant-record)))
           (lexical-ast constant-record)))))

(defmethod constant-lexical-ast (client object environment)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form-using-client client object environment)
    (let* ((lexical-ast
             (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
               :name (gensym "C")))
           (constant-record
            (make-instance 'constant-record
              :lexical-ast lexical-ast
              :creation-form creation-form)))
      (setf (constant-record-cache object)
            constant-record)
      (push (cleavir-ast:make-ast 'cleavir-ast:setq-ast
              :lhs-ast lexical-ast
              :value-ast (convert client (cst:cst-from-expression creation-form) environment))
            *prologue*)
      (setf (creation-form-finalized-p constant-record)
            t)
      (push (convert client (cst:cst-from-expression initialization-form) environment)
            *prologue*)
      (coalesce client object constant-record)
      lexical-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.

(defun convert-constant (client constant-cst environment)
  (let ((object (cst:raw constant-cst)))
    (cond ((not *use-file-compilation-semantics-p*)
           (cleavir-ast:make-ast 'cleavir-ast:constant-ast
             :value object))
          ((trivial-constant-p client object)
           (cleavir-ast:make-ast 'cleavir-ast:constant-ast
             :value object))
          (*use-file-compilation-semantics-p*
           (constant-lexical-ast
            client
            (cst:raw constant-cst)
            (trucler:global-environment client environment))))))
