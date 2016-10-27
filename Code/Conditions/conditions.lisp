(cl:in-package #:sicl-conditions)

(define-condition condition () ())

(defparameter *language* 'english)

(defgeneric report-condition (condition stream language))

(defmethod print-object ((condition condition) stream)
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t))
      (report-condition condition stream *language*)))

(defmethod report-condition ((condition condition)
                             stream
                             (language (eql 'english)))
  (declare (ignore condition))
  (format stream
          "A condition of type CONDITION was signaled."))

(define-condition serious-condition (condition) ())

(defmethod report-condition ((condition serious-condition)
                             stream
                             (language (eql 'english)))
  (format stream
          "A condition of type SERIOUS-CONDITION was signaled."))

(define-condition storage-condition (serious-condition) ())

(defmethod report-condition ((condition storage-condition)
                             stream
                             (language (eql 'english)))
  (format stream
          "A condition of type STORAGE-CONDITION was signaled."))

(define-condition simple-condition (condition) ())

(defmethod report-condition ((condition simple-condition)
                             stream
                             (language (eql 'english)))
  (format stream
          "A condition of type SIMPLE-CONDITION was signaled."))

(define-condition error (serious-condition) ())

(defmethod report-condition ((condition error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type ERROR was signaled."))

(define-condition simple-error (simple-condition error) ())

(defmethod report-condition ((condition simple-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type SIMPLE-ERROR was signaled."))

(define-condition cell-error (error)
  ((%name :initarg :name :reader cell-error-name)))

(defmethod report-condition ((condition cell-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "A error of type CELL-ERROR was signaled.~@
           The celle name was ~a"
          (cell-name condition)))

(define-condition unbound-slot (cell-error)
  ((%instance :initarg :instance :reader unbound-slot-instance)))

(defmethod report-condition ((condition unbound-slot)
                             stream
                             (language (eql 'english)))
  (format stream
          "The slot named ~a of the class instance ~s is unbound."
          (cell-name condition)
          (unbound-slot-instance condition)))

(define-condition unbound-variable (cell-error) ())

(defmethod report-condition ((condition unbound-variable)
                             stream
                             (language (eql 'english)))
  (format stream
          "The variable named ~a is unbound."
          (cell-name condition)))

(define-condition undefined-function (cell-error) ())

(defmethod report-condition ((condition undefined-function)
                             stream
                             (language (eql 'english)))
  (format stream
          "The function named ~a is undefined."
          (cell-name condition)))

(define-condition parse-error (error) ())

(defmethod report-condition ((condition parse-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type PARSE-ERROR was signaled."))

(define-condition reader-error (parse-error) ())

(defmethod report-condition ((condition reader-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type READER-ERROR was signaled."))

(define-condition arithmetic-error (error)
  ((%operation :initarg :operation :reader arithmetic-error-operation)
   (%operands :initarg :operands :reader arithmetic-error-operands)))

(defmethod report-condition ((condition arithmetic-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An arithmetic error has occurred.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition division-by-zero (arithmetic-error) ())

(defmethod report-condition ((condition division-by-zero)
                             stream
                             (language (eql 'english)))
  (format stream
          "A division by zero error has occurred.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition floating-point-inexact (arithmetic-error) ())

(defmethod report-condition ((condition floating-point-inexact)
                             stream
                             (language (eql 'english)))
  (format stream
          "A floating-point-inexact error has occurred.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition floating-point-invalid-operation (arithmetic-error) ())

(defmethod report-condition ((condition floating-point-invalid-operation)
                             stream
                             (language (eql 'english)))
  (format stream
          "An invalid floating-point operation was attempted.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition floating-point-overflow (arithmetic-error) ())

(defmethod report-condition ((condition floating-point-overflow)
                             stream
                             (language (eql 'english)))
  (format stream
          "A floating-point overflow has occurred.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition floating-point-underflow (arithmetic-error) ())

(defmethod report-condition ((condition floating-point-underflow)
                             stream
                             (language (eql 'english)))
  (format stream
          "A floating-point underflow has occurred.~@
           The operation was ~a and the operands were ~a."
          (arithmetic-error-operation condition)
          (arithmetic-error-operands condition)))

(define-condition control-error (error) ())

(defmethod report-condition ((condition control-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type CONTROL-ERROR was signaled."))

(define-condition stream-error (error)
  ((%stream :initarg :stream :reader stream-error-stream)))

(defmethod report-condition ((condition stream-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type STREAM-ERROR was signaled.~@
           The stream was ~s"
          (stream-error-stream condition)))

(define-condition end-of-file (stream-error) ())

(defmethod report-condition ((condition end-of-file)
                             stream
                             (language (eql 'english)))
  (format stream
          "An end-of-file error has occured on the stream ~a"
          (stream-error-stream condition)))

(define-condition file-error (error)
  ((%pathname :initarg :pathname :reader file-error-pathname)))

(defmethod report-condition ((condition file-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type FILE-ERROR was signaled.
           The pathname was ~s"
          (file-error-pathname condition)))

(define-condition package-error (error)
  ((%package :initarg :package :reader package-error-package)))

(defmethod report-condition ((condition package-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type PACKAGE-ERROR was signaled.
           The package was ~s"
          (package-error-package condition)))

(define-condition print-not-readable (error)
  ((%object :initarg :object :reader print-not-readable-object)))

(defmethod report-condition ((condition print-not-readable)
                             stream
                             (language (eql 'english)))
  (format stream
          "An attemps was made to print an unreadable object~@
           while *print-readably* was true.~@
           The object was ~s"
          (print-not-readable-object condition)))

(define-condition program-error (error) ())

(defmethod report-condition ((condition program-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "An error of type PROGRAM-ERROR was signaled."))

(define-condition type-error (error)
  ((%datum :initarg :datum :reader type-error-datum)
   (%expected-type :initarg :expected-type :reader type-error-expected-type)))

(defmethod report-condition ((condition type-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "The object ~s is not of the expected type ~s."
          (type-error-datum condition)
          (type-error-expected-type condition)))

(define-condition simple-type-error (simple-condition type-error) ())

(defmethod report-condition ((condition simple-type-error)
                             stream
                             (language (eql 'english)))
  (format stream
          "The object ~s is not of the expected type ~s."
          (type-error-datum condition)
          (type-error-expected-type condition)))


(define-condition warning (condition) ())

(defmethod report-condition ((condition warning)
                             stream
                             (language (eql 'english)))
  (format stream
          "A warning of type WARNING was signaled."))

(define-condition simple-warning (simple-condition warning) ())

(defmethod report-condition ((condition simple-warning)
                             stream
                             (language (eql 'english)))
  (format stream
          "A warning of type SIMPLE-WARNING was signaled."))

(define-condition style-warning (warning) ())

(defmethod report-condition ((condition style-warning)
                             stream
                             (language (eql 'english)))
  (format stream
          "A warning of type STYLE-WARNING was signaled."))
