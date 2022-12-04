(cl:in-package #:cleavir-cst-to-ast)

(defmethod acclimation:report-condition
    ((condition incorrect-number-of-arguments)
     stream
     (language acclimation:english))
  (let ((form (cst:raw (cst condition)))
        (min (expected-min condition)) (max (expected-max condition)))
    (format stream
            "In the form ~s, ~s is used with ~d arguments, ~@
             but expects ~@?."
            form (first form) (observed condition)
            (cond ((and min max) "between ~d and ~d")
                  (min "at least ~d")
                  (max "at least ~d")
                  ;; If we hit here it's actually a bug.
                  (t "otherwise"))
            min max)))

(defmethod acclimation:report-condition
    ((condition lambda-call-first-symbol-not-lambda)
     stream
     (language acclimation:english))
  (format stream
          "Lambda call form was used with the malformed lambda block~@
          first argument instead of the symbol LAMBDA. The following~@
          form was found:~@
          ~s"
          (cst:raw (cst condition))))

;; Helper for below.
(defmethod acclimation:report-condition
  ((condition too-many-arguments)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with too many arguments:~%~s~@
          Expected at most ~d,"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))
          (expected-max condition)))

(defmethod acclimation:report-condition
  ((condition not-enough-arguments)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with too few arguments:~%~s~@
          Expected at least ~d,"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))
          (expected-min condition)))

(defmethod acclimation:report-condition
  ((condition odd-keyword-portion)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with an odd number of arguments in the keyword portion:~%~s"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))))

;; Display the type declaration that informed us of the problem.
(defmethod acclimation:report-condition :after
    ((condition argument-mismatch-warning)
     stream
     (language acclimation:english))
  (format stream
          " as determined from the function's type,~%~s"
          (callee-ftype condition)))

;; NOTE: In the future, there may be other ways to signal this.
(defmethod acclimation:report-condition :after
    ((condition argument-mismatch-style-warning)
     stream
     (language acclimation:english))
  (format stream
          " as inferred from its inline definition."))

(defmethod acclimation:report-condition
  ((condition macroexpansion-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition macroexpansion-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition macroexpansion-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
    ((condition circular-dependencies-in-creation-form)
     stream
     (language acclimation:english))
  (with-accessors ((object object)
                   (creation-form creation-form))
      condition
    (format stream
            "The creation form ~S~@
             of the object ~S~@
             contains a circular dependency."
            creation-form object)))

(defmethod acclimation:report-condition
    ((condition object-not-externalizable)
     stream
     (language acclimation:english))
  (with-accessors ((object object)) condition
    (format stream
            "The object ~S is not externalizable."
            object)))
