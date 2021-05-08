(cl:in-package #:sicl-evaluation-and-compilation)

(defmethod acclimation:report-condition
    ((condition environment-must-be-omitted-or-nil)
     stream
     (language acclimation:english))
  (format stream
          "The optional environment argument must either be~@
           omitted or NIL, but the following was found instead:~%~s"
          (environment condition)))
