(in-package #:sicl-package)

(defmethod acclimation:report-condition ((c not-a-package-designator)
                                          stream
                                          (language acclimation:english))
  (format stream
          "A package designator was required,~@
           but the following was given:~@
           ~s"
          (type-error-datum c)))
