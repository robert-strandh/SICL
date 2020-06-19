(cl:in-package #:sicl-package)

(defun package-designator-to-package (package-designator)
  (cond ((packagep package-designator)
         package-designator)
        ((typep package-designator 'string-designator)
         (find-package package-designator))
        (t
         (error 'not-a-package-designator :datum package-designator))))
