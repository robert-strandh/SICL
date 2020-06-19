(cl:in-package #:sicl-package)

(defun resolve-conflict (new-symbol existing-symbol package)
  (restart-case (error 'symbol-conflict
                       :new-symbol new-symbol
                       :existing-symbol existing-symbol
                       :package)
    (shadow ()
      :report (lambda (stream)
                (format stream "Keep existing symbol."))
      (shadow existing-symbol package))
    (unintern
      :report (lambda (stream)
                (format stream "Replace existing symbol."))
      (unintern existing-symbol package))))
