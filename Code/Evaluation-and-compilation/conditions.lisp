(cl:in-package #:sicl-evaluation-and-compilation)

(define-condition environment-must-be-omitted-or-nil (error)
  ((%environment :initarg :environment :reader environment))
  (:report (lambda (condition stream)
             (format stream
                     "The optional environment argument must either be~@
                      omitted or NIL, but the following was found instead:~@
                      ~s"
                     (environment condition)))))

(define-condition load-time-value-read-only-p-not-evaluated
    (style-warning)
  ((%code :initarg :code :reader code))
  (:report (lambda (condition stream)
             (format stream
                     "The second (optional) argument (read-only-p)~@
                      is not evaluated, so a boolean value (T or NIL)~@
                      was expected. But the following was found instead:~@
                      ~s"
                     (code condition)))))

(define-condition deprecated-eval-when-situation
    (style-warning)
  ((%situation :initarg :situation :reader situation))
  (:report (lambda (condition stream)
             (format stream
                     "A deprecated evaluation situation given:~@
                      ~s"
                     (situation condition)))))
