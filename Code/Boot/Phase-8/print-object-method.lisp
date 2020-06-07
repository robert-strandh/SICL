(cl:in-package #:sicl-boot-phase-8)

(defmethod print-object ((object sicl-boot::header) stream)
  (handler-case
      (funcall (sicl-genv:fdefinition 'print-object sicl-boot::*e5*)
               object
               stream)
    (error () (format stream "[unprintable]")))
  (format stream
          (if (typep (slot-value object 'sicl-boot::%class)
                     'sicl-boot::header)
              "[pure]"
              "[impure]")))

(defmethod print-object ((object sicl-boot::header) stream)
  (let ((class (slot-value object 'sicl-boot::%class)))
    (if (typep class 'sicl-boot::header)
        (let* ((class-rack (slot-value class 'sicl-boot::%rack))
               (class-name (aref class-rack 5)))
          (format stream "[~s pure]" class-name))
        (format stream "[impure]"))))
