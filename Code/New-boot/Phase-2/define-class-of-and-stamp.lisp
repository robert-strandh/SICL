(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defparameter *trace-stamp* nil)

(defun define-class-of-and-stamp (client e1 e2)
  (flet ((find-class-e2 (class-name)
           (let ((f (clo:fdefinition client e2 'find-class)))
             (funcall f class-name)))
         (unique-number (class-in-e2)
           (let* ((f (clo:fdefinition
                      client e1 @clostrophilia:unique-number))
                  (result (funcall f class-in-e2)))
             result)))
    (flet ((local-class-of (object)
             (cond ((integerp object)
                    (find-class-e2 'fixnum))
                   ((typep object 'ratio)
                    (find-class-e2 'ratio))
                   ((null object)
                    (find-class-e2 'null))
                   ((symbolp object)
                    (find-class-e2 'symbol))
                   ((characterp object)
                    (find-class-e2 'character))
                   ((consp object)
                    (find-class-e2 'cons))
                   ((typep object 'sb:header)
                    (sb:class object))
                   (t (error "Don't know how to take the class of ~s"
                             object)))))
      (setf (clo:fdefinition client e2 'class-of) #'local-class-of)
      (setf (clo:fdefinition client e2 @clostrophilia:stamp)
            (lambda (object)
              (when *trace-stamp*
                (format *trace-output*
                        "In E2, taking the stamp of ~s~%"
                        object))
              (let ((result (unique-number (local-class-of object))))
                (when *trace-stamp*
                  (format *trace-output*
                          "In E2, stamp of ~s is ~s~%"
                          object
                          result))
                result))))))
