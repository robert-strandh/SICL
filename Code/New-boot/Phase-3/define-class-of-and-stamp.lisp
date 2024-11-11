(cl:in-package #:sicl-new-boot-phase-3)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defparameter *trace-stamp* nil)

(defun define-class-of-and-stamp (client e2 e3)
  (flet ((find-class-e3 (class-name)
           (let ((f (clo:fdefinition client e3 'find-class)))
             (funcall f class-name)))
         (unique-number (class-in-e2)
           (let ((f (clo:fdefinition
                     client e2 @clostrophilia:unique-number)))
             (funcall f class-in-e2))))
    (flet ((local-class-of (object)
             (cond ((integerp object)
                    (find-class-e3 'fixnum))
                   ((typep object 'ratio)
                    (find-class-e3 'ratio))
                   ((null object)
                    (find-class-e3 'null))
                   ((symbolp object)
                    (find-class-e3 'symbol))
                   ((characterp object)
                    (find-class-e3 'character))
                   ((consp object)
                    (find-class-e3 'cons))
                   ((typep object 'sb:header)
                    (sb:class object))
                   (t (error "Don't know how to take the class of ~s"
                             object)))))
      (setf (clo:fdefinition client e3 'class-of) #'local-class-of)
      (setf (clo:fdefinition client e3 @clostrophilia:stamp)
            (lambda (object)
              (when *trace-stamp*
                (format *trace-output*
                        "In E3, taking the stamp of ~s~%"
                        object))
              (let ((result (unique-number (local-class-of object))))
                (when *trace-stamp*
                  (format *trace-output*
                          "In E3, stamp of ~s is ~s~%"
                          object
                          result))
                result))))))
