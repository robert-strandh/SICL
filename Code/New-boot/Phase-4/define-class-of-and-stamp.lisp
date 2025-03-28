(cl:in-package #:sicl-new-boot-phase-4)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defparameter *trace-stamp* nil)

(defun define-class-of-and-stamp (client e3 e4)
  (flet ((find-class-e4 (class-name)
           (let ((f (clo:fdefinition client e4 'find-class)))
             (funcall f class-name)))
         (unique-number (class-in-e3)
           (let ((f (clo:fdefinition
                     client e3 @clostrophilia:unique-number)))
             (funcall f class-in-e3))))
    (flet ((local-class-of (object)
             (cond ((integerp object)
                    (find-class-e4 'fixnum))
                   ((typep object 'ratio)
                    (find-class-e4 'ratio))
                   ((typep object 'single-float)
                    (find-class-e4 'single-float))
                   ((typep object 'double-float)
                    (find-class-e4 'double-float))
                   ((null object)
                    (find-class-e4 'null))
                   ((symbolp object)
                    (find-class-e4 'symbol))
                   ((characterp object)
                    (find-class-e4 'character))
                   ((stringp object)
                    (find-class-e4 'string))
                   ((consp object)
                    (find-class-e4 'cons))
                   ((typep object 'sb:header)
                    (sb:class object))
                   (t (error "Don't know how to take the class of ~s"
                             object)))))
      (setf (clo:fdefinition client e4 'class-of) #'local-class-of)
      (setf (clo:fdefinition client e4 @clostrophilia:stamp)
            (lambda (object)
              (when *trace-stamp*
                (format *trace-output*
                        "In E4, taking the stamp of ~s~%"
                        object))
              (let ((result (if (typep object 'sb:header)
                                (sb:stamp object)
                                (unique-number (local-class-of object)))))
                (when *trace-stamp*
                  (format *trace-output*
                          "In E4, stamp of ~s is ~s~%"
                          object
                          result))
                result))))))
                
