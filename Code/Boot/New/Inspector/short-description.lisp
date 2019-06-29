(cl:in-package #:sicl-boot-inspector)

(defgeneric short-description (object))

(defmethod short-description (object)
  (format nil "[an instance of ~s]" (class-of object)))

(defmethod short-description ((object sicl-boot-phase-3::header))
  (cond ((typep (slot-value object 'sicl-boot-phase-3::%class)
                'sicl-boot-phase-3::header)
         "[a pure ersatz object]")
        (t
         "[an impure ersatz object")))
