(cl:in-package #:sicl-environment)

(defgeneric call-sites (operator-entry))

(defgeneric (setf call-sites) (call-sites operator-entry))

(stealth-mixin:define-stealth-mixin
    operator-entry () clostrum-basic::operator-entry
  ((%call-sites :initform '() :accessor call-sites)))

(defgeneric add-call-site (client environment call-site name))

(defmethod add-call-site (client environment call-site name)
  (let ((entry (clostrum-basic::ensure-operator-entry name environment)))
    (push call-site (call-sites entry))))
