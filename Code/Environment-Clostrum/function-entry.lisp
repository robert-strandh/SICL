(cl:in-package #:sicl-environment)

(defgeneric call-sites (function-entry))

(defgeneric (setf call-sites) (call-sites function-entry))

(stealth-mixin:define-stealth-mixin
    function-entry () clostrum-basic::function-entry
  ((%call-sites :initform '() :accessor call-sites)))

(defgeneric add-call-site (client environment call-site name))

(defmethod add-call-site (client environment call-site name)
  (let ((entry (clostrum-basic::ensure-function-entry name environment)))
    (push call-site (call-sites entry))))
