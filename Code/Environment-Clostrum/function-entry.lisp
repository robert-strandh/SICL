(cl:in-package #:sicl-environment)

(defgeneric call-sites (function-entry))

(defgeneric (setf call-sites) (call-sites function-entry))

(stealth-mixin:define-stealth-mixin
    function-entry () clostrum-basic::function-entry
  ((%call-sites :initform '() :accessor call-sites)))
