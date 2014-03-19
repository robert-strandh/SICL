(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm#find-method
(defgeneric find-method
    (generic-function method-qualifiers specializers &optional errorp))
