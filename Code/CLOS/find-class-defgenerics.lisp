(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_c.htm#find-class
(defgeneric find-class (symbol &optional errorp environment))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_find_c.htm#find-class
(defgeneric (setf find-class) (new-class symbol &optional errorp environment))
