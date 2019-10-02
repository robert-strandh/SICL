(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_init_i.htm#initialize-instance
(defgeneric initialize-instance
    (instance &rest initargs &key &allow-other-keys))
