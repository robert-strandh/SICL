(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_shared.htm#shared-initialize
(defgeneric shared-initialize
    (instance slot-names &rest initargs &key &allow-other-keys))
