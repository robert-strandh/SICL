(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_reinit.htm#reinitialize-instance
(defgeneric reinitialize-instance
    (instance &rest initargs &key &allow-other-keys))
