(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_chg_cl.htm#change-class
(defgeneric change-class (instance new-class &key &allow-other-keys))
