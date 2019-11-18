(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_update.htm#update-instance-for-different-class
(defgeneric update-instance-for-different-class
    (previous current
     &rest initargs
     &key
     &allow-other-keys))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm#update-instance-for-redefined-class
(defgeneric update-instance-for-redefined-class
    (instance added-slots discarded-slots property-list
     &rest initargs
     &key
     &allow-other-keys))
