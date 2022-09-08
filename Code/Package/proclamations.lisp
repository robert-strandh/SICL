(cl:in-package #:sicl-package)

;;; FIXME: The optional parameter is a package designator,
;;; and we should have a type definition for that somewhere.
(declaim (ftype (function (string &optional t)
                          (values symbol
                                  (member :inherited
                                          :external
                                          :internal
                                          nil)))
                find-symbol))
