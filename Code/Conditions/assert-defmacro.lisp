(cl:in-package #:sicl-conditions)

;;; FIXME: Do this better
(defmacro assert (test &rest arguments)
  `(unless ,test (error "assertion ~s failed" ',test)))
