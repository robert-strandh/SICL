(cl:in-package #:sicl-cons)

(defmacro pushnew (item place
		   &environment env
		   &rest args
		   &key
		   (key nil key-p)
		   (test nil test-p)
		   (test-not nil test-not-p))
  (pushnew-expander item place env args
                    key key-p
                    test test-p
                    test-not test-not-p))
