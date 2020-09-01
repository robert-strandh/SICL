(cl:in-package #:sicl-cons)

(defmacro pushnew (item place
		   &environment environment
		   &rest args
		   &key
		   (key nil key-p)
		   (test nil test-p)
		   (test-not nil test-not-p))
  (let* ((global-env (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-env)))
    (pushnew-expander client item place environment args
                      key key-p
                      test test-p
                      test-not test-not-p)))
