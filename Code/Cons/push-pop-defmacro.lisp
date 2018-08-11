(cl:in-package #:sicl-cons)

(defmacro push (item place &environment env)
  (push-expander item place env))

(defmacro pop (place &environment env)
  (pop-expander place env))
