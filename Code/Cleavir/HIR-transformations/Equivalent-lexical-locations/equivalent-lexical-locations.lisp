(cl:in-package #:cleavir-equivalent-lexical-locations)

(defun set-equality (set1 set2 test)
  (and (null (set-difference set1 set2 :test test))
       (null (set-difference set2 set1 :test test))))

(defun class-equality (class1 class2)
  (set-equality class1 class2 #'eq))

(defun partition-equality (partition1 partition2)
  (set-equality partition1 partition2 #'class-equality))
