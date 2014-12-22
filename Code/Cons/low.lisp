(cl:in-package #:sicl-cons)

(defun car (list)
  (declare (type list list))
  (if (consp list)
      (cleavir-primop:car list)
      nil))

(defun cdr (list)
  (declare (type list list))
  (if (consp list)
      (cleavir-primop:cdr list)
      nil))
  
(defun rplaca (cons object)
  (declare (type cons cons))
  (cleavir-primop:rplaca cons object)
  cons)

(defun rplacd (cons object)
  (declare (type cons cons))
  (cleavir-primop:rplacd cons object)
  cons)
