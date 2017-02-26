(in-package #:cleavir-kildall-type-inference)

(defclass type-inference (cleavir-kildall:forward-single-traverse
                          cleavir-kildall:map-pool-mixin)
  ())

(defmethod cleavir-kildall:entry-pool ((s type-inference) i)
  (declare (ignore i))
  (cleavir-kildall:empty-map-pool))

(defmethod cleavir-kildall:object-meet ((s type-inference) t1 t2)
  (typecase t1
    (symbol (assert (symbolp t2))
     (binary-join t1 t2))
    (cons
     (assert (consp t2))
     (ecase (car t1)
       ((values) (assert (eq (car t2) 'values))
        (values-binary-join t1 t2))
       ((unboxed) (assert (equal t1 t2)) t1)))))

(defmethod cleavir-kildall:object<= ((s type-inference) t1 t2)
  (typecase t1
    (symbol (assert (symbolp t2))
     (sub-descriptor-p t2 t1))
    (cons
     (assert (consp t2))
     (ecase (car t2)
       ((values) (assert (eq (car t2) 'values))
        (sub-values-p t2 t1))
       ((unboxed) (assert (equal t1 t2)) t)))))

;;; move this?
(defmethod cleavir-kildall-graphviz:draw-object
    ((s type-inference) object)
  (let ((desc
          (if (and (consp object) (eq (car object) 'values))
              (values-descriptor->type object)
              object)))
    (format nil "~a" desc)))
