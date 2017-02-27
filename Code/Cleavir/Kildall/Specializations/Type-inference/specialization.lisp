(in-package #:cleavir-kildall-type-inference)

(defclass type-inference
    (cleavir-kildall:forward-single-traverse
     cleavir-kildall:map-pool-mixin
     cleavir-kildall:forward-traverse-interfunction)
  ())

(defmethod cleavir-kildall:entry-pool ((s type-inference) i)
  (declare (ignore i))
  (cleavir-kildall:empty-map-pool))

(defmethod cleavir-kildall:object-meet ((s type-inference) t1 t2)
  ;; normal descriptors join with binary-join
  ;; values descriptors join with values-binary-join
  ;; function descriptors join with function-binary-join
  ;; a normal and function descriptor are joined by discarding the
  ;;  function information (i.e. reducing it to FUNCTION)
  ;; values V non-values = error
  ;; unboxed V not-unboxed = error
  ;; unboxed V unboxed-but-different = error
  (typecase t1
    (symbol
     (etypecase t2
       (symbol (binary-join t1 t2))
       (cons (assert (function-descriptor-p t2))
        (binary-join t1 (approximate-type 'function)))))
    (cons
     (ecase (car t1)
       ((values) (assert (values-descriptor-p t2))
        (values-binary-join t1 t2))
       ((unboxed) (assert (equal t1 t2)) t1)
       ((function)
        (etypecase t2
          (symbol (binary-join (approximate-type 'function) t2))
          (cons (assert (function-descriptor-p t2))
           (function-binary-join t1 t2))))))))

(defmethod cleavir-kildall:object<= ((s type-inference) t1 t2)
  (typecase t1
    (symbol
     (etypecase t2
       (symbol (sub-descriptor-p t2 t1))
       (cons (assert (function-descriptor-p t2))
        (sub-descriptor-p (approximate-type 'function) t1))))
    (cons
     (ecase (car t1)
       ((function)
        (etypecase t2
          (symbol
           (sub-descriptor-p t2 (approximate-type 'function)))
          (cons (assert (function-descriptor-p t2))
           (sub-function-p t2 t1))))
       ((values) (assert (values-descriptor-p t2))
        (sub-values-p t2 t1))
       ((unboxed) (assert (equal t1 t2)) t)))))

;;; move this?
(defmethod cleavir-kildall-graphviz:draw-object
    ((s type-inference) object)
  (let ((desc
          (cond ((values-descriptor-p object)
                 (values-descriptor->type object))
                ((function-descriptor-p object)
                 (function-descriptor->type object))
                (t object))))
    (format nil "~a" desc)))
