(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'and)) rest)
  (loop for type-spec in rest
	always (generic-typep object type-spec)))

(defmethod typep-compound (object (head (eql 'eql)) rest)
  (eql object (first rest)))

(defmethod typep-compound (object (head (eql 'member)) rest)
  (member object rest))

(defmethod typep-compound (object (head (eql 'not)) rest)
  (assert (= (length rest) 1))
  (not (generic-typep object (first rest))))

(defmethod typep-compound (object (head (eql 'or)) rest)
  (loop for type-spec in rest
	when (generic-typep object type-spec)
	  return t))

(defmethod typep-compound (object (head (eql 'satisfies)) rest)
  (assert (= (length rest) 1))
  (funcall (sicl-genv:fdefinition (first rest) (sicl-genv:global-environment))
	   object))
