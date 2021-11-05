(cl:in-package #:cleavir-ast-visualizer)


(defgeneric label (ast))

;;; The default label is the lower-case version of the name of the
;;; class (as a string) with suffix -ast stripped off. 
(defmethod label (ast)
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

(defmethod label ((ast cleavir-ast:literal-ast))
  (let* ((label (format nil "~s" (cleavir-ast:value ast)))
         (length (length label)))
    (subseq label 0 (min length 30))))

(defmethod label ((ast cleavir-ast:lexical-ast))
  (format nil "~a" (cleavir-ast:name ast)))

(defmethod label ((ast cleavir-ast:tag-ast))
  (format nil "~a" (cleavir-ast:name ast)))

(defmethod label ((ast cleavir-ast:aref-ast))
  (format nil "~:[non-simple~;simple~] aref ~s"
          (cleavir-ast:simple-p ast)
          (cleavir-ast:element-type ast)))

(defmethod label ((ast cleavir-ast:aset-ast))
  (format nil "~:[non-simple~;simple~] aset ~s"
          (cleavir-ast:simple-p ast)
          (cleavir-ast:element-type ast)))

(defmethod label ((ast cleavir-ast:the-ast))
  (format nil "the (values ~{~s ~}~@[&optional ~{~s ~}~]&rest ~s)"
          (cleavir-ast:required-types ast)
          (cleavir-ast:optional-types ast)
          (cleavir-ast:rest-type ast)))

(defmacro deflabel (ast label)
  `(defmethod label ((ast ,ast))
     (declare (ignorable ast))
     ,label))

(deflabel cleavir-ast:fdefinition-ast "fdef")
(deflabel cleavir-ast:load-time-value-ast "l-t-v")
(deflabel cleavir-ast:bind-ast "bind")

(deflabel cleavir-ast:float-add-ast "float +")
(deflabel cleavir-ast:float-sub-ast "float -")
(deflabel cleavir-ast:float-mul-ast "float *")
(deflabel cleavir-ast:float-div-ast "float /")
(deflabel cleavir-ast:float-less-ast "float <")
(deflabel cleavir-ast:float-not-greater-ast "float <=")
(deflabel cleavir-ast:float-equal-ast "float =")
(deflabel cleavir-ast:float-not-less-ast "float >=")
(deflabel cleavir-ast:float-greater-ast "float >")
(deflabel cleavir-ast:float-sin-ast "float sin")
(deflabel cleavir-ast:float-cos-ast "float cos")
(deflabel cleavir-ast:float-sqrt-ast "float sqrt")
