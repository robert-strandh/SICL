(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:fixnump-ast) lexical-environment)
  `(typep
    ,(translate-ast (ast:object-ast ast) lexical-environment)
    'fixnum))

(defmethod translate-ast
    ((ast ast:fixnum-add-ast) lexical-environment)
  `(progn
     (+ ,(translate-ast (ast:arg1-ast ast) lexical-environment)
        ,(translate-ast (ast:arg2-ast ast) lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    ((ast ast:fixnum-sub-ast) lexical-environment)
  `(progn
     (- ,(translate-ast (ast:arg1-ast ast) lexical-environment)
        ,(translate-ast (ast:arg2-ast ast) lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    ((ast ast:fixnum-multiply-ast) lexical-environment)
  `(values
    0
     (* ,(translate-ast (ast:multiplier-ast ast) lexical-environment)
        ,(translate-ast (ast:multiplicand-ast ast) lexical-environment))))

(defmethod translate-ast
    ((ast ast:fixnum-divide-ast) lexical-environment)
  `(floor
    (* ,(translate-ast (ast:multiplier-ast ast) lexical-environment)
       ,(translate-ast (ast:multiplicand-ast ast) lexical-environment))))

(defmethod translate-ast
    ((ast ast:fixnum-less-ast) lexical-environment)
  `(< ,(translate-ast (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-not-greater-ast) lexical-environment)
  `(<= ,(translate-ast (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-greater-ast) lexical-environment)
  `(> ,(translate-ast (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-not-less-ast) lexical-environment)
  `(>= ,(translate-ast (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-equal-ast) lexical-environment)
  `(= ,(translate-ast (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logand-ast) lexical-environment)
  `(logand ,(translate-ast (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logior-ast) lexical-environment)
  `(logior ,(translate-ast (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logxor-ast) lexical-environment)
  `(logxor ,(translate-ast (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-lognot-ast) lexical-environment)
  `(lognot ,(translate-ast (ast:arg-ast ast) lexical-environment)))
