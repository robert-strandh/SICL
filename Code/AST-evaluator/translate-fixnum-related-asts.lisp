(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:fixnump-ast) global-environment lexical-environment)
  `(typep
    ,(translate-ast (ast:object-ast ast)  global-environment lexical-environment)
    'fixnum))

(defmethod translate-ast
    ((ast ast:fixnum-add-ast) global-environment lexical-environment)
  `(progn
     (+ ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
        ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    ((ast ast:fixnum-sub-ast) global-environment lexical-environment)
  `(progn
     (- ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
        ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    ((ast ast:fixnum-multiply-ast) global-environment lexical-environment)
  `(values
    0
     (* ,(translate-ast (ast:multiplier-ast ast)  global-environment lexical-environment)
        ,(translate-ast (ast:multiplicand-ast ast)  global-environment lexical-environment))))

(defmethod translate-ast
    ((ast ast:fixnum-divide-ast) global-environment lexical-environment)
  `(floor
    (* ,(translate-ast (ast:multiplier-ast ast)  global-environment lexical-environment)
       ,(translate-ast (ast:multiplicand-ast ast)  global-environment lexical-environment))))

(defmethod translate-ast
    ((ast ast:fixnum-less-ast) global-environment lexical-environment)
  `(< ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
      ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-not-greater-ast) global-environment lexical-environment)
  `(<= ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
       ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-greater-ast) global-environment lexical-environment)
  `(> ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
      ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-not-less-ast) global-environment lexical-environment)
  `(>= ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
       ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-equal-ast) global-environment lexical-environment)
  `(= ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
      ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logand-ast) global-environment lexical-environment)
  `(logand ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
           ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logior-ast) global-environment lexical-environment)
  `(logior ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
           ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-logxor-ast) global-environment lexical-environment)
  `(logxor ,(translate-ast (ast:arg1-ast ast)  global-environment lexical-environment)
           ,(translate-ast (ast:arg2-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:fixnum-lognot-ast) global-environment lexical-environment)
  `(lognot ,(translate-ast (ast:arg-ast ast)  global-environment lexical-environment)))
