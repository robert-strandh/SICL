(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    (client (ast ast:fixnump-ast) lexical-environment)
  `(typep
    ,(translate-ast client (ast:object-ast ast) lexical-environment)
    'fixnum))

(defmethod translate-ast
    (client (ast ast:fixnum-add-ast) lexical-environment)
  `(progn
     (+ ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
        ,(translate-ast client (ast:arg2-ast ast) lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    (client (ast ast:fixnum-sub-ast) lexical-environment)
  `(progn
     (- ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
        ,(translate-ast client (ast:arg2-ast ast) lexical-environment))
     ;; We don't expect an overflow during bootstrapping.
     t))

(defmethod translate-ast
    (client (ast ast:fixnum-multiply-ast) lexical-environment)
  `(values
    0
     (* ,(translate-ast client (ast:multiplier-ast ast) lexical-environment)
        ,(translate-ast client (ast:multiplicand-ast ast) lexical-environment))))

(defmethod translate-ast
    (client (ast ast:fixnum-divide-ast) lexical-environment)
  `(floor
    (* ,(translate-ast client (ast:multiplier-ast ast) lexical-environment)
       ,(translate-ast client (ast:multiplicand-ast ast) lexical-environment))))

(defmethod translate-ast
    (client (ast ast:fixnum-less-ast) lexical-environment)
  `(< ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-not-greater-ast) lexical-environment)
  `(<= ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-greater-ast) lexical-environment)
  `(> ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-not-less-ast) lexical-environment)
  `(>= ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
       ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-equal-ast) lexical-environment)
  `(= ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
      ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-logand-ast) lexical-environment)
  `(logand ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-logior-ast) lexical-environment)
  `(logior ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-logxor-ast) lexical-environment)
  `(logxor ,(translate-ast client (ast:arg1-ast ast) lexical-environment)
           ,(translate-ast client (ast:arg2-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:fixnum-lognot-ast) lexical-environment)
  `(lognot ,(translate-ast client (ast:arg-ast ast) lexical-environment)))
