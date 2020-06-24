(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPE-EXPAND.
;;;
;;; Performs type macroexpansion (macros being defined by DEFTYPE)
;;; in the given environment. Only top-level, but should expand
;;; repeatedly. That is to say, this is macroexpand, not
;;; macroexpand-1 or macroexpand-all.

(defgeneric type-expand (environment type-specifier))

(defmethod type-expand ((env entry) type-specifier)
  (type-expand (next env) type-specifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function HAS-EXTENDED-CHAR-P.
;;;
;;; Returns a boolean indicating whether the implementation has a
;;; non-empty EXTENDED-CHAR type. (An implementation may decide to
;;; have all characters be BASE-CHARs, in which case this returns
;;; NIL.)
;;; Used during type inference.

(defgeneric has-extended-char-p (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FLOAT-TYPES.
;;;
;;; Returns a list of floating point types available in the
;;; implementation, that is, a list with SHORT-FLOAT, SINGLE-FLOAT,
;;; DOUBLE-FLOAT, and LONG-FLOAT zero or one times each. If two of
;;; the types are the same, only one should be returned.
;;; For example, in an implementation where (subtypep short single)
;;; this might return (SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).
;;; Used during type inference.

(defgeneric float-types (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-COMPLEX-PART-TYPES.
;;;
;;; Returns a list of element types for distinct complex types in
;;; the implementation, e.g. (SINGLE-FLOAT DOUBLE-FLOAT REAL)
;;; Used during type inference.

(defgeneric upgraded-complex-part-types (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-ARRAY-ELEMENT-TYPES.
;;;
;;; Returns a list of element types for distinct array types in the
;;; implementation, e.g. the minimum is (BIT BASE-CHAR CHARACTER T)
;;; Used during type inference.

(defgeneric upgraded-array-element-types (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.
;;;
;;; As CL:FIND-CLASS, but a generic function with different
;;; parameters so as to allow client specialization.

(defgeneric find-class (name environment system &optional errorp)
  (:argument-precedence-order system environment name))

;;; Default method: Use CL:FIND-CLASS.
(defmethod find-class (name environment system &optional errorp)
  (declare (ignore system))
  (cl:find-class name errorp environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions PARSE-TYPE-SPECIFIER, PARSE-EXPANDED-TYPE-SPECIFIER,
;;; and PARSE-COMPOUND-TYPE-SPECIFIER.
;;;
;;; These functions serve to convert a type specifier into some
;;; object - a "ctype" - that is independent of the environment,
;;; and which can be used with the CLEAVIR-CTYPE protocol.
;;; The default behavior of the generic functions is designed to
;;; facilitate client implementations of the protocol, as well as
;;; to support a default Cleavir-provided implementation in which
;;; ctypes are CL type specifiers with environment-dependent
;;; parts removed, manipulated with CL:SUBTYPEP etc.
;;;
;;; Given a type specifier and environment, PARSE-TYPE-SPECIFIER
;;; expands the specifier using TYPE-EXPAND (above), and then
;;; calls PARSE-EXPANDED-TYPE-SPECIFIER.
;;;
;;; The default methods on PARSE-EXPANDED-TYPE-SPECIFIER are
;;; as follows:
;;; * If the specifier is a class, it is returned.
;;; * If the specifier is a symbol, it is looked up in the
;;;   environment using FIND-CLASS (above). If a class exists,
;;;   it is returned. Otherwise the specifier is returned.
;;; * If the specifier is a cons, PARSE-COMPOUND-TYPE-SPECIFIER
;;;   is called using the CAR and CDR, and its value is returned.
;;;
;;; PARSE-COMPOUND-TYPE-SPECIFIER handles specifiers with
;;; heads of AND, OR, NOT, CONS, ARRAY, SIMPLE-ARRAY, COMPLEX,
;;; MEMBER, SATISFIES, FUNCTION, and the REAL subtypes by
;;; calling the appropriate constructors in the CLEAVIR-CTYPE
;;; package. It is expected that clients will define other
;;; compound types using DEFTYPE, so they are not defined here.
;;;
;;; A client implementation of ctypes will probably need to
;;; override the behavior of PARSE-EXPANDED-TYPE-SPECIFIER on
;;; symbols and classes, as well as the CLEAVIR-CTYPE
;;; constructors.

(defun parse-type-specifier (type-specifier environment system)
  (parse-expanded-type-specifier
   (type-expand environment type-specifier)
   environment system))

(defgeneric parse-expanded-type-specifier
    (type-specifier environment system)
  (:argument-precedence-order system environment type-specifier))

(defmethod parse-expanded-type-specifier
    ((type-specifier symbol) environment system)
  (declare (ignore system))
  (or (find-class type-specifier environment system nil)
      type-specifier))

(defmethod parse-expanded-type-specifier
    ((type-specifier class) environment system)
  (declare (ignore environment system))
  type-specifier)

(defmethod parse-expanded-type-specifier
    ((type-specifier cons) environment system)
  (parse-compound-type-specifier
   (first type-specifier) (rest type-specifier) environment system))

(defgeneric parse-compound-type-specifier
    (head rest environment system)
  (:argument-precedence-order system environment head rest))

;;; Internal helper function
(defun parse-type-specifier-list (list environment system)
  (loop for spec in list
        collect (parse-type-specifier spec environment system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'and)) rest environment system)
  (apply #'cleavir-ctype:conjoin
         system
         (parse-type-specifier-list rest environment system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'or)) rest environment system)
  (apply #'cleavir-ctype:disjoin
         system
         (parse-type-specifier-list rest environment system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'not)) rest environment system)
  (destructuring-bind (under) rest
    (cleavir-ctype:negate
     (parse-type-specifier under environment system)
     system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'cons)) rest environment system)
  (destructuring-bind (&optional (car 't) (cdr 't)) rest
    (when (eql car '*) (setf car 't))
    (when (eql cdr '*) (setf cdr 't))
    (cleavir-ctype:cons
     (parse-type-specifier car environment system)
     (parse-type-specifier cdr environment system)
     system)))

;;: Internal helper.
(defun parse-array-type-specifier
    (simplicity element-type dimensions environment system)
  (cleavir-ctype:array
   (if (eql element-type '*)
       element-type
       (cleavir-ctype:upgraded-array-element-type
        (parse-type-specifier element-type environment system)
        system))
   dimensions simplicity system))

(defmethod parse-compound-type-specifier
    ((head (eql 'array)) rest environment system)
  (destructuring-bind (&optional (et '*) (dimensions '*)) rest
    (parse-array-type-specifier head et dimensions environment system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'simple-array)) rest environment system)
  (destructuring-bind (&optional (et '*) (dimensions '*)) rest
    (parse-array-type-specifier head et dimensions environment system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'complex)) rest environment system)
  (destructuring-bind (&optional (et '*)) rest
    (cleavir-ctype:complex
     (if (eql et '*)
         et
         (cleavir-ctype:upgraded-complex-part-type
          (parse-type-specifier et environment system)
          system))
     system)))

;;; Internal helper.
(defun parse-range (head rest system)
  (destructuring-bind (&optional (low '*) (high '*)) rest
    (cleavir-ctype:range head low high system)))

(macrolet ((defreal (head)
             `(defmethod parse-compound-type-specifier
                  ((head (eql ',head)) rest environment system)
                (declare (ignore environment))
                (parse-range head rest system)))
           (defreals (&rest heads)
             `(progn
                ,@(loop for head in heads
                        collect `(defreal ,head)))))
  (defreals integer rational real
    float short-float single-float double-float long-float))

(defmethod parse-compound-type-specifier
    ((head (eql 'member)) rest environment system)
  (declare (ignore environment))
  (apply #'cleavir-ctype:member system rest))

(defmethod parse-compound-type-specifier
    ((head (eql 'satisfies)) rest environment system)
  (declare (ignore environment))
  (destructuring-bind (fname) rest
    (cleavir-ctype:satisfies fname system)))

(defmethod parse-compound-type-specifier
    ((head (eql 'cl:function)) rest environment system)
  (destructuring-bind (&optional (arg '*) (value '*)) rest
    (multiple-value-call #'cleavir-ctype:function
      (if (eq arg '*)
          arg
          (parse-function-type-lambda-list arg environment system))
      (if (eq value '*)
          value
          (parse-values-type-specifier value environment system))
      system)))

(defun parse-function-type-lambda-list (lambda-list env system)
  ;; FIXME?: Use a parser generator.
  ;; NOTE: In the CLHS, part of the description doesn't mention
  ;; &allow-other-keys and other parts do. Given the actual
  ;; meaning of the specifier, describing calls, it seems
  ;; reasonable to allow &allow-other-keys after &key.
  (loop with state = nil
        with required with optional with rest with restp
        with keys with keyp with aokp
        for element in lambda-list
        do (case element
             ((&optional)
              ;; FIXME: Better errors for these
              (assert (member state '(nil)))
              (setf state element))
             ((&rest)
              (assert (member state '(nil &optional)))
              (setf state element restp t))
             ((&key)
              (assert (member state '(nil &optional after-&rest)))
              (setf state element keyp t))
             ((&allow-other-keys)
              (assert (member state '(&key)))
              (setf state element aokp t))
             (otherwise
              (ecase state
                ((nil)
                 (push (parse-type-specifier element env system)
                       required))
                ((&optional)
                 (push (parse-type-specifier element env system)
                       optional))
                ((&rest)
                 (setf rest (parse-type-specifier element env system)
                       state 'after-&rest))
                ((&key)
                 (destructuring-bind (key type) element
                   (push (list key
                               (parse-type-specifier type env system))
                         keys))))))
        finally
           ;; Make sure we don't have (... &rest)
           (assert (not (member state '(&rest))))
           (return (values (nreverse required)
                           (nreverse optional)
                           (if restp
                               rest
                               (parse-type-specifier 'nil env system))
                           keyp (nreverse keys) aokp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PARSE-VALUES-TYPE-SPECIFIER.
;;;
;;; Values types are not accepted by PARSE-TYPE-SPECIFIER due to
;;; their special semantics. This function handles them. If given
;;; a non-values type specifier, it will still return a values
;;; type specifier. The semantics of values ctypes are explained
;;; in the CLEAVIR-CTYPE package; this function properly applies
;;; the "fuzziness" required by THE, etc.
;;;
;;; A client with a different ctype implementation should
;;; specialize CLEAVIR-CTYPE:VALUES.

(defun parse-values-type-specifier (type-specifier
                                    environment system)
  (let ((spec (type-expand environment type-specifier)))
    (if (and (consp spec) (eql (car spec) 'values))
        (multiple-value-call #'cleavir-ctype:values
          (parse-values-type-lambda-list
           (rest spec) environment system)
          system)
        (cleavir-ctype:coerce-to-values
         (parse-expanded-type-specifier spec environment system)
         system))))

(defun parse-values-type-lambda-list (lambda-list env sys)
  (loop with state = nil
        with required with optional with rest with restp
        for element in lambda-list
        do (case element
             ((&optional)
              (assert (member state '(nil)))
              (setf state element))
             ((&rest)
              (assert (member state '(nil &optional)))
              (setf state element restp t))
             (t
              (let ((ctype (parse-type-specifier
                            element env sys)))
                (ecase state
                  ((nil) (push ctype required))
                  ((&optional) (push ctype optional))
                  ((&rest) (setf rest ctype state 'after-&rest))))))
        finally
           (assert (not (member state '(&rest))))
           (return (values (nreverse required) (nreverse optional)
                           ;; Apply "fuzziness". See the comment on
                           ;; CLEAVIR-CTYPE:VALUES for more info.
                           (if restp
                               rest
                               (parse-type-specifier 't env sys))))))
