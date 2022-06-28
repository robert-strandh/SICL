(cl:in-package #:sicl-boot-arithmetic)

(defclass environment (sicl-boot:environment)
  ())

(defclass client (sicl-boot:client) ())

;;; Here, we need to include every standard arithmetic function that
;;; is defined as a generic function in SICL, but where the host
;;; version of the function is imported during bootstrapping, so that
;;; it is present in environment E5.
(defparameter *arithmetic-function-names*
  '(realp
    rationalp
    numerator
    denominator
    numberp
    floatp
    expt
    sicl-arithmetic:binary-add +
    sicl-arithmetic:binary-subtract -
    sicl-arithmetic:binary-multiply *
    sicl-arithmetic:binary-divide /
    sicl-arithmetic:binary-equal =
    sicl-arithmetic:binary-less <
    sicl-arithmetic:binary-not-greater <= > >=
    sicl-arithmetic:binary-gcd gcd
    sicl-arithmetic:binary-lcm lcm
    sicl-arithmetic:binary-logand logand
    sicl-arithmetic:binary-logior logior
    sicl-arithmetic:binary-logxor logxor
    logandc1 logandc2
    logorc1 logorc2
    lognand lognor
    sicl-arithmetic:generic-ceiling ceiling
    sicl-arithmetic:generic-floor floor
    sicl-arithmetic:generic-round round
    sicl-arithmetic:generic-truncate truncate
    max min
    plusp minusp))

(defvar *environment*)

(defmethod env:fdefinition
    ((client client)
     (environment sicl-boot-phase-5:environment)
      function-name)
  (if (member function-name *arithmetic-function-names* :test #'equal)
      ;; Then query the environment dedicated to this phase instead.
      (env:fdefinition client *environment* function-name)
      ;; Else do the default thing.
      (call-next-method)))

(defmethod (setf env:fdefinition)
    (new-function
     (client client)
     (environment sicl-boot-phase-5:environment)
     function-name)
  (if (member function-name *arithmetic-function-names* :test #'equal)
      ;; Then define it in the environment dedicated to this phase
      ;; instead.
      (setf (env:fdefinition client *environment* function-name)
            new-function)
      ;; Else do the default thing.
      (call-next-method)))

(defmethod cleavir-cst-to-ast:declaration-proclamations
    ((client sicl-boot:client) (environment sicl-boot-phase-5:environment))
  '(fast-generic-functions:method-properties))
