(in-package #:cleavir-type-descriptors)

;;;; This is the main file, linking the others.

;;;; The CL type system is too big. For Kildall, we need a
;;;; semilattice, and specifically a lattice meeting an ascending
;;;; chain condition. This means that there can be no infinite
;;;; sequence of distinct Tn such that T1 <: T2 <: ...
;;;; (Where <: means subtype.)
;;;; There are several ways to construct such sequences with CL
;;;; types, such as (MEMBER 1) <: (MEMBER 1 2) <: ...
;;;; or (OR X) <: (OR X Y) <: (OR X Y Z) <: ...
;;;; Therefore, for type inference we add an "approximation" step,
;;;; in which type declarations from the coder or wherever else are
;;;; squeezed into a lattice structure more amenable to analysis.
;;;; We call members of this lattice "descriptors".

;;;; There are several distinct classes of descriptors, because
;;;; life is difficult.
;;;; * lattice-descriptor defines the basic lattice for CL values.
;;;;   After Baker ("A decision procedure for CL's SUBTYPEP predicate")
;;;;   the space of values is broken up into several kingdoms etc.,
;;;;   which lets us represent descriptors as bit-vectors, so cool.
;;;; * values-descriptor defines a distinct lattice for values
;;;;   types. Values types appear exclusively from single-value
;;;;   types at HIR level, so the lattices really are distinct.
;;;; * function-descriptor defines a lattice for single value types
;;;;   that are complicated function types, i.e. including argument
;;;;   and/or return types. This lattice is part of
;;;;   lattice-descriptor: meet between lattice and function
;;;;   descriptors is defined, for example. In that case the
;;;;   function descriptor is further approximated into a lattice
;;;;   descriptor. It's set up like this because function types are
;;;;   generally only function types. Function types are inherently
;;;;   recursive (include other types) so they can't be included in
;;;;   a basic lattice structure.
;;;; * unboxed-descriptor defines technically-a-lattice for unboxed
;;;;   value types. These are not included in lattice-descriptor.
;;;;   Because of how HIR is structured, these are super limited.
;;;; * eql-descriptor defines an extention to lattice-descriptor
;;;;   for constant values. Meet/whatever between eql and lattice
;;;;   descriptors just approximates the eql.

;;;; Most of the lattice operations depend on a global environment.
;;;; Ideally type macros (from deftype) should be expanded at this
;;;; point, but beyond that, a few aspects of the type system are
;;;; implementation-dependent. Specifically
;;;; * extended-char may or may not be empty. If it is we don't
;;;;   want to give it a lattice position.
;;;; * Float types may be collapsed together.
;;;; * Array and complex upgrading. Each complex type gets a
;;;;   lattice position, and each array gets two (simple and not).
;;;; * (and array (not simple-array)) could be empty, but we don't
;;;;   actually notice that right now.
;;;; * Some implementations include nonstandard disjoint subtypes
;;;;   of SEQUENCE or NUMBER. We avoid dealing with this by using
;;;;   only the implementation SUBTYPEP.

;;;; Anyway the actual stuff in this file just deals with the
;;;; cross-class stuff above.

;;;; We also handle caching. When specifiers are turned into
;;;; descriptors the cache is checked. Specifier to descriptor will
;;;; probably remain the slowest part of this system; as of this
;;;; writing the general case involves ~20 SUBTYPEP calls.

;;; Only single-value types.
(defun approximate-type (specifier environment cache)
  (multiple-value-bind (value presentp) (gethash specifier cache)
    (if presentp
        value
        (setf (gethash specifier cache)
              ;; This is the only place we actually look at the
              ;; form of a specifier, rather than using SUBTYPEP.
              (if (consp specifier)
                  (case (car specifier)
                    ;; Because of how the program is structured,
                    ;; this case should indicate a bug rather than
                    ;; user error.
                    (values (error "bad context for VALUES"))
                    (function
                     (destructuring-bind (lambda-list rvalues)
                         (rest specifier)
                       (make-function-descriptor
                        lambda-list rvalues)))
                    (eql (assert (and (consp (cdr specifier))
                                      (null (cddr specifier))))
                     (make-eql (second specifier)))
                    (member
                     (if (and (consp (cdr specifier))
                              (null (cddr specifier)))
                         (make-eql (second specifier))
                         (specifier->ltype specifier environment)))
                    (otherwise
                     (specifier->ltype specifier environment)))
                  (specifier->ltype specifier environment))))))

(defun approximate-values
    (required optional rest environment cache)
  (flet ((approx (spec)
           ;; we stick with ltypes in values descriptors to make
           ;; the dependencies a little easier. FIXME?
           (coerce-to-ltype
            (approximate-type spec environment cache)
            environment cache)))
    (values->descriptor (mapcar #'approx required)
                        (mapcar #'approx optional)
                        (approx rest)
                        environment)))

;;; like the above, but taking descriptors instead of specifiers.
(defun make-values-descriptor
    (required optional rest environment cache)
  (flet ((fix (descriptor)
           (coerce-to-ltype descriptor environment cache)))
    (values->descriptor (mapcar #'fix required)
                        (mapcar #'fix optional)
                        (fix rest)
                        environment)))

(defun coerce-to-ltype (descriptor environment cache)
  (etypecase descriptor
    (ltype descriptor)
    (function-descriptor
     (approximate-type 'function environment cache))
    (eql-descriptor
     (object-ltype (eql-descriptor-object descriptor)
                   environment))))

(defun binary-join (descriptor1 descriptor2 environment cache)
  (etypecase descriptor1
    (ltype
     (ltype-join descriptor1
                 (coerce-to-ltype descriptor2 environment cache)))
    (values-descriptor (assert (values-descriptor-p descriptor2))
     (values-binary-join descriptor1 descriptor2 environment))
    (unboxed-descriptor (assert (equal descriptor1 descriptor2))
     descriptor1)
    (eql-descriptor
     (etypecase descriptor2
       (ltype
        (ltype-join
         (coerce-to-ltype descriptor1 environment cache)
         descriptor2))
       (function-descriptor
        (ltype-join
         (coerce-to-ltype descriptor1 environment cache)
         (coerce-to-ltype descriptor2 environment cache)))
       (eql-descriptor
        (if (eql (eql-descriptor-object descriptor1)
                 (eql-descriptor-object descriptor2))
            descriptor1
            (ltype-join
             (coerce-to-ltype descriptor1 environment cache)
             (coerce-to-ltype descriptor2 environment cache))))))
    (function-descriptor
     (etypecase descriptor2
       (ltype
        (ltype-join (coerce-to-ltype descriptor1 environment cache)
                    descriptor2))
       (eql-descriptor
        (ltype-join (coerce-to-ltype descriptor1 environment cache)
                    (coerce-to-ltype descriptor2 environment cache)))
       (function-descriptor
        (function-binary-join
         descriptor1 descriptor2 environment))))))

(defun binary-meet (descriptor1 descriptor2 environment cache)
  (etypecase descriptor1
    (ltype
     (ltype-meet descriptor1
                 (coerce-to-ltype descriptor2 environment cache)))
    (values-descriptor (assert (values-descriptor-p descriptor2))
     (values-binary-meet descriptor1 descriptor2 environment))
    (unboxed-descriptor (assert (equal descriptor1 descriptor2))
     descriptor1)
    (eql-descriptor
     (etypecase descriptor2
       (ltype
        (ltype-meet
         (coerce-to-ltype descriptor1 environment cache)
         descriptor2))
       (function-descriptor
        (ltype-meet
         (coerce-to-ltype descriptor1 environment cache)
         (coerce-to-ltype descriptor2 environment cache)))
       (eql-descriptor
        (if (eql (eql-descriptor-object descriptor1)
                 (eql-descriptor-object descriptor2))
            descriptor1
            (ltype-bottom environment)))))
    (function-descriptor
     (etypecase descriptor2
       (ltype
        (ltype-meet (coerce-to-ltype descriptor1 environment cache)
                    descriptor2))
       (eql-descriptor
        (ltype-meet (coerce-to-ltype descriptor1 environment cache)
                    (coerce-to-ltype descriptor2 environment cache)))
       (function-descriptor
        (function-binary-meet
         descriptor1 descriptor2 environment))))))

(defun sub-descriptor-p (descriptor1 descriptor2 environment cache)
  (etypecase descriptor1
    (values-descriptor (assert (values-descriptor-p descriptor2))
     (sub-values-p descriptor1 descriptor2 environment))
    (ltype
     (etypecase descriptor2
       (ltype (subltypep descriptor1 descriptor2))
       ;; technically FUNCTION is a subtype of (FUNCTION * *).
       ;; However, (a) the way Kildall is organized the latter type
       ;; will never be inferred, and (b) this function is only
       ;; used for Kildall in which returning NIL here is ok.
       (function-descriptor nil)
       ;; all ltypes are infinite, therefore
       (eql-descriptor nil)))
    (eql-descriptor
     (etypecase descriptor2
       ((or ltype function-descriptor) t) ; logic as above
       (eql-descriptor (eql (eql-descriptor-object descriptor1)
                            (eql-descriptor-object descriptor2)))))
    (function-descriptor
     (etypecase descriptor2
       (ltype (subltypep
               (coerce-to-ltype descriptor1 environment cache)
               descriptor2))
       (eql-descriptor nil) ; logic as above
       (function-descriptor
        (sub-function-p descriptor1 descriptor2 environment))))
    (unboxed-descriptor (assert (equal descriptor1 descriptor2))
     t)))

(defun top-p (descriptor environment)
  (etypecase descriptor
    (ltype (ltype-top-p descriptor environment))
    (eql-descriptor nil)
    (function-descriptor nil)
    (unboxed-descriptor nil)))
(defun bottom-p (descriptor environment)
  (etypecase descriptor
    (ltype (ltype-bottom-p descriptor environment))
    (eql-descriptor nil)
    (function-descriptor nil)
    (unboxed-descriptor nil)))

(defun bottom (environment)
  (ltype-bottom environment))

;;; Sometimes returns (unboxed whatever), so it's not a CL type
;;; specifier.
(defun descriptor->specifier (descriptor env)
  (etypecase descriptor
    (ltype (ltype->specifier descriptor env))
    (values-descriptor (values-descriptor->type descriptor env))
    (function-descriptor
     (function-descriptor->type descriptor env))
    (eql-descriptor descriptor)
    (unboxed-descriptor descriptor)))
