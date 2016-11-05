(cl:in-package #:asdf-user)

;;;; Conceptual summary:

;;;; Lisp has OPTIMIZE declarations for expressing that some
;;;; lexical block should be compiled with an emphasis on "speed",
;;;; and stuff like that. That's nice, but in the compiler's view,
;;;; vague. This system converts OPTIMIZE information into specific
;;;; and actionable "policies" for the rest of the compiler.

;;;; Policies are separate objects computed from the optimize info.
;;;; A policy consists of several "qualities", which are just a
;;;; symbol naming it and a value. Qualities are computed from the
;;;; generic function COMPUTE-POLICY-QUALITY, which is called from
;;;; the overall COMPUTE-POLICY function.

;;;; What policies exist is defined by the POLICY-QUALITIES
;;;; generic function. Cleavir defines several of its own policies
;;;; with DEFINE-CLEAVIR-COMPILER-POLICY, but implementations can
;;;; as well for their own compiler transforms. POLICY-QUALITIES's
;;;; return value has the same format as
;;;; CLEAVIR-ENV:OPTIMIZE-QUALITIES, and an APPEND method combo.

;;;; Every AST and instruction stores the policy that was computed
;;;; for its lexical region. (This means that policies should be
;;;; kept cached.) When a compiler transform wants to know whether
;;;; it should do something to such an object, it checks the
;;;; POLICY-VALUE for whatever it's doing.

;;;; For implementors:

;;;; 1) If you have your own policies, return something from
;;;;    POLICY-QUALITIES when specified on your global environment.
;;;;    If you don't have your own policies don't sweat it.
;;;; 2) Define a method for COMPUTE-POLICY-QUALITY specialized on
;;;;    each policy quality (including cleavir's) and your global
;;;;    environment. Example:
#+(or)
(defmethod cleavir-policy:compute-policy-quality
    ((name (eql 'cleavir-typed-transforms:insert-type-checks))
     optimize (env sys:global-environment))
  (= (cleavir-policy:optimize-value optimize 'safety) 3))
;;;; 3) Make sure your environments respect the optimize info
;;;;    protocols in cleavir/environment. You can use
;;;;    COMPUTE-POLICY to compute policies from optimize decls, but
;;;;    you should avoid doing this on every optimize-info call.

(defsystem :cleavir-compilation-policy
  :depends-on (:cleavir-environment :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "policy")
   (:file "define-policy")
   (:file "optimize")
   (:file "compute")))
