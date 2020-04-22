(cl:in-package #:asdf-user)

;;; This system contains some definitions from the "Evaluation and
;;; compilation" dictionary of the HyperSpec.  Most of the contents of
;;; that dictionary consists of special operators which do not have
;;; any associated definitions.  Some others have been placed in the
;;; Environment system because that is the logical place for them in
;;; SICL, since SICL has first-class global environments, and those
;;; definitions are directly related to the environment.

(defsystem :sicl-evaluation-and-compilation
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")))
