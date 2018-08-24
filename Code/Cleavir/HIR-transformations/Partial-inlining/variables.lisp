(cl:in-package #:cleavir-partial-inlining)

;;; This variable contains the original ENTER-INSTRUCTION of the
;;; function being inlined.  It is used to determine whether a
;;; reference to a lexical location in the function being inlined is
;;; owned by that function, or whether, on the contrary, it is a
;;; reference to a lexical location in some ancestor function.

(defvar *original-enter-instruction*)

;;; This variable contains an EQ hash table mapping locations to
;;; owners.  Recall that the owner of a location is the
;;; ENTER-INSTRUCTION of the outermost function that refers to the
;;; location.

(defvar *location-ownerships*)

;;; This variable contains a mapping from instructions in the
;;; original callee to copies previously created during this inlining.
;;; Unlike the lexical variable mappings, it is global.

(defvar *instruction-mapping*)

;;; This variable contains an EQ hash table mapping instructions to
;;; owners.

(defvar *instruction-ownerships*)

;;; This variable contains the ENTER instruction of the function
;;; being inlined into.

(defvar *target-enter-instruction*)

;;; True (the default) if we should copy sub functions,
;;; as required in general.

(defvar *copy-functions*)

;;; True (the default) if we should copy locations, as required
;;; in general.

(defvar *copy-locations*)
