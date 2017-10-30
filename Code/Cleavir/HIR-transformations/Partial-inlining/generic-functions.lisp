(cl:in-package #:cleavir-partial-inlining)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INLINE-ONE-INSTRUCTION.

;;; In the following description, the terms INPUT and OUTPUT refer to
;;; data in HIR code.  If needed, we use the terms ARGUMENTS and
;;; VALUES to refer to objects supplied to and received from function
;;; calls.
;;;
;;; The first input to CALL-INSTRUCTION is a temporary that is the
;;; output of ENCLOSE-INSTRUCTION, and ENCLOSE-INSTRUCTION has
;;; ENTER-INSTRUCTION as its input.  Furthermore, ENCLOSE-INSTRUCTION
;;; is the immediate predecessor of CALL-INSTRUCTION.
;;;
;;; We know that no other call-instruction has the same first input as
;;; CALL-INSTRUCTION, so we are free to mutate CALL-INSTRUCTION and
;;; ENTER-INSTRUCTION to add more inputs and outputs respectively.
;;; When this is done, new inputs and outputs are added to the end of
;;; the existing list.
;;;
;;; MAPPING is a mapping that takes an instruction or a datum in the
;;; callee and maps it to an instruction or a datum in the caller,
;;; provided such a correspondence has been added to the mapping.
;;;
;;; Let A be the instruction immediately following ENTER-INSTRUCTION.
;;;
;;; If A maps to some instruction B in MAPPING, no copy of it is made,
;;; and the empty list is returned from the call to
;;; INLINE-ONE-INSTRUCTION.  This case is handled by an :AROUND method
;;; on INLINE-ONE-INSTRUCTION.
;;;
;;; Otherwise, a copy B of A is made.  The inputs of A must either be
;;; in MAPPING, or they refer to variables in an enclosing function.
;;; So for each input of A, MAPPING is consulted, and if the input is
;;; in the mapping, the translation is used in B.  Otherwise, the same
;;; input as that in A is also used in B.  An output of A can one of
;;; three kinds.  It can be in MAPPING, in which case the translated
;;; version is used in B.  If it is not in MAPPING, it can be an
;;; output owned by an enclosing function, in which case it is used as
;;; is in B.  Finally, it can be a new variable, in which case it is
;;; added to MAPPING as described below.
;;;
;;; The side effects of this function is to add B as an instruction
;;; preceding ENCLOSE-INSTRUCTION, and to add A->B as a new entry to
;;; MAPPING.  Furthermore, for each new entry OA->OB added to MAPPING,
;;; OB is added as a new input to CALL-INSTRUCTION and OA as a new
;;; input to ENTER-INSTRUCTION.
;;;
;;; This function returns a list of WORKLIST-ITEMs, one for each
;;; successor of the instruction A.  Each element of the list contains
;;; an ENCLOSE-INSTRUCTION, a CALL-INSTRUCTION, an ENTER-INSTRUCTION,
;;; and a MAPPING.
;;;
;;; If the instruction A has no successors, the empty list is returned
;;; from INLINE-ONE-INSTRUCTION.
;;;
;;; If the instruction A has a single successor, the arguments can be
;;; handled as follows.  ENCLOSE-INSTRUCTION is used as is, with no
;;; modifications.  CALL-INSTRUCTION is modified to have additional
;;; inputs.  ENTER-INSTRUCTION is modified to have additional outputs,
;;; and to have the successor of A as its new successor.  MAPPING is
;;; modified to contain additional correspondence
;;;
;;; If the instruction A has more than one successor, the arguments
;;; can be handled as follows.  For one successor of A, the arguments
;;; can be reused as for the case of a single successor.  For the
;;; other successor, copies must me made as follows.  Every additional
;;; successor of A, a copy of the arguments must be created.  The copy
;;; of the ENCLOSE-INSTRUCTION is inserted in the corresponding
;;; successor of the instruction B in the caller.  A copy of the
;;; CALL-INSTRUCTION is inserted after the copied
;;; ENCLOSE-INSTRUCTION..  An additional datum is created to hold the
;;; output of the copied ENCLOSE-INSTRUCTION and the first input of
;;; the copied CALL-INSTRUCTION.  The copied ENCLOSE-INSTRUCTION has
;;; the copied ENTER-INSTRUCTION as its input.  The copied
;;; ENTER-INSTRUCTION has the corresponding successor of A as its
;;; successor.

(defgeneric inline-one-instruction
  (enclose-instruction call-instruction enter-instruction mapping))
