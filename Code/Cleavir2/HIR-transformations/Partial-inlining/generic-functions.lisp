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
;;; ENTER-INSTRUCTION as its input.
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
;;; SUCCESSOR-INSTRUCTION is the instruction immediately following
;;; ENTER-INSTRUCTION.  Let us call it A for short.
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

(defgeneric inline-one-instruction (enclose-instruction
                                    call-instruction
                                    enter-instruction
                                    successor-instruction
                                    mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INLINE-FUNCTION.
;;;
;;; Inline the function that starts with ENTER-INSTRUCTION, so that
;;; the body of that function replaces CALL-INSTRUCTION.
;;;
;;; Initial-instruction is the initial instruction of the top-level
;;; HIR program.  It is used in order to compute ownership of lexical
;;; locations in the program.
;;;
;;; This function starts by creating an enclose-instruction that
;;; precedes CALL-INSTRUCTION, and temporary lexical location that
;;; connects the output of that enclose-instruction to the first input
;;; of CALL-INSTRUCTION.  It also creates a copy of
;;; ENCLOSE-INSTRUCTION to be used exclusively during the inlining
;;; procedure.
;;;
;;; Furthermore, it creates temporary lexical locations to hold the
;;; inputs to CALL-INSTRUCTION corresponding to the outputs of
;;; ENTER-INSTRUCTION.  These temporary lexical locations represent
;;; the initial environment of the function to be inlined, so for each
;;; such location, a correspondence from the output of
;;; ENTER-INSTRUCTION to the new input of CALL-INSTRUCTION is entered
;;; into MAPPING.
;;;
;;; Then an initial WORK-LIST-ITEM is created that contains the newly
;;; created enclose-instruction, CALL-INSTRUCTION, the newly created
;;; enter-instruction, CALL-INSTRUCTION, and MAPPING.
;;;
;;; Finally, this function enters into a loop, in each iteration
;;; processing a WORKLIST-ITEM by calling INLINE-ONE-INSTRUCTION on
;;; the data stored in the WORKLIST-ITEM.  The loop ends when the
;;; worklist is empty.
;;;

(defgeneric inline-function (initial-instruction
                             call-instruction
                             enter-instruction
                             mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COPY-INSTRUCTION.
;;;
;;; Create a copy of an instruction for the purpose of inlining.
;;; The copy has predecessors and successors of NIL - the caller must
;;; perform the hookup into the graph.
;;;
;;; This is an internal function only concerned with inlining-specific
;;; copying requirements. Clients should work with their own
;;; instructions via the more general CLEAVIR-IR:CLONE-INSTRUCTION.

(defgeneric copy-instruction (instruction mapping))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for managing mappings.

;;; Return the object that FROM is mapped to in MAPPING.  If FROM does
;;; not appear in MAPPING, then NIL is returned.
(defgeneric find-in-mapping (mapping from))

;;; Destructively add a new item to MAPPING that makes FROM correspond
;;; to TO.  If FROM already appears in MAPPING, an error is signaled.
(defgeneric add-to-mapping (mapping from to))

;;; Return a fresh copy of MAPPING.
(defgeneric copy-mapping (mapping))
