(cl:in-package #:asdf-user)

;;;; In the Cleavir intermediate representation, an AST representing a
;;;; top-level form is compiled into FLOWCHART.  A flowchart is a
;;;; graph in which the nodes are INSTRUCTIONS and DATA.  There are
;;;; two types of arcs: CONTROL ARCS and DATA ACRS.
;;;;
;;;; A CONTROL ARC represents the flow of control in the flowchart,
;;;; and connects one instruction to another instruction.  If there is
;;;; a control arc from an instruction I to an instruction J in a
;;;; flowchart, then J is said to be A SUCCESSOR of I, and I is said
;;;; to be A PREDECESSOR of J.  An instruction can have zero, one, or
;;;; several successors.  Most instructions have a single successor.
;;;; Some instruction types such as RETURN instructions have no
;;;; successors.  Instruction types with two or more successors
;;;; represent some kind of test that can have more than one outcome.
;;;; An instruction can have zero, one, or several predecessors.  If
;;;; an instruction has no predecessors, then it is an INITIAL
;;;; INSTRUCTION of some subgraph of the flowchart, and then it can
;;;; only be reached by CALLING it, as opposed to by the ordinary flow
;;;; of control.  The entire flowchart is represented by such an
;;;; initial instruction.
;;;;
;;;; A DATA ARC represents the input to or the output from an
;;;; instruction.  A data arc with an instruction as its HEAD is an
;;;; INPUT ARC.  A data arc with an instruction as its TAIL is an
;;;; OUTPUT ARC.  The HEAD of an output arc is always a DATUM that can
;;;; be written to.  The TAIL of an input arc is usually a DATUM,
;;;; except that the input to an ENCLOSE instruction is the INITIAL
;;;; INSTRUCTION of some subgraph.  The output of that same ENCLOSE
;;;; instruction is a CLOSURE that, when called, transfers control to
;;;; the initial instruction that is the input of the enclose
;;;; instruction.
;;;;
;;;; An instruction J is said to be REACHABLE from some instruction I
;;;; if and only if there is a (possibly empty) sequence of control
;;;; arcs that corresponds to a path from I to J.
;;;;
;;;; The initial instructions of a flowchart form a TREE, called the
;;;; NESTING TREE.  The root of the tree is the initial instruction of
;;;; the flowchart, and the parent in the tree of some initial
;;;; instruction I other than the initial instruction of the flowchart
;;;; is the unique initial instruction from which I can be reached.
;;;; This tree is a mirror of the tree of functions in the source
;;;; code; functions defined by LAMBDA, FLET, or LABELS.
;;;;
;;;; The NESTING depth of initial instruction I is the the depth of I
;;;; in the nesting tree.
;;;;
;;;; A PROCEDURE P corresponding to some initial instruction I is a
;;;; set of instructions that are reachable from I, but that are not
;;;; reachable from any parent of I in the nesting tree.  The
;;;; instruction I is called the INITIAL INSTRUCTION OF P.
;;;;
;;;; By extension, the nesting depth of some procedure P is the
;;;; nesting depth of the initial instruction of P, and the nesting
;;;; depth of an INSTRUCTION I is the nesting depth of the procedure
;;;; of which I is a member.  Furthermore, by extension we use the
;;;; terms PARENT and ANCESTOR about procedures as well as initial
;;;; instructions with the obvious meaning.
;;;;
;;;; Clearly, given the definitions above, for every control arc A,
;;;; the HEAD of A can not have a greater nesting depth than the TAIL
;;;; of A.  However, there are control arcs for which the nesting
;;;; depth of the head is smaller than the nesting dept of the tail.
;;;; Such control arcs are the result of RETURN-FROM and GO special
;;;; operators.
;;;;
;;;; A procedure P is said to be CONTROL DEPENDENT on some procedure Q
;;;; if Q is an ancestor of P in the nesting tree and there is a
;;;; control arc from some instruction belonging to P to some
;;;; instruction belonging to Q.  A procedure P is said to be CONTROL
;;;; INDEPENDENT if and only if it is control dependent on no other
;;;; procedure.  Each control independent procedures will be turned
;;;; into a code object in itself, whereas other procedures will share
;;;; code with the procedures they depend on.  Notice that it is
;;;; possible for a procedure P to be control dependent, but not on
;;;; its direct parent.  The parent of P can thus be control
;;;; independent, whereas P will share code with some ancestor of its
;;;; parent.

(defsystem :cleavir-ir
  :depends-on (:cleavir-attributes :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "datum")
   (:file "instruction")
   (:file "graph-modifications")
   (:file "instruction-mixin-classes")
   (:file "graph-instructions")
   (:file "graphviz-drawing")
   (:file "map-instructions-arbitrary-order")
   (:file "set-predecessors")
   (:file "map-instructions")
   (:file "map-local-instructions")))
