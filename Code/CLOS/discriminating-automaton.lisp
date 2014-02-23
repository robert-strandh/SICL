(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Discriminating automaton.
;;;
;;; The discriminating automaton is used to construct a discriminating
;;; function.  As such, it is not used during normal execution of
;;; application code, but only when the discriminating function of
;;; some generic function needs to be recomputed.
;;;
;;; The structure of the discriminating automaton encodes a series of
;;; tests to determine an effective method, given the unique class
;;; numbers of the specialized required parameters of the generic
;;; function.  At the moment, the automaton always encodes tests from
;;; left to right.  In other words, the unique class number of the
;;; leftmost specialized parameter is tested first.  It might be
;;; advantageous for some configuration of method specializers to test
;;; in a different order, but we do not consider that possibility at
;;; the moment.
;;;
;;; Each state of the automaton can be considered to belong to a
;;; particular LAYER.  For a generic function with N specialized
;;; required parameters, the corresponding automaton has N+1 layers,
;;; numbered from 0 to N.  Layer 0 consists of a single state, namely
;;; the initial state.  Layer N consists of final states, each
;;; corresponding to a particular effective method.
;;; 
;;; For any particular sequence of N unique class numbers, the
;;; automaton will make exactly N transitions.  It might be possible
;;; in some cases for an automaton to make fewer transitions.  As an
;;; example, consider a generic function with the following method
;;; specializers (all classes): (A T B) (A T C) (D E F).  If the first
;;; test determines that the class of the first required argument is
;;; A, then only the first two methods are potentially applicable.
;;; Neither of the first two methods specialize on the second required
;;; parameter, so in this case, the class of the second required
;;; argument does not need to be consulted in order to determine which
;;; of the first two methods is applicable.  As a consequence, if the
;;; class of the first required argument is A in this case, then it
;;; would be possible for the automaton to make only 2 transitions.
;;; However, this kind of optimization would require detailed
;;; knowledge about the particular subclasses of generic functions and
;;; methods in question, as well as of the way the methods on
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES work for those
;;; subclasses.  For now, then, we require no such detailed knowledge.
;;; The only information we use is that a particular sequence of
;;; unique numbers of classes should trigger the execution of a
;;; particular effective method.
;;;
;;; The discriminating automaton is computed from the call history of
;;; the generic function.  Recall that the call history of the generic
;;; function consists of a set (represented as a list) of call caches.
;;; For the purpose of building the discriminating automaton, each
;;; call cache contains a class number cache and an effective method
;;; cache.  Several different call caches may share the same (EQ)
;;; effective method cache.  
;;;
;;; We represent the automaton as nested lists.  We deliberately avoid
;;; using classes so as to avoid circular dependencies.

;;; A STATE of the automaton is either a FINAL STATE or an INTERNAL
;;; state.  A final state has no transitions associated with it, and
;;; instead it has a TAG which is a non-NIL atom.  Final states with
;;; identical (EQ) tags are considered equivalent.  An internal state
;;; has a list of transitions associated with it.  During construction
;;; of the automaton, some internal states may have an empty list of
;;; transitions, but in the final automaton, every internal state has
;;; a non-empty list of transitions associated with it.  There can
;;; never be two transitions in a state with the same label (see
;;; below).  Two internal states with the same set of transitions
;;; (same number of transitions, same labels, leading to equivalent
;;; states) are considered equivalent.  A state is represented as a
;;; CONS cell.  The CAR of the cell is initially NIL and will later be
;;; used to hold a GENSYMed NAME of the state.  If the state is a
;;; final state, then the CDR of the CONS cell contains the tag.  If
;;; the state is an internal state, the CDR of the CONS cell contains
;;; the list of transitions.

(defun make-internal-state (&optional transitions)
  (assert (listp transitions))
  (cons nil transitions))

(defun internal-state-p (state)
  (listp (cdr state)))

(defun state-transitions (state)
  (assert (internal-state-p state))
  (cdr state))

(defun (setf state-transitions) (new-transitions state)
  (assert (internal-state-p state))
  (setf (cdr state) new-transitions))

(defun make-final-state (tag)
  (assert (not (listp tag)))
  (cons nil tag))

(defun final-state-p (state)
  (not (listp (cdr state))))

(defun state-tag (state)
  (assert (final-state-p state))
  (cdr state))

(defun state-name (state)
  (car state))

;;; A transition of the automaton is represented as a CONS cell.  The
;;; CAR of the CONS cell is the label (the unique number of a class),
;;; and the CDR of the CONS cell is the target state.

(defun make-transition (label target)
  (cons label target))

(defun transition-label (transition)
  (car transition))

(defun transition-target (transition)
  (cdr transition))

(defun (setf transition-target) (target transition)
  (setf (cdr transition) target))

;;; A layer is a set of states.  We represent a layer as a non-empty
;;; list.  The first cell of the list is a sentinel and it always
;;; contains NIL in its CAR.

(defun make-layer ()
  (list nil))

(defun layer-states (layer)
  (cdr layer))

(defun (setf layer-states) (new-states layer)
  (setf (cdr layer) new-states))

;;; An automaton is represented as a list of N+1 layers.  When a new
;;; automaton is created, the initial state (with no transitions) is
;;; automatically created in layer 0.

(defun make-automaton (n+1)
  (let ((result (loop repeat n+1
		      collect (make-layer))))
    (push (make-internal-state) (layer-states (car result)))
    result))

;;; Given an automaton, a list of transition labels (of length N) and
;;; a tag of a final state, add states and transitions to the
;;; automaton, so that there exists a path from the initial state to a
;;; final state with the given tag.
;;;
;;; The automaton must not already contain a path from the initial
;;; state to a final state with the labels given, not even to a final
;;; state with the same tag as the one given. 
(defun add-path (automaton labels tag)
  (let ((state (car (layer-states (car automaton)))))
    (pop automaton)
    ;; Follow transitions in the automaton until we find a state where
    ;; there is no transition for the corresponding label.  Should we
    ;; end up in a final state (which should not happen), then the
    ;; call (state-transitions state) will fail because a final
    ;; state does not have any transitions. 
    (loop for label = (car labels)
	  for transitions = (state-transitions state)
	  for transition = (assoc label transitions)
	  until (null transition)
	  do (pop labels)
	     (pop automaton)
	     (setf state (transition-target transition)))
    ;; At this point, STATE is an internal state with no transition
    ;; with the first label in LABELS.  Now, we create new states
    ;; until right before we reach a final state.
    (loop until (null (cdr labels))
	  for label = (car labels)
	  for target = (make-internal-state)
	  for transition = (make-transition label target)
	  do (push transition (state-transitions state))
	     (push target (layer-states (car automaton)))
	     (pop labels)
	     (pop automaton)
	     (setf state target))
    ;; At this point, there is a single element left in LABELS,
    ;;  corresponding to a transition to a final state.  There is also
    ;;  a single element in AUTOMATON, namely the layer containing
    ;;  final states. A final state with the tag TAG may already
    ;;  exist, in which case we reuse it.  If not, we create a new
    ;;  final state.
    (assert (and (consp automaton) (null (cdr automaton))))
    (assert (and (consp labels) (null (cdr labels))))
    (let ((final (car (member tag (layer-states (car automaton))
			      :key #'state-tag))))
      (when (null final)
	;; There is no existing state with the tag TAG.  Create one
	;; and add it to the final layer of the automaton.
	(setf final (make-final-state tag))
	(push final (layer-states (car automaton))))
      ;; Add a transition from STATE to FINAL.
      (push (make-transition (car labels) final)
	    (state-transitions state)))))

;;; Compare two transitions for equality.  Two transitions are
;;; considered equal if they have the same label and if the target
;;; states are the same under EQ.
(defun transitions-equal (transition1 transition2)
  (and (eql (transition-label transition1)
	    (transition-label transition2))
       (eq (transition-target transition1)
	   (transition-target transition2))))

;;; Two states are equivalent if they have the same outgoing
;;; transitions, as determined by TRANSITION-EQUAL.
(defun states-equivalent-p (state1 state2)
  (unless (= (length (state-transitions state1))
	     (length (state-transitions state2)))
    (return-from states-equivalent-p nil))
  (setf (state-transitions state1)
	(sort (state-transitions state1) #'< :key #'transition-label))
  (setf (state-transitions state2)
	(sort (state-transitions state2) #'< :key #'transition-label))
  (every #'transitions-equal
	 (state-transitions state1)
	 (state-transitions state2)))

;;; If the target state of the transition is equivalent to some other
;;; state indicated by the alist, then change the transition to point
;;; to the equivalent state instead.
(defun adjust-transition (transition alist)
  (let* ((target (transition-target transition))
	 (equivalent-target (cdr (assoc target alist))))
    (unless (null equivalent-target)
      (setf (transition-target transition) equivalent-target))))

;;; Adjust all the outgoing transitions of a state so that if two
;;; target states of such transitions are equivalent, they are also
;;; EQ.
(defun adjust-state (state alist)
  (loop for transition in (state-transitions state)
	do (adjust-transition transition alist)))

;;; Minimize a layer.  The next layer has already been minimized, and
;;; the result of that operation was a dictionary (represented as an
;;; alist) where each key is a state that was removed and the
;;; corresponding value is a state that was retained and which is
;;; equivalent to the key.  The result of the minimization of the next
;;; layer is passed as an argument to this function.
;;;
;;; This function starts by updating the transitions of the current
;;; layer according to the dictionary of equivalent states in the next
;;; layer.  It then finds equivalent states in the current layer, adds
;;; them to a resulting dictionary and removes one of the states.
(defun minimize-layer (layer dico)
  ;; Start by traversing the layer and updating the transitions
  ;; according to DICO.
  (loop for state in (layer-states layer)
	do (adjust-state state dico))
  (let ((equivalences '()))
    ;; Now, for each state in the layer: First collect any equivalent
    ;; states that occur after the first one in the later.  Then
    ;; remove those equivalent states from the layer, using a
    ;; destructive operation. Finally add equivalences to the
    ;; resulting dictionary.  Since we are destructively modifying the
    ;; list of states of the layer, we are not allowed to use a list
    ;; traversal operation because then the result would be undefined,
    ;; so we use explicit list operations instead.
    (loop with remaining-states = (layer-states layer)
	  for state = (car remaining-states)
	  until (null (cdr remaining-states))
	  do (setf (cdr remaining-states)
		   (loop for s in (cdr remaining-states)
			 if (states-equivalent-p state s)
			   do (push (cons s state) equivalences)
			 else
			   collect s))
	     (pop remaining-states))
    equivalences))

(defun minimize-automaton (start-state)
  (let ((layers (compute-layers start-state)))
    (loop for rest = (reverse layers) then (cdr rest)
	  until (null (cdr rest))
	  do (adjust-layer (cadr rest) (minimize-layer (car rest)))))
  start-state)
