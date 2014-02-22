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
;;; automaton will make at most N transitions.  The automaton may make
;;; fewer than N transitions in some cases.  As an example, consider a
;;; generic function with the following method specializers (all
;;; classes): (A T B) (A T C) (D E F).  If the first test determines
;;; that the class of the first required argument is A, then only the
;;; first two methods are potentially applicable.  Neither of the
;;; first two methods specialize on the second required parameter, so
;;; in this case, the class of the second required argument does not
;;; need to be consulted in order to determine which of the first two
;;; methods is applicable.  As a consequence, if the class of the
;;; first required argument is A in this case, then the automaton will
;;; make only 2 transitions.
;;;
;;; The discriminating automaton is computed from the call history of
;;; the generic function.  We represent the automaton as nested lists.
;;; We deliberately avoid using classes so as to avoid circular
;;; dependencies.

;;; A state of the automaton is represented as a list of at least two
;;; elements, the NAME which is a gensym, and INFO which is typically
;;; NIL but can contain any information associated with the state.
;;; The remaining elements of the list represent transitions from this
;;; state to other states (see below for the representation of a
;;; transition).

(defun make-state (&optional info)
  (list (gensym) info))

(defun state-name (state)
  (car state))

(defun (setf state-name) (name state)
  (setf (car state) name))

(defun state-info (state)
  (cadr state))

(defun (setf state-info) (info state)
  (setf (cadr state) info))

(defun state-transitions (state)
  (cddr state))

(defun (setf state-transitions) (transitions state)
  (setf (cddr state) transitions))

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

;;; Add a path from the state STATE to the final state.  The path is a
;;; list of labels (unique numbers of classes).
(defun add-path (state path final-state)
  (if (null (cdr path))
      ;; By construction, we do not yet have a transition to the final
      ;; state, so no need to check for one.
      (push (make-transition (car path) final-state)
	    (state-transitions state))
      (let ((transition (find (car path)
			      (state-transitions state)
			      :key #'transition-label)))
	(if (null transition)
	    ;; If transition is NULL we have no transition from the
	    ;; state with the first label of the path.  We need to
	    ;; create a target state.
	    (let ((new-state (make-state)))
	      ;; Add a the transition from the current state to the
	      ;; new state.
	      (push (make-transition (car path) new-state)
		    (state-transitions state))
	      ;; And add the remaining path from the new state to the
	      ;; final state.
	      (add-path new-state (cdr path) final-state))
	    ;; We already have a transiton with that label.  Find the
	    ;; target state of that transition, and add the remaining
	    ;; path from there.
	    (add-path (transition-target transition)
		      (cdr path)
		      final-state)))))

;;; Compare two transitions for equality.  Two transitions are
;;; considered equal if they have the same label and if the target
;;; states are the same under EQ.
(defun transitions-equal (transition1 transition2)
  (and (eql (transition-label transition1)
	    (transition-label transition2))
       (eq (transition-target transition1)
	   (transition-target transition2))))

;;; Two states are equivalent if they have the same INFO as determined
;;; by EQUAL, and the same outgoing transitions, as determined by
;;; TRANSITION-EQUAL.
(defun states-equivalent-p (state1 state2)
  (unless (= (length (state-transitions state1))
	     (length (state-transitions state2)))
    (return-from states-equivalent-p nil))
  (and (equal (state-info state1) (state-info state2))
       (progn 
	 (setf (state-transitions state1)
	       (sort (state-transitions state1) #'< :key #'transition-label))
	 (setf (state-transitions state2)
	       (sort (state-transitions state2) #'< :key #'transition-label))
	 (every #'transitions-equal
		(state-transitions state1)
		(state-transitions state2)))))

;;; Our automata have a particular shape.  There are no loops and
;;; Every final state is reached by the same number of transitions
;;; from the initial state.  Thus, we can talk about LAYERS of states.
;;; A layer is defined by all states having the same number of
;;; transitions from the initial state.

;;; For a particular automaton, return a list of its layers.  Each
;;; layer is a list of states.
(defun compute-layers (start-state)
  (let ((result (list (list start-state))))
    (loop until (null (state-transitions (caar result)))
	  do (push (remove-duplicates
		    (loop for state in (car result)
			  append (mapcar #'transition-target
					 (state-transitions state))))
		   result))
    (reverse result)))

;;; Minimize a layer, which is a list of states.  The next layer has
;;; already been minimized, so that if there are two equivalent target
;;; states of any transition from a state in this layer, then the
;;; target states are also EQ.  This function returns an ALIST in
;;; which the CDR of each association does not occur as the CAR of any
;;; association, and is to be considered the state to be retained.
;;; Transitions to a state which is the CAR of some association in
;;; this alist should be modified to refer to the equivalent state in
;;; the CDR of that association instead.
(defun minimize-layer (layer)
  (let ((equivalences '()))
    (loop for rest on layer
	  for state1 = (car rest)
	  do (when (null (assoc state1 equivalences))
	       (loop for state2 in (cdr rest)
		     do (when (states-equivalent-p state1 state2)
			  (push (cons state2 state1) equivalences)))))
    equivalences))

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

(defun adjust-layer (layer alist)
  (loop for state in layer
	do (adjust-state state alist)))

(defun minimize-automaton (start-state)
  (let ((layers (compute-layers start-state)))
    (loop for rest = (reverse layers) then (cdr rest)
	  until (null (cdr rest))
	  do (adjust-layer (cadr rest) (minimize-layer (car rest)))))
  start-state)
