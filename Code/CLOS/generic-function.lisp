(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effective method automaton.

;;; To determine the effective method to run from the classes of the
;;; required arguments, we use an automaton, but a particularly simple
;;; one.  From the start state to a final state of the automaton there
;;; are as many state transitions as there are specialized parameters
;;; of the generic function.  Each transition is based on the unique
;;; number of the class of the corresponding specialized argument.  
;;;
;;; The automaton is created every time a new combination of classes
;;; of required arguments is seen.  It is computed from the call
;;; history of the generic function.  At runtime, the automaton is
;;; represented as nested IFs that test the unique numbers of the
;;; classes of the call to what has already been seen.  On modern
;;; computers, such tests can be very fast, because they only require
;;; testing against small constant integers, so no memory accesses are
;;; needed, other than for finding the unique number of each class at
;;; the beginning of each call.  

;;; At compile time, we represent the automaton as nested lists.  We
;;; deliberately avoid using classes so as to avoid circular
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

;;; A TRANSFER is similar to a transition.  It has a label and a
;;; target, but the target is a symbol.
(defun make-transfer (label target)
  (cons label target))

(defun transfer-label (transfer)
  (car transfer))

(defun transfer-target (transfer)
  (cdr transfer))

;; An INTERVAL is a sequence of consecutive integers.  The START of an
;; interval is equal to the first integer of the sequence, and the END
;; of the interval is 1+ of the last integer of the sequence. 

(defun make-interval (start end)
  (cons start end))

(defun interval-start (interval)
  (car interval))

(defun interval-end (interval)
  (cdr interval))

(defun (setf interval-end) (new-value interval)
  (setf (cdr interval) new-value))

;; A TRANSFER-GROUP is similar to a transfer.  It has an interval of
;; labels and a target, which is a symbol.

(defun make-transfer-group (interval target)
  (cons interval target))

(defun transfer-group-interval (transfer-group)
  (car transfer-group))

(defun transfer-group-target (transfer-group)
  (cdr transfer-group))

;;; From a list of transfers, compute a list of transfer groups by
;;; grouping together transfers with adjacent labels that have the
;;; same target.
(defun make-transfer-groups (transfers)
  (loop with trs = (sort (copy-list transfers) #'< :key #'transfer-label)
	with first = (car trs)
	with rest = (cdr trs)
	with result = (list (make-transfer-group
			     (make-interval (transfer-label first)
					    (1+ (transfer-label first)))
			     (transfer-target first)))
	for tr in rest
	do (if (and (eq (transfer-target tr)
			(transfer-group-target (car result)))
		    (= (transfer-label tr)
		       (interval-end (transfer-group-interval (car result)))))
	       (incf (interval-end (transfer-group-interval (car result))))
	       (push (make-transfer-group
		      (make-interval (transfer-label tr)
				     (1+ (transfer-label tr)))
		      (transfer-target tr))
		     result))
	finally (return (reverse result))))

;;; Given a list of transfer groups, compute a nested IF form.  The
;;; argument VAR contains a symbol that is tested against the transfer
;;; labels in the intervals of the transfer groups.  The leaves of the
;;; IF form are all of the form (GO <tag>) where <tag> is the target
;;; symbol of the transfer group, or the symbol which is the value of
;;; the parameter DEFAULT if there is no target for some value of VAR.
;;; The parameters OPEN-INF-P and OPEN-SUP-P indicate whether there is
;;; some action to take for label values that less than the start of
;;; the first interval and greater than or equal to the end of the
;;; last interval respectively.
(defun compute-test-tree (var default transfer-groups open-inf-p open-sup-p)
  (let ((length (length transfer-groups)))
    (if (= length 1)
	(let* ((transfer-group (car transfer-groups))
	       (interval (transfer-group-interval transfer-group))
	       (start (interval-start interval))
	       (end (interval-end interval))
	       (target (transfer-group-target transfer-group)))
	  (if open-inf-p
	      (if open-sup-p
		  (if (= end (1+ start))
		      `(if (= ,var ,start)
			   (go ,target)
			   (go ,default))
		      `(if (< ,var ,start)
			   (go ,default)
			   (if (< ,var ,end)
			       (go ,target)
			       (go ,default))))
		  `(if (< ,var ,start)
		       (go ,default)
		       (go ,target)))
	      (if open-sup-p
		  `(if (< ,var ,end)
		       (go ,target)
		       (go ,default))
		  `(go ,target))))
	(let* ((half (floor length 2))
	       (left (subseq transfer-groups 0 half))
	       (right (subseq transfer-groups half))
	       ;; FIXME: these cars and cdrs should be abstracted.
	       (open-p (/= (interval-end
			    (transfer-group-interval (car (last left))))
			   (interval-start
			    (transfer-group-interval (car right))))))
	  ;; FIXME: these cars and cdrs should be abstracted.
	  `(if (< ,var ,(interval-start (transfer-group-interval (car right))))
	       ,(compute-test-tree var default left open-inf-p open-p)
	       ,(compute-test-tree var default right nil open-sup-p))))))

(defun test-tree-from-state (var default state)
  (let* ((transfers 
	   (mapcar
	    (lambda (transition)
	      (make-transfer (transition-label transition)
			     (state-name (transition-target transition))))
	    (state-transitions state)))
	 (transfer-groups (make-transfer-groups transfers)))
    ;; T and T might not be optimal for the last two arguments. 
    (compute-test-tree var default transfer-groups t t))) 

;;; A CALL SIGNATURE is a list of unique class numbers of specialized
;;; arguments used in a call to a generic function. 

;;; A DISPATCH ENTRY is a call signature and a form to be evaluated
;;; when a call with that signature is made. 

(defun make-dispatch-entry (call-signature form)
  (cons call-signature form))

(defun dispatch-entry-call-signature (dispatch-entry)
  (car dispatch-entry))

(defun dispatch-entry-form (dispatch-entry)
  (cdr dispatch-entry))

;;; Create a TAGBODY form that implements the dispatch function for
;;; the given dispatch entries.  The VARS parameter contains a list of
;;; symbols to be used as variables to test in the dispatch.  The
;;; length of VARS is the same as the call signatures in each dispatch
;;; of the DISPATCH-ENTRIES.  The TAGBODY form is structured so that
;;; the FORM of a dispatch entry D in the DISPATCH-ENTRIES is
;;; evaluated if and only if the variables indicated by VARS contain
;;; values with classes corresponding to the call signature of D.  If
;;; the values of the variables indicated by VARS do not correspond to
;;; any of the call signatures of DISPATCH-ENTRIES, then the execution
;;; of the TAGBODY terminates normally, with none of the forms having
;;; been evaluated.  Each form must do a non-local transfer of
;;; control.  This is typically implemented by wrapping the TAGBODY
;;; created by this function in a BLOCK and have the forms be of the
;;; form (RETURN-FROM <block-name> ...).  Default behavior is
;;; implemented by following the ... FIXME: say more!
(defun make-dispatch (vars dispatch-entries)
  (let ((start-state (make-state)))
    ;; Create final states corresponding to each dispatch entry, where
    ;; the info slot is the form to be evaluated in that final state.
    ;; Add the call signatures as paths in the automaton. 
    (loop for dispatch-entry in dispatch-entries
	  do (add-path start-state
		       (dispatch-entry-call-signature dispatch-entry)
		       (make-state (dispatch-entry-form dispatch-entry))))
    ;; Minimize the automaton obtained.
    (minimize-automaton start-state)
    ;; Create the TAGBODY form.
    (let ((layers (compute-layers start-state))
	  (default (gensym)))
      `(tagbody
	  ,@(loop for layer in layers
		  for var in vars
		  append (loop for state in layer
			       collect (state-name state)
			       collect (test-tree-from-state var default state)))
	  ,@(loop for state in (car (last layers))
		  append (list (state-name state)
			       (state-info state)))
	  ,default))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a list of methods, all from the same generic function,
;;; extract a SPECILIZER PROFILE, which is a list with the same number
;;; of elements as the number of required parameters to the generic
;;; function, and which contains NIL in every position where every
;;; method is specialized only for the class T, and T in a position
;;; where some method has a specializer form something other than the
;;; class T.

(defun compute-specializer-profile (methods)
  (reduce (lambda (specializers1 specializers2)
	    (mapcar (lambda (specializer1 specializer2)
		      (or specializer1 specializer2))
		    specializers1
		    specializers2))
	  (mapcar (lambda (method)
		    (mapcar (lambda (specializer)
			      (not (eq specializer *t*)))
			    (method-specializers method)))
		  methods)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DISCRIMINATING-FUNCTION.

;;; The DISCRIMINATING FUNCTION of a generic function is the function
;;; that will be invoked when the generic function is invoked.  The
;;; discriminating function closes over a local variable that contains
;;; the generic function.
;;;
;;; The CACHE of a generic function is a function that is called with
;;; two arguments, a list of the arguments to the generic function,
;;; and the generic function itself.  The cache is contained as a
;;; local variable closed over by the discriminating function.  
;;;
;;; The discriminating function also closes over a function that sets
;;; the cache.  By doing it this way, we don't have to set the
;;; discriminating function to a new value each time we modify the
;;; cache.  This is especially valuable in a system where we need to
;;; set (FDEFINITION NAME) to the discriminating function.

;;; For all arguments that are specialized on according to the in the
;;; specializer profile, return a list of their classes.
(defun compute-call-classes (args specialier-profile)
  (loop for arg in args
	for item in specialier-profile
	when item collect (class-of arg)))

;;; Same as above, but return a list of unique class numbers instead. 
(defun compute-call-signature (args specialier-profile)
  (mapcar #'unique-number
	  (compute-call-classes args specialier-profile)))

(defun compute-cache-lambda (specializer-profile call-history)
  (let* ((block-name (gensym))
	 (dispatch-entries
	   (loop for history-entry in call-history
		 collect (make-dispatch-entry
			  (car history-entry)
			  `(return-from ,block-name
			     (apply ,(cdr history-entry) args)))))
	 (bindings (loop for s in specializer-profile
			 for i from 0
			 when s
			   collect `(,(gensym)
				     (unique-number
				      (class-of (nth ,i args))))))
	 (vars (mapcar #'car bindings)))
    `(lambda (args generic-function)
       (block ,block-name
	 (let ,bindings
	   (declare (ignorable ,@vars))
	   ,(if (null dispatch-entries)
		nil
		(make-dispatch vars dispatch-entries))
	   (handle-cache-miss args generic-function))))))
	 
(defun compute-cache-function (specializer-profile call-history)
  (compile nil
	   (compute-cache-lambda specializer-profile call-history)))

(defun compute-and-set-cache-from-call-history (generic-function)
  (funcall (set-cache generic-function)
	   (compute-cache-function (specializer-profile generic-function)
				   (call-history generic-function))))
  

;;; For a list of classes (with possible duplicates), compute a list
;;; containing those classes, and all their superclasses, but without
;;; any duplication.  We use the class precedence list because we got
;;; the classes from up-to-date instances, so inheritence is
;;; finalized.
(defun all-unique-superclasses (classes)
  (let ((result '()))
    (loop for class in classes
	  do (loop for super in (class-precedence-list class)
		   do (pushnew super result :test #'eq)))
    result))

;; (defclass call-history-entry-remover ()
;;   (;; The call history entry to be removed.
;;    (%call-history-entry
;;     :initarg :call-history-entry :reader call-history-entry)
;;    ;; The generic function containing this call history entry. 
;;    (%function-of-entry
;;     :initarg :function-of-entry
;;     :reader function-of-entry)
;;    ;; All the classes that have this dependent in its
;;    ;; set of dependents
;;    (%classes-containing-dependent
;;     :initarg :classes-containing-dependent
;;     :reader classes-containing-dependent)))

;; (defmethod update-dependent (class (dependent call-history-entry-remover))
;;   ;; This dependent is a dependent of several classes, but now that we
;;   ;; remove the call-history entry, we do not need this dependent
;;   ;; anymore in any classes, so we remove it.
;;   (let ((gf (function-of-entry dependent)))
;;     (loop for class in (classes-containing-dependent dependent)
;; 	  do (remove-dependent class dependent))
;;     ;; Remove the call history entry from the call history of the
;;     ;; generic function.
;;     (setf (call-history gf)
;; 	  (remove (call-history-entry dependent)
;; 		  (call-history gf)
;; 		  :test #'eq))
;;     ;; Create and set a new cache using the updated call history.
;;     (compute-and-set-cache-from-call-history gf)))

;; (defun make-call-history-entry-remover (generic-function entry arg-classes)
;;   (let* ((all-classes (all-unique-superclasses arg-classes))
;; 	 (remover (make-instance
;; 		   'call-history-entry-remover
;; 		   :call-history-entry entry
;; 		   :function generic-function
;; 		   :classes-containing-dependent all-classes)))
;;     (loop for class in all-classes
;; 	  do (add-dependent class remover))))

(defun handle-cache-miss (args generic-function)
  (let* ((specializer-profile (specializer-profile generic-function))
	 (number-of-required-args (length specializer-profile))
	 (required-args (subseq args 0 number-of-required-args))
	 (classes (mapcar #'class-of required-args)))
    (multiple-value-bind (applicable-methods valid-p)
	(compute-applicable-methods-using-classes generic-function classes)
      (if valid-p
	  (let* ((call-signature
		   (compute-call-signature args specializer-profile))
		 (effective-method-function
		   (compute-effective-method-function applicable-methods))
		 (call-history-entry
		   (cons call-signature effective-method-function))
		 (new-call-history
		   (cons call-history-entry
			 (call-history generic-function))))
	    (setf (call-history generic-function)
		  new-call-history)
	    (compute-and-set-cache-from-call-history generic-function)
	    (funcall effective-method-function args))
	  (error "can't handle this yet")))))

(defun compute-and-set-discriminating-function (generic-function)
  (let ((fun (compute-discriminating-function generic-function)))
    (setf (discriminating-function generic-function) fun)
    (setf (fdefinition (generic-function-name generic-function)) fun)))

(defmethod initialize-instance :after
    ((instance standard-generic-function) &key &allow-other-keys)
  (compute-and-set-discriminating-function instance))

;;; An EFFECTIVE METHOD COMPONENTS object is an object that represents
;;; an effective method in a way that it can be compared with EQUAL.
;;; It is a list, the elements of which are defined with respect to a
;;; particular method combination.  Each element represents a
;;; permissible qualifier for the method combiation.  For the standard
;;; method combination, there are thus four elements, the first one
;;; representing the primary methods, the second one the :before
;;; methods, the third one the :after methods and the fourth one the
;;; :around methods.  Each such element is a list of methods, ordered
;;; from most specific to least specific.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Call record.
;;;
;;; A CALL PROFILE (or a PROFILE for short) of a particular call to a
;;; generic function is a list of unique numbers of classes of
;;; specialized required arguments used in that call.  The length of a
;;; call profile is that of the number of required arguments that are
;;; specialized.  The list is ordered from left to right, i.e., the
;;; first element of the list corresponds to the leftmost specialized
;;; required argument, etc.
;;;
;;; A CALL RECORD is a CONS cell where the CAR is a call profile, and
;;; the CDR is an effective method.

(defun make-call-record (profile effective-method)
  (cons profile effective-method))

(defun call-record-profile (call-record)
  (car call-record))

(defun call-record-effective-method (call-record)
  (cdr call-record))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda lists.
;;;
;;; There are several different lambda lists involved in generic functions.
;;;
;;; From the programmer's perspective, there are only two kinds of
;;; lambda lists: the lambda list of the generic function, either
;;; explicitly mentioned when the generic function was created, or
;;; automatically generated from a method definition, and the lambda
;;; lists of individual methods as explicitly mentioned in the
;;; method-defining forms.
;;;
;;; From our perspective, the situation is more complicated.  First of
;;; all, there are a few more lambda lists involved.  There is the
;;; lambda list of the effective method function which is derived from
;;; the lambda lists of the applicable methods that make up the
;;; effective method.  Then there is the lambda list of the function
;;; that gets invoked when a method calls call-next-method.
;;;
;;; Furthermore, we can alter or replace the explicitly supplied
;;; lambda lists as long as this is transparent to the programmer.
;;; And we might want to do that for performance reasons.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making slot-value and (setf slot-value) faster.
;;;
;;; Probably not an original idea, but still: Pretend that every slot
;;; has a reader and a writer generic function.  These functions are
;;; stored in two hash tables, one for readers and one for writers
;;; using the slot name as a key.
;;;
;;; Use a compiler macro so that whenever either SLOT-VALUE or (SETF
;;; SLOT-VALUE) is used with a constant slot name, the corresponding
;;; form gets replaced by a call to the particular generic function
;;; for that slot name.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Avoiding consing during generic-function invocation.
;;;
;;; The standard protocol requires that &rest arguments be used and
;;; lists of arguments to be passed around from the generic function
;;; to its effective methods, and between different applicable
;;; methods.  But when neither &rest nor &key is used, this seems
;;; wasteful, because in that case, memory will be allocated and then
;;; discarded immediately after the end of the call. 
;;;
;;; Here is an idea to try to avoid that wasted memory: Use a (small)
;;; pool of CONS cells to use for such calls.  Allocate them upon
;;; entry to the generic function and discard them on exit.  When the
;;; pool is empty, allocate as usual using CONS.
;;;
;;; Notice that we can't use a fixed list per generic function because
;;; of the presence of threads.  Two threads can simultaneously call
;;; one generic function with different augments, so each thread needs
;;; a different argument list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Metastability issues.
;;;
;;; In the AMOP book, a few metastability issues are mentioned.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE.
;;;
;;; Calling SLOT-VALUE involves finding where the slot is located
;;; which requires accessing the EFFECTIVE-SLOTS slot of
;;; STANDARD-CLASS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DISCRIMINATING-FUNCTION.
;;;
;;; If the generic function COMPUTE-DISCRIMINATING-FUNCTION were to be
;;; modified, for instance a method might be added, then its own
;;; discriminating function might need to be recomputed.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method reader functions.
;;;
;;; The method reader functions are: METHOD-FUNCTION,
;;; METHOD-GENERIC-FUNCTION, METHOD-LAMBDA-LIST, METHOD-SPECIALIZERS,
;;; METHOD-QUALIFIERS, and ACCESSOR-METHOD-SLOT-DEFINITION.  The last
;;; one only applies to subclasses of STANDARD-ACCESSOR-METHOD.  The
;;; other ones apply to subclasses of the class METHOD.  Initially,
;;; ACCESSOR-METHOD-SLOT-DEFINITION has a method specialized to
;;; STANDARD-ACCESSOR-METHOD, and the others each have a method
;;; specialized to STANDARD-METHOD.  Each one of those methods is an
;;; instance of STANDARD-READER-METHOD.
;;;
;;; We assume that each reader function stores a CALL HISTORY, which
;;; consists of a list of entries, where each entry associates the
;;; class of the argument with an effective method to be called for
;;; that class.  The call history is used to build a CACHE, but that
;;; is unimportant to this discussion, because the cache can be built
;;; by using only the call history, and the representation of the
;;; cache can vary from one generic function to another, and even for
;;; the same generic function according to the complexity of the call
;;; history.
;;;
;;; The following can happen:
;;;
;;;  1. The function is called with an argument whose class is not in
;;;     the call history, so the call history needs to be added to,
;;;     but what is already in the call history is valid.
;;;
;;;  2. A method is removed. 
;;; 
;;;  3. A method is added.
;;;
;;; For situation number 1, we must assume that the call history of
;;; each method reader function contains an entry for each method
;;; subclass that has instances on any of these method reader
;;; functions.  In practice, this means that the call history must
;;; contain entries for the classes STANDARD-METHOD and
;;; STANDARD-READER-METHOD.  When the function is called with an
;;; argument of a class that is not in the call history, a call is
;;; made to COMPUTE-EFFECTIVE-METHOD which will call the method reader
;;; functions on each method of this reader function.  By the
;;; assumption above, these calls will work.
;;;
;;; Situation 2 is more complicated.  We cannot simply trash the call
;;; history and hope that it will automatically be rebuilt, because
;;; once the call history is trashed, the function will not work for
;;; any arguments, and, in order to rebuild the call history, we need
;;; for the function to work, at least for some arguments.  So instead
;;; of trashing the call history, we compute a new one, while letting
;;; the old one remain in place.  This allows us to make recursive
;;; calls to the function, and those recursive calls will use the old
;;; call history.  Only when the new call history has been completely
;;; recomputed do we replace the old one.  Notice that if the initial
;;; method (see above) is removed, then nothing will work after that,
;;; so we must prevent this from happening.
;;;
;;; Situation 3 is similar to situation 2, as long as the method added
;;; is one that can already be handled by the reader function.  In
;;; other words, if the method added is an instance of STANDARD-METHOD
;;; or STANDARD-READER-METHOD, then the existing call history can
;;; handle the recursive calls required to rebuild a new one.
;;;
;;; It is not possible to add a new method to any of the method reader
;;; functions if that new method is not an instance of STANDARD-METHOD
;;; or STANDARD-READER-METHOD.  It can not even be a subclass of any
;;; of those classes.  Here is why: The method reader functions would
;;; have to be called recursively on the new method in order to add to
;;; the call history, but that would require that the method reader
;;; function already work for the new type of method.
;;;
;;; Notice that the restriction in the previous paragraph does not
;;; prevent users from creating subclasses of STANDARD-METHOD,
;;; STANDARD-READER-METHOD, or even of METHOD, as long as methods on
;;; those subclasses for the method reader functions are themselves
;;; instances of STANDARD-METHOD or STANDARD-READER-METHOD. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Meters.
;;;
;;; Generic function invocation is an excellent opportunity for
;;; Multics-style meters.
;;;
;;; For instance, we could record:
;;;
;;;  * Total number of calls.
;;;  * Number of calls resulting in a cache miss.
;;;  * Total time computing a new cache.
;;;  * Number of times the call record was destroyed.
;;;
;;; With this information, we can compute some very interesting
;;; statistics, such as the average overhead per call as a result of
;;; computing the cache. Etc.
