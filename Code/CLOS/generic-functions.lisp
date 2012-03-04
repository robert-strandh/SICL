(in-package #:sicl-clos)

(defparameter *generic-functions* (make-hash-table :test #'eq))

(defparameter *call-next-method*
  (lambda (&rest args)
    (declare (ignore args))
    (error "not next method")))

(defparameter *next-method-p*
  (lambda () nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS-OF.

(defun class-of (object)
  (if (standard-instance-p object)
      (standard-instance-class object)
      (built-in-class-of object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda list functions.
;;;
;;; FIXME: check syntax of lambda list (but not of specializers) in
;;; both these functions according to the MOP.

(defun extract-lambda-list (specialized-lambda-list)
  (loop for rest = specialized-lambda-list then (cdr rest)
	until (or (atom rest)
		  (member (car rest) lambda-list-keywords))
	collect (if (consp (car rest))
		    (caar rest)
		    (car rest))
	  into required
	finally (return (append required rest))))

(defun extract-specizlier-names (specialized-lambda-list)
  (loop for rest = specialized-lambda-list then (cdr rest)
	until (or (atom rest)
		  (member (car rest) lambda-list-keywords))
	collect (if (consp (car rest))
		    (cadar rest)
		    t)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-GENERIC-FUNCTION and DEFGENERIC.

(defun ensure-generic-function (name &rest keys)
  (or (gethash name *generic-functions*)
      (let ((generic-function (apply #'make-instance
				     'standard-generic-function
				     :name name
				     keys)))
	(setf (gethash name *generic-functions*)
	      generic-function)
	generic-function)))

;;; FIXME: add options and methods
(defmacro defgeneric (name lambda-list)
  `(ensure-generic-function
    ',name
    :lambda-list ',lambda-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A few definitions:
;;;
;;; A METHOD FUNCTION is a function that represents a single method
;;; metaobject of a generic function.  By default, when the generic
;;; function is an instance of STANDARD-GENERIC-FUNCTION and the
;;; method is an instance of STANDARD-METHOD, then the method function
;;; takes two arguments: a list of all the arguments to the generic
;;; function and a list of NEXT METHODS that the method may call using
;;; CALL-NEXT-METHOD.  These next methods are METHOD METAOBJECTS, so
;;; that calling then involves using the generic function
;;; METHOD-FUNCTION to get the method function of the method
;;; metaobject and use FUNCALL or APPLY to call it.  But for now we
;;; don't do it that way, and the don't do it that way in the book
;;; either.  Instead the next methods are just functions to call. 
;;;
;;; A METHOD LAMBDA is a lambda expression that must be converted into
;;; a METHOD FUNCTION.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAKE-METHOD-LAMBDA.

;;; This generic function takes four arguments: a generic function, a
;;; method, a lambda expression, and an environment.  We are told from
;;; the specification that "The generic function and method the method
;;; function will be used with are not required to be the given
;;; ones. Moreover, the method metaobject may be uninitialized."  This
;;; all means that MAKE-METHOD-LAMBDA must do its job without
;;; inspecting those arguments, and using only their classes to
;;; determine what to do.  Presumably, the method argument will just
;;; be a prototype instance of some class.
;;;
;;; The third argument, the lambda expression, is the lambda
;;; expression resulting from some minor transformations of the
;;; DEFMETHOD form.  In other words, MAKE-METHOD-LAMBDA must transform
;;; the unspecialized lambda list and body of an invocation of
;;; DEFMETHOD to a lambda expression.
;;;
;;; We are also told that the result of a call to MAKE-METHOD-LAMBDA
;;; must be converted to a function an passed as the :function
;;; initialization argument to MAKE-INSTANCE when an instance of the
;;; method metaobject is created.  This means that we can not use any
;;; information about the generic function (other than its class) to
;;; which the method will eventually belong.  
;;;
;;; This function returns two values, the first is a lambda expression
;;; and the second a list of initialization arguments and values.  As
;;; indicated above, the lambda expression must be converted to a
;;; function.  The initialization arguments and values are also passed
;;; to MAKE-INSTANCE when the method metaobject is created. 

(defun make-method-lambda-standard
    (generic-function method lambda-expression environment)
  (declare (ignore generic-function method environment))
  (let ((args (gensym))
	(next-methods (gensym)))
    (values `(lambda (,args ,next-methods)
	       (flet ((next-method-p ()
			(not (null ,next-methods)))
		      (call-next-method (&rest args)
			(when (null ,next-methods)
			  ;; FIXME: do this better.
			  (error "no next method"))
			(funcall (car ,next-methods)
				 (or args ,args)
				 (cdr ,next-methods))))
		 (declare (ignorable (function next-method-p)
				     (function call-next-method)))
		 (apply ,lambda-expression
			,args)))
	    '())))

(defun make-method-lambda
    (generic-function method lambda-expression environment)
  (make-method-lambda-standard
   generic-function method lambda-expression environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD and DEFMETHOD.

(defun parse-defmethod (args)
  (let ((name (pop args))
	(qualifiers (loop while (and (consp args) (not (listp (car args))))
			  collect (pop args)))
	(lambda-list (pop args)))
    ;; FIXME: handle declarations and documentation
    ;; FIXME: check the lambda list.  proper list, etc. 
    (let ((pos (position-if (lambda (x)
			      (member x '(&optional &rest &body &key
					  &allow-other-keys &aux)))
			    lambda-list)))
      (when (null pos)
	(setf pos (length lambda-list)))
      (values name
	      qualifiers
	      (append (mapcar (lambda (parameter)
				(if (consp parameter)
				    (car parameter)
				    parameter))
			      (subseq lambda-list 0 pos))
		      (subseq lambda-list pos))
	      (mapcar (lambda (parameter)
			(if (consp parameter)
			    (cadr parameter)
			    t))
		      (subseq lambda-list 0 pos))
	      args))))

(defun add-method (generic-function method)
  (setf (method-generic-function method) generic-function)
  (push method (generic-function-methods generic-function))
  ;; Trash the cache. 
  (let ((fun (compute-discriminating-function generic-function)))
    (setf (generic-function-discriminating-function generic-function) fun)
    (setf (fdefinition (generic-function-name generic-function)) fun))
  ;; FIXME: add method to specializer classes
  method)

(defun ensure-method (generic-function &rest keys)
  (let ((method (apply #'make-instance
		       'standard-method
		       keys)))
    (add-method generic-function method)
    method))

(defun canonicalize-specializers (specializers)
  ;; FIXME: handle eql specializers.
  (mapcar #'find-class specializers))

(defmacro defmethod (&rest arguments)
  (multiple-value-bind (name qualifiers lambda-list specializers body)
      (parse-defmethod arguments)
    (let ((generic-function-var (gensym)))
    ;; FIXME: do the lambda list
      `(let ((,generic-function-var (ensure-generic-function ',name)))
	 (ensure-method
	  ,generic-function-var
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ',(canonicalize-specializers specializers)
	  :body ',body
	  :function (make-method-lambda
		     ,generic-function-var
		     nil ; FIXME: pass a prototype method
		     '(lambda ,lambda-list ,@body)
		     nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-APPLICABLE-METHODS.

;;; Class C1 is a sub-specizlizer of class C2 with respect to some
;;; argument class C if and only if C1 occurs before C2 in the class
;;; precedence list of C.
;;;
;;; Recall that this function is used to determine whether one
;;; applicable method is more specific than another applicable method.
;;; Thus, we have already determined that C is more specific than both
;;; C1 and C2, and therefore both C1 and C2 are in the class
;;; precedence list of C,
;;;
;;; Should we ever be called with classes C1 and C2 that are not in
;;; the class precedence list of C, then the method we use (numeric
;;; comparison of the result of calling POSITION), will signal an
;;; error, which is reassuring.
(defun sub-specializer-p (class1 class2 class-of-argument)
  (let ((precedence-list (class-precedence-list class-of-argument)))
    (< (position class1 precedence-list) (position class2 precedence-list))))

;;; Determine whether a method is more specific than another method
;;; with respect to a list of classes of required arguments.  
;;;
;;; Recall that whether a method is more or less specific than another
;;; method is also a function of the classes of the arguments, because
;;; the order of two classes in the class precedence list of two
;;; different argument classes can be different.  
;;;
;;; This function is called only with applicable methods with respect
;;; to the classes of the arguments supplied.  
;;;
;;; It is possible for two methods of a generic function to be equally
;;; specific (which then means that they have the same specializer in
;;; every required position), but then they must have different
;;; qualifiers.  This function is called with all applicable
;;; functions, independent of the qualifiers, so this situation might
;;; happen here.
;;;
;;; FIXME: take into account the argument precedence order.
(defun method-more-specific-p (method1 method2 classes-of-arguments)
  (loop for s1 in (method-specializers method1)
	for s2 in (method-specializers method2)
	for class-of-argument in classes-of-arguments
	unless (eq s1 s2)
	  return (sub-specializer-p s1 s2 class-of-argument)))

;;; Determine whether a class C1 is a subclass of another class C2.
;;; This can be done by checking whether C2 is in the class precedence
;;; list of C1.
(defun subclassp (class1 class2)
  (member class2 (class-precedence-list class1)))

;;; Given a list of classes of the required arguments of a generic
;;; function, compute the applicable methods, independently of their
;;; qualifiers, but sorted in order from most to least specific. 
;;;
;;; The applicable methods are found by filtering out methods for
;;; which every specializer is a (non-strict) subclass of the
;;; corresponding argument class.  Then they are sorted according to
;;; the order determined by METHOD-MORE-SPECIFIC-P as defined above. 
(defun compute-applicable-methods-using-classes
    (generic-function classes-of-arguments)
  (sort (copy-list
	 (remove-if-not (lambda (method)
			  (every #'subclassp
				 classes-of-arguments
				 (method-specializers method)))
			(generic-function-methods generic-function)))
	(lambda (method1 method2)
	  (method-more-specific-p method1 method2 classes-of-arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD-FUNCTION.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))

(defun compute-effective-method-function (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
	(before-methods (remove-if-not #'before-method-p methods))
	(after-methods (remove-if-not  #'after-method-p methods))
	(around-methods (remove-if-not  #'around-method-p methods)))
    (when (null primary-methods)
      (error "no primary method"))
    (let ((primary-chain
	    `(funcall ,(method-function (car primary-methods))
		      args
		      '(,@(loop for method in (cdr primary-methods)
				collect (method-function method)))))
	  (before-chain
	    (loop for method in before-methods
		  collect `(funcall ,(method-function method)
				    args
				    '())))
	  (after-chain
	    (loop for method in (reverse after-methods)
		  collect `(funcall ,(method-function method)
				    args
				    '()))))
      (compile
       nil
       (if (null around-methods)
	   `(lambda (&rest args)
	      ,@before-chain
	      ,primary-chain
	      ,@after-chain)
	   `(lambda (&rest args)
	      (funcall ,(method-function (car around-methods))
		       args
		       (list ,@(loop for method in (cdr around-methods)
				     collect (method-function method))
			     (lambda (args next-methods)
			       (declare (ignore next-methods))
			       ,@before-chain
			       ,primary-chain
			       ,@after-chain)))))))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-DISCRIMINATING-FUNCTION.

(defun compute-real-lambda (generic-function)
  (let ((number-of-required
	  (length
	   (extract-specizlier-names
	    (generic-function-lambda-list generic-function)))))
    `(lambda (&rest args)
       (locally (declare (special *cache* *generic-function*))
	 (let* ((cache *cache*)
		(required (subseq args 0 ,number-of-required))
		(classes (mapcar #'class-of required))
		(effective-method (gethash classes cache)))
	   (when (null effective-method)
	     (let* ((applicable-methods
		      (compute-applicable-methods-using-classes
		       *generic-function* classes)))
	       (setf effective-method
		     (compute-effective-method-function
		      applicable-methods))
	       (setf (gethash classes cache)
		     effective-method)))
	   (apply effective-method args))))))

(defun compute-discriminating-function (generic-function)
  (let ((real-function
	  (compile nil (compute-real-lambda generic-function)))
	(cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (let ((*generic-function* generic-function)
	    (*cache* cache))
	(declare (special *cache* *generic-function*))
	(apply real-function args)))))

(defun sd-initialize-instance-after-standard-generic-function
    (instance &key &allow-other-keys)
  (let ((fun (compute-discriminating-function instance)))
    (setf (generic-function-discriminating-function instance) fun)
    (setf (fdefinition (generic-function-name instance)) fun)))

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
;;; Call history.
;;;
;;; We maintain a CALL HISTORY of the generic function.  This call
;;; history is a list of call records.  Whenever a call is made to the
;;; generic function with some call profile that has not yet been used
;;; in a call, we compute the effective method to use, and we add a
;;; call record to the call history.

(defun generic-function-call-history (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-call-history" object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%call-history))
	  (t (error "no method for ~s on generic-function-call-history"
		    object)))))

(defun (setf generic-function-call-history) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf generic-function-call-history)" object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (setf (slot-value object '%call-history) new-value))
	  (t (error "no method for ~s on (setf generic-function-call-history)"
		    object)))))

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
;;; history of the generic function.

(defun make-state (&optional name info)
  (list name info))

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

(defun make-transition (number target)
  (cons number target))

(defun transition-number (transition)
  (car transition))

(defun transition-target (transition)
  (cdr transition))

(defun (setf transition-target) (target transition)
  (setf (cdr transition) target))

(defun transitions-equal (transition1 transition2)
  (and (eql (transition-number transition1)
	    (transition-number transition2))
       (eq (transition-target transition1)
	   (transition-target transition2))))

(defun add-path (state path final-state)
  (if (null (cdr path))
      ;; by construction, we have no transition yet
      (push (make-transition (car path) final-state)
	    (state-transitions state))
      (let ((transition (find (car path)
			      (state-transitions state)
			      :key #'transition-number)))
	(if (null transition)
	    (let ((new-state (make-state)))
	      (push (make-transition (car path) new-state)
		    (state-transitions state))
	      (add-path new-state (cdr path) final-state))
	    (add-path (transition-target transition)
		      (cdr path)
		      final-state)))))

(defun states-equivalent-p (state1 state2)
  (unless (= (length (state-transitions state1))
	     (length (state-transitions state2)))
    (return-from states-equivalent-p nil))
  (setf (state-transitions state1)
	(sort (state-transitions state1) #'< :key #'transition-number))
  (setf (state-transitions state2)
	(sort (state-transitions state2) #'< :key #'transition-number))
  (every #'transitions-equal
	 (state-transitions state1)
	 (state-transitions state2)))	

(defun compute-layers (start-state)
  (let ((result (list start-state)))
    (loop until (null (state-transitions (car result)))
	  do (push (remove-duplicates
		    (loop for state in (car result)
			  append (mapcar #'transition-target
					 (state-transitions state))))
		   result))
    (reverse result)))

(defun minimize-layer (layer)
  (loop for rest on layer
	for state1 = (car rest)
	do (when (null (state-info state1))
	     (loop for state2 in (cdr rest)
		   do (when (and (null (state-info state2))
				 (states-equivalent-p state1 state2))
			(setf (state-info state2) state1))))))

(defun adjust-transition (transition)
  (unless (null (state-info (transition-target transition)))
    (setf (transition-target transition)
	  (state-info (transition-target transition)))))

(defun adjust-state (state)
  (mapc #'adjust-transition (state-transitions state)))

(defun adjust-layer (layer)
  (mapc #'adjust-state layer))

(defun minimize-automaton (start-state)
  (let ((layers (compute-layers start-state)))
    (loop for rest = (cdr (reverse layers)) then (cdr rest)
	  until (null (cdr rest))
	  do (minimize-layer (car rest))
	     (adjust-layer (cadr rest))))
  start-state)

(defun make-intervals (transitions)
  (loop with trs = (sort (copy-list transitions) #'<
			 :key #'transition-number)
	with first = (car trs)
	with rest = (cdr trs)
	with result = (list (cons (cons (transition-number first)
					(1+ (transition-number first)))
				  (transition-target first)))
	for tr in rest
	do (if (and (eq (transition-target tr)
			(cdr (car result)))
		    (= (transition-number tr)
		       (cdr (car (car result)))))
	       (incf (cdr (car (car result))))
	       (push (cons (cons (transition-number tr)
				 (1+ (transition-number tr)))
			   (transition-target tr))
		     result))
	finally (return (reverse result))))

(defun compute-test-tree (var intervals open-inf-p open-sup-p)
  (let ((length (length intervals)))
    (if (= length 1)
	(let* ((interval (car intervals))
	       (start (car (car interval)))
	       (end (cdr (car interval)))
	       (target (cdr interval)))
	  (if open-inf-p
	      (if open-sup-p
		  `(if (< ,var ,start)
		       (go err)
		       (if (< ,var ,end)
			   (go ,target)
			   (go err)))
		  `(if (< ,var ,start)
		       (go err)
		       (go ,target)))
	      (if open-sup-p
		  `(if (< ,var ,end)
		       (go ,target)
		       (go err))
		  `(go ,target))))
	(let* ((half (floor length 2))
	       (left (subseq intervals 0 half))
	       (right (subseq intervals half))
	       (open-p (/= (cdr (car (car (last left))))
			   (car (car (car right))))))
	  `(if (< ,var ,(car (car (car right))))
	       ,(compute-test-tree var left open-inf-p open-p)
	       ,(compute-test-tree var right nil open-sup-p))))))

(defun test-automaton ()
  (let ((fs (loop for info in '(w x y z) collect (make-state nil info)))
	(ps (remove-duplicates (loop repeat 200
				     collect (loop repeat 4
						   collect (random 8)))
			       :test #'equal))
	(ss (make-state)))
    (loop for p in ps
	  do (add-path ss p (elt fs (random (length fs)))))
    (minimize-automaton ss)))

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
