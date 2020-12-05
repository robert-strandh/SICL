(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class cache.
;;;
;;; A CLASS CACHE of a particular call to a generic function is a list
;;; of classes of the specialized required arguments passed to the
;;; generic function in that call.  The call class cache the same
;;; order as the required parameters of the generic function,
;;; independently of the argument precedence order of the function.
;;; The class cache (together with the class T for unspecialized
;;; arguments) is what is passed to
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES in order to determine
;;; whether a list of applicable methods can be computed, using only
;;; the classes of the required arguments.
;;;
;;; For a particular call to a generic function, if the classes of the
;;; specialized required arguments correspond to the classes in a
;;; class cache, then we have already at some point determined a list
;;; of applicable methods for that call, so we do not have to compute
;;; it again.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specializer profile.
;;;
;;; The SPECIALIZER PROFILE of a generic function is a proper list,
;;; the length of which is the number of required parameters of the
;;; generic function.  The specializer profile represents a condensed
;;; version of the information concerning the specializers of the
;;; methods of the generic function.  Each element of the specializer
;;; profile is either T or NIL.  The element is T when there exists a
;;; method on the generic function with a specializer other than the
;;; class T in the corresponding parameter position.  The element is
;;; NIL when every method on the generic function has the class T as a
;;; specializer in the corresponding parameter position.  Arguments to
;;; the generic function corresponding to a specializer profile
;;; element of NIL make no difference in determining the applicable
;;; methods for a particular call.
;;;
;;; The specializer profile must be updated when methods are added or
;;; removed from the generic function.  When a method is added, each
;;; specializer of that method which is not the class named T causes
;;; the corresponding element of the specializer profile to be set to
;;; T.  In this case, the call cache is discarded.  When a method is
;;; removed, the specializer profile is initially set to a list of a
;;; NIL elements.  Then the list of methods of the generic function is
;;; traversed and the specializer profile is updated as if each method
;;; were just added to the generic function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Applicable method cache.
;;;
;;; An APPLICABLE METHOD CACHE of a particular call to a generic
;;; function is list of applicable methods, as returned by the generic
;;; function COMPUTE-APPLICABLE-METHODS-USING-CLASSES when called with
;;; the classes in the call profile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effective method cache.
;;;
;;; An EFFECTIVE METHOD CACHE for a particular applicable method cache
;;; is the result of calling the generic function
;;; COMPUTE-EFFECTIVE-METHOD, passing it the list of methods of that
;;; applicable method cache and then compiling the result in the null
;;; lexical environment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Call cache.
;;;
;;; A CALL CACHE represents information about a particular call to a
;;; generic function.  It is represented as a proper list with at
;;; least 2 CONS cells in it, and it conceptually contains 3 items:
;;;
;;;   1. A class cache containing a list of class metaobjects, one for
;;;      each specialized required parameter, corresponding to the
;;;      classes of the arguments of the call.  This item is located
;;;      in the CAR of the list representing the call cache.
;;;
;;;   2. An effective method cache, containing the effective method
;;;      function to invoke for calls with corresponding to the
;;;      classes in the class cache.  This item is located in the
;;;      CADDR of the list representing the call history entry.
;;;
;;;   3. An applicable method cache, containing a list of the
;;;      applicable methods that make up the effective method for this
;;;      call.  This item is located in the CDDDR of the list
;;;      representing the call cache.

(defun make-call-cache
    (relevant-classes applicable-method-cache effective-method-cache)
  (list* relevant-classes
         effective-method-cache
         applicable-method-cache))

(defun class-cache (call-cache)
  (car call-cache))

(defun effective-method-cache (call-cache)
  (cadr call-cache))

(defun applicable-method-cache (call-cache)
  (cddr call-cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Call history.
;;;
;;; We maintain a CALL HISTORY for each generic function.  The call
;;; history is a proper list, each element of which is a CALL CACHE.
;;;
;;; Different call caches in one call history may share the same
;;; applicable method cache and the same effective method cache.  When
;;; a new call cache C is about to be added to the call history, the
;;; existing call history is traversed to see whether there is an
;;; existing call cache D with the same (EQUAL) applicable method
;;; cache.  In this case, C is modified before it is added so that its
;;; applicable method cache and effective method cache are set to
;;; those of D.

;;; The discriminating function does the following:
;;;
;;;   1. Compute the list of instance class numbers of the required
;;;      arguments that it was passed and for which the specializer
;;;      profile contains T.
;;;
;;;   2. Compare each one of those to pre-computed constants using a
;;;      TAGBODY form.  If there is a hit, then the corresponding
;;;      effective method is invoked and the discriminating function
;;;      returns.
;;;
;;;   3. If there is not a hit, then control is transferred to the end
;;;      of the TAGBODY form.  There, DEFAULT-DISCRIMINATING-FUNCTION
;;;      is invoked.
;;;
;;; The default discriminating function does the following:
;;;
;;;   1. Check that the instance class number of each specialized
;;;      required argument is the same as the unique number of its
;;;      class.  If it is not the case, call the generic function
;;;      UPDATE-INSTANCE-FOR-REDEFINED-CLASS on those arguments and
;;;      invoke the discriminating function again.
;;;
;;;   2. If the instances are all up-to-date, then compute a call
;;;      profile for the call by calling CLASS-OF for each required
;;;      argument and then call the generic function
;;;      COMPUTE-APPLICABLE-METHODS-USING-CLASSES with the resulting
;;;      call profile.
;;;
;;;   3. If the call in step 2 returns TRUE as a second return value,
;;;      then the first value returned represents an applicable method
;;;      cache to be stored.  If so, call the generic function
;;;      COMPUTE-EFFECTIVE-METHOD with applicable method cache, thus
;;;      computing an effective method cache.  Create a call cache
;;;      from the list computed in step 1, the applicable method
;;;      cache, and the effective method cache.  Add the computed call
;;;      cache to the call history.  Call the generic function
;;;      COMPUTE-DISCRIMINATING-FUNCTION in order to compute a new
;;;      discriminating function that takes into account the new
;;;      argument classes.  Finally, call the effective just computed
;;;      method and return the result.
;;;
;;;   4. If the call in step 2 returns FALSE as a second return value,
;;;      then instead call the generic function
;;;      COMPUTE-APPLICABLE-METHODS, passing it all the current
;;;      arguments.
;;;
;;;   5. If the call in step 4 returns a non-empty list of methods,
;;;      then call COMPUTE-EFFECTIVE-METHOD with that list.  Call the
;;;      resulting effective method and return the result.
;;;
;;;   6. If the call in step 4 returns an empty list, then call
;;;      NO-APPLICABLE-METHOD.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; How we make slot accessors fast.
;;;
;;; A slot accessor (reader or writer) generic function has methods on
;;; it that are instances of STANDARD-READER-METHOD and
;;; STANDARD-WRITER-METHOD.  Such methods have the moral meaning of
;;; (SLOT-VALUE <object> '<slot-name>) and
;;; (SETF (SLOT-VALUE <object> '<slot-name>) <new-value>)
;;; where <object> and <new-value> are arguments of the generic function
;;; and <slot-name> is the name of the slot given by applying
;;; ACCESSOR-METHOD-SLOT-DEFINITION to the method metaobject.
;;;
;;; But SLOT-VALUE and (SETF SLOT-VALUE) must do a lot of work (though
;;; it is possible to speed it up), and we want the accessor to go
;;; directly to the slot location to make it fast.  The problem with
;;; that idea is that the slot location can be different in different
;;; subclasses of the class being specialized to, as given by applying
;;; METHOD-SPECIALIZERS to the methods.  It can even be an instance
;;; slot in some subclasses and shared slot in others.
;;;
;;; We handle this situation by "cheating" in the discriminating
;;; function.  Once we have determined a list of applicable methods by
;;; calling COMPUTE-APPLICABLE-METHODS-USING-CLASSES, we make a pass
;;; over them and replace any accessor method by a newly created
;;; method that does the equivalent of (STANDARD-INSTANCE-ACCESS
;;; <object> <slot-location>), where <slot-position> is calculated by
;;; getting the name of the slot from the DIRECT-SLOT-DEFINITION
;;; stored in the accessor method, using CLASS-SLOTS on the class of
;;; <object> to find the EFFECTIVE slots of the class, then finding
;;; the slot with the right name, and finally getting its location by
;;; using SLOT-DEFINITION-LOCATION.

;;; When the applicable methods for a particular list of classes has
;;; no auxiliary methods and the most specific primary method is an
;;; accessor method, then we can create an effective method that calls
;;; STANDARD-INSTANCE-ACCESS or (SETF STANDARD-INSTANCE-ACCESS) as
;;; appropriate.  For that, we need to know the slot position of the
;;; particular slot in the class of the instance being accessed.  We
;;; do that by first finding the name of the slot in the
;;; DIRECT-SLOT-DEFINITION stored in the accessor method, using
;;; CLASS-SLOTS on the class of the instance to find the EFFECTIVE
;;; slots of the class, then finding the slot with the right name, and
;;; finally getting its location by using SLOT-DEFINITION-LOCATION.

(defun determine-slot-location (class accessor-method)
  (let* ((slot-definition (accessor-method-slot-definition accessor-method))
         (slot-name (slot-definition-name slot-definition))
         (effective-slots (class-slots class))
         (effective-slot (find slot-name effective-slots
                               :key #'slot-definition-name
                               :test #'eq)))
    (slot-definition-location effective-slot)))

(defun effective-method-from-reader-method (class method)
  (let ((location (determine-slot-location class method)))
    (compile nil
             `(lambda (instance)
                (declare (ignorable instance))
                ,(if (consp location)
                     `(car ',location)
                     `(standard-instance-access instance ,location))))))

(defun effective-method-from-writer-method (class method)
  (let ((location (determine-slot-location class method)))
    (compile nil
             `(lambda (new-value instance)
                (declare (ignorable instance))
                (setf ,(if (consp location)
                           `(car ',location)
                           `(standard-instance-access instance ,location))
                      new-value)))))

(defun final-methods (methods classes)
  (loop for method in methods
        collect (maybe-replace-method method classes)))

;;; This function computes a discriminating for a generic function,
;;; given the existing call history, and then it call
;;; SET-FUNCALLABLE-INSTANCE-FUNCTION in order to install that
;;; discriminating function.
(defun compute-and-install-discriminating-function (generic-function)
  (let ((df (compute-discriminating-function generic-function)))
    (set-funcallable-instance-function generic-function df)))

;;; This function takes a generic function, a list of classes of all
;;; the required arguments, a list of relevant classes (i.e., classes
;;; of the arguments that have parameters that are specialized upon),
;;; and a list of applicable methods.  It adds an new call cache to
;;; the call history of the generic function.  If the same (EQUAL)
;;; applicable method cache A already exists in some call cache C in
;;; the call history, then the new call cache is constructed from the
;;; list of class numbers passed as an argument, A, and the
;;; effective-method cache of C.  Otherwise the new call cache is
;;; constructed from the list of class numbers passed as an argument,
;;; the list of applicable methods passed as an argument, and a new
;;; effective method obtained by calling COMPUTE-EFFECTIVE-METHOD.
;;; Either way, we return the effective method of the new call cache.
(defun add-call-cache
    (generic-function classes relevant-classes applicable-methods)
  (let* ((call-history (call-history generic-function))
         (call-cache (car (member applicable-methods call-history
                                  :key #'applicable-method-cache
                                  :test #'equal)))
         (method-combination
           (generic-function-method-combination generic-function)))
    (if (null call-cache)
        ;; No call cache exists with the same applicable method cache.
        ;; We must create a new effective method.
        (let* ((effective-method
                 (compute-effective-method
                  generic-function
                  method-combination
                  (final-methods applicable-methods classes)))
               (effective-method-function (compile nil effective-method)))
          ;; Add a new call cache to the call history.
          (setf (call-history generic-function)
                (cons (make-call-cache relevant-classes
                                       applicable-methods
                                       effective-method-function)
                      call-history))
          effective-method-function)
        ;; We already have a call cache with the same applicable
        ;; method cache.  Create an entry that reuses the existing
        ;; applicable method cache and the existing effective method.
        (let ((applicable-methods (applicable-method-cache call-cache))
              (effective-method-function (effective-method-cache call-cache)))
          (setf (call-history generic-function)
                (cons (make-call-cache relevant-classes
                                       applicable-methods
                                       effective-method-function)
                      call-history))
          effective-method-function))))

;;; This function can not itself be the discriminating function of a
;;; generic function, because it also takes the generic function
;;; itself as an argument.  However it can be called by the
;;; discriminating function, in which case the discriminating function
;;; must supply the GENERIC-FUNCTION argument either from a
;;; closed-over variable, from a compiled-in constant, or perhaps by
;;; some other mechanism.
(defun default-discriminating-function (generic-function arguments profile)
  (let* ((required-argument-count (length profile))
         (required-arguments (subseq arguments 0 required-argument-count))
         (classes (loop for argument in required-arguments
                        for p in profile
                        collect (if p (class-of argument) (find-class 't))))
         (relevant-classes (loop for class in classes
                                 for p in profile
                                 when p
                                   collect class))
         (entry (car (member relevant-classes (call-history generic-function)
                             :key #'class-cache :test #'equal))))
    (unless (null entry)
      (compute-and-install-discriminating-function generic-function)
      (return-from default-discriminating-function
        (apply generic-function arguments)))
      ;; There should never be a valid entry, because it would
      ;; then have been found by the TAGBODY preceding this code.
    ;; (error "entry found"))
    (let ((method-combination
            (generic-function-method-combination generic-function)))
      (multiple-value-bind (applicable-methods ok)
          (compute-applicable-methods-using-classes generic-function classes)
        (when ok
          (when (null applicable-methods)
            (apply #'no-applicable-method generic-function arguments))
          (let* ((effective-method-function
                   (add-call-cache generic-function
                                   classes
                                   relevant-classes
                                   applicable-methods)))
            (set-funcallable-instance-function
             generic-function
             (compute-discriminating-function generic-function))
            (return-from default-discriminating-function
              (apply generic-function arguments))))
        ;; Come here if we can't compute the applicable methods using
        ;; only the classes of the arguments.
        (let ((applicable-methods
                (compute-applicable-methods generic-function arguments)))
          (when (null applicable-methods)
            (apply #'no-applicable-method generic-function arguments))
          (let* ((effective-method
                   (compute-effective-method
                    generic-function
                    method-combination
                    (final-methods applicable-methods classes)))
                 (effective-method-function (compile nil effective-method)))
            (funcall effective-method-function arguments)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To do: Meters.
;;;
;;; Generic function invocation is an excellent opportunity for
;;; Multics-style meters.
;;;
;;; For instance, we could record:
;;;
;;;  * Total number of calls.
;;;
;;;  * Number of calls resulting in a cache miss, so that a new
;;;    discriminating function must be computed.
;;;
;;;  * Total time computing a new discriminating function.
;;;
;;; With this information, we can compute some very interesting
;;; statistics, such as the average overhead per call as a result of
;;; computing a new discriminating function, etc.
