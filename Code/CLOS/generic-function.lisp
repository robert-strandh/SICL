(cl:in-package #:sicl-clos)

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
