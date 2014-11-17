(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation context.
;;;
;;; Each AST is compiled in a particular COMPILATION CONTEXT or
;;; CONTEXT for short.  A context object has two components: 
;;;
;;; 1. SUCCESSORS, which is a proper list containing zero, one or two
;;; elements.  These elements are instructions resulting from the
;;; generation of the code that should be executed AFTER the code
;;; generated from this AST.  If the list contains two elements, then
;;; this AST is compiled in a context where a Boolean result is
;;; required.  In this case, the first element of the list is the
;;; successor to use when the value generated by the AST is NIL, and
;;; the second element is the successor to use when the value
;;; generated by the AST is something other than NIL.  If there are no
;;; successors, as indicated by SUCCESSORS being the empty list, then
;;; either a TAILCALL-INSTRUCTION (if the AST is a CALL-AST) or a
;;; RETURN-INSTRUCTION (for other ASTs) that returns all the values of
;;; the AST should be generated.
;;;
;;; 2. RESULTS, a proper list indicating how many values are required
;;; from the compilation of this AST.  It contains a list of lexical
;;; locations into which the generated code must put the values of
;;; this AST.  If the list is empty, it means either that no values
;;; are required (when (PLUSP (LENGTH SUCCESSORS))) or that ALL values
;;; are requried (when (ZEROP (LENGTH SUCCESSORS))).  If the list
;;; contains more elements than the number of values generated by this
;;; AST, then the remaining lexical locations in the list must be
;;; filled with NIL by the code generated from this AST.
;;;
;;; The following combinations can occur:
;;;
;;;   SUCCESSORS is the empty list.  Then RESULTS is also the empty
;;;   list, which means that ALL the values are required.  Forms that
;;;   are in TAIL POSITION are compiled in a context like this.
;;;
;;;   SUCCESSORS has one element.  then RESULTS can have any number of
;;;   elements.
;;;
;;;      If RESULTS has no elements, this means that no values are
;;;      required.  Forms inside a PROGN other than the last are
;;;      compiled in a context like this.
;;;
;;;      If RESULTS has a single element, then a single value is
;;;      required.  Arguments to function calls are examples of ASTs
;;;      that are compiled in a context like this.
;;;
;;;      If RESULTS has more than one element, then that many values
;;;      are required.  The VALUES-FORM-AST of MULTIPLE-VALUE-BIND-AST
;;;      is compiled in a context like this.
;;;
;;;   SUCCESSOR has two elements.  Then RESULTS is the empty list,
;;;   meaning that no values are required.  The TEST-AST of an IF-AST
;;;   is compiled in a context like this. 
;;;
;;;   SUCCESSORS has more than two elements.  This possibility is
;;;   currently not used.  It is mean to be used for forms like CASE,
;;;   TYPECASE, etc.  Again, the RESULTS would be the empty list. 

(defclass context ()
  ((%results :initarg :results :reader results)
   (%successors :initarg :successors :accessor successors)
   (%invocation :initarg :invocation :reader invocation)))

(defun context (results successors invocation)
  (unless (and (listp results)
	       (every (lambda (result)
			(typep result 'cleavir-ir:lexical-location))
		      results))
    (error "illegal results: ~s" results))
  (unless (and (listp successors)
	       (every (lambda (successor)
			(typep successor 'cleavir-ir:instruction))
		      successors))
    (error "illegal successors: ~s" results))
  (when (and (null successors) (not (null results)))
    (error "Illegal combination of results and successors"))
  (unless (or (null invocation)
	      (typep invocation 'cleavir-ir:enter-instruction))
    (error "Illegal invocation"))
  (make-instance 'context
    :results results
    :successors successors
    :invocation invocation))

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream)
    (format stream " results: ~s" (results obj))
    (format stream " successors: ~s" (successors obj))))
