;;;; This example shows an AST that implements the CAR function.
;;;;
;;;; It corresponds-to the following approximate source code:
;;;;
;;;; (lambda (object)
;;;;   (if (typeq object null)
;;;;       nil
;;;;       (if (typeq object cons)
;;;;           (%car object)
;;;;           (error 'type-error 
;;;;                  :datum object
;;;;                  :expected-type '(or cons null)))))
;;;;
;;;; Where TYPEQ is a hypothetical special operator that produces the
;;;; TYPEQ-AST, and %CAR is a hypothetical special operator that
;;;; produces the CAR-AST.
;;;;
;;;; The format of the AST below has been edited slightly compared to
;;;; the raw output from PRINT in order to be easier to read.  When
;;;; read, it still produces a correct AST, however.

[CLEAVIR-AST:FUNCTION-AST
   :LAMBDA-LIST (#1=[CLEAVIR-AST:LEXICAL-AST
                       :CHILDREN COMMON-LISP:NIL
                       :NAME CLEAVIR-AST::OBJECT ])
   :CHILDREN 
      ([CLEAVIR-AST:IF-AST
	  :CHILDREN 
	     ([CLEAVIR-AST::TYPEQ-AST
		 :CHILDREN (#1# 
			    [CLEAVIR-AST:CONSTANT-AST
			       :CHILDREN COMMON-LISP:NIL
			       :VALUE COMMON-LISP:NULL ]) ]
	      [CLEAVIR-AST:CONSTANT-AST
		 :CHILDREN COMMON-LISP:NIL
		 :VALUE COMMON-LISP:NIL ]
	      [CLEAVIR-AST:IF-AST
		 :CHILDREN 
		    ([CLEAVIR-AST::TYPEQ-AST
			:CHILDREN (#1# 
                                   [CLEAVIR-AST:CONSTANT-AST
				      :CHILDREN COMMON-LISP:NIL
				      :VALUE COMMON-LISP:CONS ]) ]
		     [CLEAVIR-AST::CAR-AST
			:CHILDREN (#1#) ]
		     [CLEAVIR-AST:CALL-AST
			:CHILDREN 
			   ([CLEAVIR-AST:GLOBAL-AST
			       :CHILDREN COMMON-LISP:NIL
			       :NAME COMMON-LISP:ERROR
			       :FUNCTION-TYPE COMMON-LISP:T ]
			    [CLEAVIR-AST:CONSTANT-AST
			       :CHILDREN COMMON-LISP:NIL
			       :VALUE COMMON-LISP:TYPE-ERROR ]
			    [CLEAVIR-AST:CONSTANT-AST
			       :CHILDREN COMMON-LISP:NIL
			       :VALUE :DATUM ]
			    #1#
			    [CLEAVIR-AST:CONSTANT-AST
			       :CHILDREN COMMON-LISP:NIL
			       :VALUE :EXPECTED-TYPE ]
			    [CLEAVIR-AST:CONSTANT-AST
			       :CHILDREN COMMON-LISP:NIL
			       :VALUE (COMMON-LISP:OR
				       COMMON-LISP:CONS
				       COMMON-LISP:NULL) ]) ]) ]) ]) ] 
