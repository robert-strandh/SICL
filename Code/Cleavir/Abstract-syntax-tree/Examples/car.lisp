;;;; This example shows an AST that implements the CAR function.
;;;;
;;;; It corresponds to the following source code:
;;;;
;;;; (lambda (object)
;;;;   (if (cleavir-primop:typeq object null)
;;;;       nil
;;;;       (if (cleavir-primop:typeq object cons)
;;;;           (cleavir-primop:car object)
;;;;           (error 'type-error 
;;;;                  :datum object
;;;;                  :expected-type '(or cons null)))))
;;;;
;;;; Where CLEAVIR-PRIMOP:TYPEQ is a special operator that produces
;;;; the TYPEQ-AST, and CLEAVIR-PRIMOP:CAR is a special operator that
;;;; produces the CAR-AST.

[CLEAVIR-AST:FUNCTION-AST
   :LAMBDA-LIST
   (#1=[CLEAVIR-AST:LEXICAL-AST
          :NAME
          COMMON-LISP-USER::X
          :CHILDREN
          COMMON-LISP:NIL ])
   :CHILDREN
   ([CLEAVIR-AST:PROGN-AST
       :CHILDREN
       ([CLEAVIR-AST:IF-AST
           :CHILDREN
           ([CLEAVIR-AST:TYPEQ-AST
               :CHILDREN
               (#1#
                [CLEAVIR-AST:CONSTANT-AST
                   :VALUE
                   COMMON-LISP:NULL
                   :CHILDREN
                   COMMON-LISP:NIL ]) ]
            [CLEAVIR-AST:CONSTANT-AST
               :VALUE
               COMMON-LISP:NIL
               :CHILDREN
               COMMON-LISP:NIL ]
            [CLEAVIR-AST:IF-AST
               :CHILDREN
               ([CLEAVIR-AST:TYPEQ-AST
                   :CHILDREN
                   (#1#
                    [CLEAVIR-AST:CONSTANT-AST
                       :VALUE
                       COMMON-LISP:CONS
                       :CHILDREN
                       COMMON-LISP:NIL ]) ]
                [CLEAVIR-AST:CAR-AST :CHILDREN (#1#) ]
                [CLEAVIR-AST:CALL-AST
                   :CHILDREN
                   ([CLEAVIR-AST:GLOBAL-AST
                       :FUNCTION-TYPE
                       (COMMON-LISP:AND)
                       :NAME
                       COMMON-LISP:ERROR
                       :CHILDREN
                       COMMON-LISP:NIL ]
                    [CLEAVIR-AST:CONSTANT-AST
                       :VALUE
                       COMMON-LISP:TYPE-ERROR
                       :CHILDREN
                       COMMON-LISP:NIL ]
                    [CLEAVIR-AST:CONSTANT-AST
                       :VALUE
                       :DATUM
                       :CHILDREN
                       COMMON-LISP:NIL ]
                    #1#
                    [CLEAVIR-AST:CONSTANT-AST
                       :VALUE
                       :EXPECTED-TYPE
                       :CHILDREN
                       COMMON-LISP:NIL ]
                    [CLEAVIR-AST:CONSTANT-AST
                       :VALUE
                       (COMMON-LISP:OR COMMON-LISP:NULL COMMON-LISP:CONS)
                       :CHILDREN
                       COMMON-LISP:NIL ]) ]) ]) ]) ]) ] 
