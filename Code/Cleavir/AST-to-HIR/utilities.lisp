(cl:in-package #:cleavir-ast-to-hir)

;;; During compilation, this variable contains a hash table that maps
;;; ASTs representing locations to HIR locations.
(defvar *location-info*)

;;; Given an AST of type LEXICAL-LOCATION, return a corresponding HIR
;;; lexical location.  If no corresponding HIR location is found, one
;;; is created and retured, and made to correspond to the AST in
;;; future invocations.
(defun find-or-create-location (ast)
  (or (gethash ast *location-info*)
      (let ((location
              (etypecase ast
                (cleavir-ast:lexical-ast
                 (cleavir-ir:make-lexical-location
                  (cleavir-ast:name ast))))))
        (setf (gethash ast *location-info*) location))))

;;; Convenience function to avoid having long function names.
(defun make-temp ()
  (cleavir-ir:new-temporary))

;;; Given a list of results and a successor, generate a sequence of
;;; instructions preceding that successor, and that assign NIL to each
;;; result in the list.
(defun nil-fill (results successor)
  (let ((next successor))
    (loop for value in results
          do (setf next
                   (make-instance 'cleavir-ir:assignment-instruction
                    :input (cleavir-ir:make-constant-input 'nil)
                    :output value
                    :successor next))
          finally (return next))))
