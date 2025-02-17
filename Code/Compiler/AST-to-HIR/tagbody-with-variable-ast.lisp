(cl:in-package #:sicl-ast-to-hir)

;;; When we translate a GO-WITH-VARIABLE-AST, we do not have access to
;;; the corresponding TAGBODY-WITH-VARIABLE-AST, but we need to
;;; translate the contents of the INDEX slot of the
;;; GO-WITH-VARIABLE-AST into a successor instruction for the
;;; resulting UNWIND-INSTRUCTION.  Furthermore, that successor
;;; instruction may not exist when the GO-WITH-VARIABLE-AST is
;;; translated.  So this variable maps the VARIABLE-DEFINITION-AST of
;;; the TAGBODY-WITH-VARIABLE-AST to a vector that will ultimately
;;; contain the first instruction of each tagbody segment starting
;;; with a tag.
(defparameter *tagbody-vectors* '())

(defmethod translate-ast (client (ast ico:tagbody-segment-ast))
  (translate-implicit-progn client (ico:statement-asts ast)))

(defmethod translate-ast (client (ast ico:tagbody-with-variable-ast))
  (let* ((variable-definition-ast (ico:variable-definition-ast ast))
         (segment-asts (ico:segment-asts ast))
         (tag-count
           (if (null segment-asts)
               0
               (+ (length segment-asts)
                  (if (null (ico:tag-ast (first segment-asts))) -1 0))))
         (instruction-vector (make-array tag-count)))
    (let* ((current-dynamic-environment-register
             *dynamic-environment-register*)
           (*dynamic-environment-register*
             (make-instance 'hir:single-value-register))
           (*next-instruction*
             (if (null *target-register*)
                 *next-instruction*
                 (make-instance 'hir:assignment-instruction
                   :inputs (list (make-instance 'hir:literal :value nil))
                   :outputs *target-register*)))
           (*target-register* nil)
           (*tagbody-vectors*
             (acons variable-definition-ast instruction-vector
                    *tagbody-vectors*))
           (identity-register (make-instance 'hir:single-value-register)))
      (setf (find-register variable-definition-ast) identity-register)
      (loop for segment-ast in (reverse segment-asts)
            for index downfrom (1- tag-count)
            do (setf *next-instruction*
                     (translate-ast client segment-ast))
               (unless (null (ico:tag-ast segment-ast))
                 (setf (svref instruction-vector index)
                       *next-instruction*)))
      (make-instance 'hir:exit-point-instruction
        :inputs (list current-dynamic-environment-register)
        :outputs (list *dynamic-environment-register*
                       identity-register)
        :successors (list *next-instruction*)))))
          
      
    
                   
                 
    
