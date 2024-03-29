(in-package #:sicl-hir-evaluator)

;;; This file introduces a convenient macro for creating and registering
;;; instruction thunks.  It automatically handles destructuring of inputs,
;;; outputs, and successors and provides a symbol macro for manipulating
;;; the current dynamic environment.
;;;
;;; This macro also ensures that thunks are created and registered before
;;; any successors are converted.  This way, even circular instruction
;;; graphs can be converted with ease.

;;; I have a feeling that we don't need to call host THROW to
;;; synchronize the dynamic environments of the host and the target.
;;; So I have commented out the use of the prologue and the epilogue.

(defmacro make-thunk ((client instruction lexical-environment
                       &key (inputs 0) (outputs 0) (successors 1))
                      &body body)
  (sicl-utilities:once-only (client instruction lexical-environment)
    (let ((thunk-gensym (gensym "THUNK"))
          (lexical-locations (gensym "LEXICAL-LOCATIONS"))
          (dynamic-environment-gensym-1 (gensym "DYNAMIC-ENVIRONMENT"))
          (dynamic-environment-gensym-2 (gensym "DYNAMIC-ENVIRONMENT"))
          (input-gensyms (loop repeat inputs collect (gensym "INPUT")))
          (output-gensyms (loop repeat outputs collect (gensym "OUTPUT")))
          (successor-gensyms (loop repeat successors collect (gensym "SUCCESSOR")))
          (successor-thunk-gensyms (loop repeat successors collect (gensym "SUCCESSOR-THUNK"))))
      `(destructuring-bind (,@input-gensyms &rest _)
           (cleavir-ir:inputs ,instruction)
         (declare (ignore _))
         (destructuring-bind (,@output-gensyms &rest _)
             (cleavir-ir:outputs ,instruction)
           (declare (ignore _))
           (destructuring-bind (,@successor-gensyms &rest _)
               (cleavir-ir:successors ,instruction)
             (declare (ignore _))
             (let ((,dynamic-environment-gensym-1
                     (ensure-lref 'dynamic-environment ,lexical-environment))
                   (,dynamic-environment-gensym-2
                     (ensure-lref (cleavir-ir:dynamic-environment-location ,instruction)
                                 ,lexical-environment))
                   ,@(loop for input-gensym in input-gensyms
                           collect
                           `(,input-gensym (ensure-lref ,input-gensym ,lexical-environment)))
                   ,@(loop for output-gensym in output-gensyms
                           collect
                           `(,output-gensym (ensure-lref ,output-gensym ,lexical-environment)))
                   ,@(loop for successor-thunk-gensym in successor-thunk-gensyms
                           collect
                           `(,successor-thunk-gensym #'dummy-successor)))
               (declare (function ,@successor-thunk-gensyms))
               (declare (ignorable
                         ,dynamic-environment-gensym-1
                         ,dynamic-environment-gensym-2))
               (macrolet ((lref (lref)
                            `(%lref ,',lexical-locations ,lref))
                          (input (index)
                            (case index
                              ,@(loop for input-gensym in input-gensyms
                                      for index from 0
                                      collect
                                      `((,index) `(lref ,',input-gensym)))
                              (otherwise
                               (error "Invalid input index: ~S" index))))
                          (output (index)
                            (case index
                              ,@(loop for output-gensym in output-gensyms
                                      for index from 0
                                      collect
                                      `((,index) `(lref ,',output-gensym)))
                              (otherwise
                               (error "Invalid output index: ~S" index))))
                          (successor (index)
                            (case index
                              ,@(loop for successor-thunk-gensym in successor-thunk-gensyms
                                      for index from 0
                                      collect
                                      `((,index) ',successor-thunk-gensym))
                              (otherwise
                               (error "Invalid successor index: ~S" index))))
                          (lexical-locations ()
                            ',lexical-locations))
                 (let ((,thunk-gensym
                         (symbol-macrolet
                             ((dynamic-environment (lref ,dynamic-environment-gensym-2)))
                           (lambda (,lexical-locations)
                             (declare (simple-vector ,lexical-locations))
                             (declare (ignorable ,lexical-locations))
                             (progn ,@body)))))
                   (setf (gethash ,instruction *instruction-thunks*)
                         ,thunk-gensym)
                   ,client ; Touch the client to avoid warnings when there
                           ; are zero successors.
                   ,@(loop for successor-gensym in successor-gensyms
                           for successor-thunk-gensym in successor-thunk-gensyms
                           collect
                           `(setf ,successor-thunk-gensym
                                  (instruction-thunk ,client ,successor-gensym ,lexical-environment))
                           collect
                           `(check-type ,successor-thunk-gensym function))
                   ,thunk-gensym)))))))))

(defun dummy-successor ()
  (error "Invocation of the dummy successor."))
