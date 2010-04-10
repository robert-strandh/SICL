(defclass mir-instruction () ())

(defmethod print-object :around ((instruction mir-instruction) stream)
  (format stream "[")
  (call-next-method)
  (format stream "]"))

(defclass mir-has-left-mixin ()
  ((%left :initarg :left :reader left)))

(defclass mir-operation-mixin ()
  ((%operation :initarg :operation :reader operation)))

(defclass mir-binary-expression-mixin ()
  ((%operand1 :initarg :operand1 :reader operand1)
   (%operand2 :initarg :operand1 :reader operand2)))

(defclass mir-unary-expression-mixin ()
  ((%operand :initarg :operand :reader operand)))

(defclass mir-list-expression-mixin ()
  ())

(defclass mir-no-expression-mixin ()
  ())

(defclass mir-transfer-mixin ()
  ((%label :initarg :label :accessor label)))

(defclass mir-trap-mixin ()
  ((%trap-number :initarg :trap-number :reader trap-number)))

(defclass mir-label (mir-instruction mir-no-expression-mixin)
  ((%name :initarg :name :reader name)))

(defmethod print-object ((instruction mir-label) stream)
  (format stream "~a:" (name instruction)))

(defclass mir-receive (mir-instruction mir-has-left-mixin)
  ((%parameter-type :initarg :parameter-type :reader parameter-type)))

(defmethod print-object ((instruction mir-receive) stream)
  (format stream "receive ~a (~a)" 
          (left instruction)
          (name (parameter-type instruction))))

(defclass mir-binary-assign (mir-instruction
                             mir-has-left-mixin
                             mir-binary-expression-mixin
                             mir-operation-mixin)
  ())

(defmethod print-object ((instruction mir-binary-assign) stream)
  (format stream "~a <- ~a ~a ~a"
          (left instruction)
          (operand1 instruction)
          (operation instruction)
          (operand2 instruction)))

(defclass mir-unary-assign (mir-instruction
                            mir-has-left-mixin
                            mir-unary-expression-mixin
                            mir-operation-mixin)
  ())

(defmethod print-object ((instruction mir-unary-assign) stream)
  (format stream "~a <- ~a ~a"
          (left instruction)
          (operation instruction)
          (operand instruction)))

(defclass mir-value-assign (mir-instruction
                            mir-has-left-mixin
                            mir-unary-expression-mixin)
  ())

(defmethod print-object ((instruction mir-value-assign) stream)
  (format stream "~a <- ~a"
          (left instruction)
          (operand instruction)))

(defclass mir-conditional-assign (mir-instruction
                                  mir-has-left-mixin
                                  mir-unary-expression-mixin)
  ((%condition-var :initarg :condition-var :reader condition-var)))

(defmethod print-object ((instruction mir-conditional-assign) stream)
  (format stream "~a <- (~a)? ~a"
          (left instruction)
          (condition-var instruction)
          (operand instruction)))

(defclass mir-cast-assign (mir-instruction
                           mir-has-left-mixin
                           mir-unary-expression-mixin)
  ((%type-name :initarg :type-name :reader type-name)))

(defmethod print-object ((instruction mir-cast-assign) stream)
  (format stream "~a <- (~a) ~a"
          (left instruction)
          (type-name instruction)
          (operand instruction)))

(defclass mir-indirect-assign (mir-instruction
                               mir-has-left-mixin
                               mir-unary-expression-mixin)
  ())

(defmethod print-object ((instruction mir-indirect-assign) stream)
  (format stream "*~a <- ~a"
          (left instruction)
          (operand instruction)))

(defclass mir-element-assign (mir-instruction
                              mir-has-left-mixin
                              mir-unary-expression-mixin)
  ((%element-name :initarg :element-name :reader element-name)))

(defmethod print-object ((instruction mir-element-assign) stream)
  (format stream "~a.~a <- ~a"
          (left instruction)
          (element-name instruction)
          (operand instruction)))

(defclass mir-indirect-element-assign (mir-instruction
                                       mir-has-left-mixin
                                       mir-unary-expression-mixin)
  ((%element-name :initarg :element-name :reader element-name)))

(defmethod print-object ((instruction mir-indirect-element-assign) stream)
  (format stream "*~a.~a <- ~a"
          (left instruction)
          (element-name instruction)
          (operand instruction)))

(defclass mir-goto (mir-instruction
                    mir-no-expression-mixin
                    mir-transfer-mixin)
  ())

(defmethod print-object ((instruction mir-goto) stream)
  (format stream "Goto ~a"
          (label instruction)))

(defclass mir-binary-if (mir-instruction
                         mir-binary-expression-mixin
                         mir-operation-mixin
                         mir-transfer-mixin)
  ())

(defmethod print-object ((instruction mir-binary-assign) stream)
  (format stream "~If ~a ~a ~a Goto ~a"
          (operand1 instruction)
          (operation instruction)
          (operand2 instruction)
          (label instruction)))

(defclass mir-unary-if (mir-instruction
                        mir-unary-expression-mixin
                        mir-operation-mixin
                        mir-transfer-mixin)
  ())

(defmethod print-object ((instruction mir-unary-if) stream)
  (format stream "If ~a ~a Goto ~a"
          (operation instruction)
          (operand instruction)
          (label instruction)))

(defclass mir-value-if (mir-instruction
                        mir-unary-expression-mixin)
  ((%label :initarg :label :reader label)))

(defmethod print-object ((instruction mir-value-if) stream)
  (format stream "If ~a Goto ~a" 
          (operand instruction)
          (label instruction)))

(defclass mir-binary-trap (mir-instruction
                           mir-binary-expression-mixin
                           mir-operation-mixin
                           mir-trap-mixin)
  ())

(defmethod print-object ((instruction mir-binary-assign) stream)
  (format stream "~If ~a ~a ~a Trap ~a"
          (operand1 instruction)
          (operation instruction)
          (operand2 instruction)
          (trap-number instruction)))

(defclass mir-unary-trap (mir-instruction
                          mir-unary-expression-mixin
                          mir-operation-mixin
                          mir-trap-mixin)
  ())

(defmethod print-object ((instruction mir-unary-trap) stream)
  (format stream "If ~a ~a Trap ~a"
          (operation instruction)
          (operand instruction)
          (trap-number instruction)))

(defclass mir-value-trap (mir-instruction
                          mir-unary-expression-mixin
                          mir-trap-mixin)
  ())

(defmethod print-object ((instruction mir-binary-assign) stream)
  (format stream "If ~a Trap ~a"
          (operand instruction)
          (trap-number instruction)))

(defclass mir-call (mir-instruction mir-list-expression-mixin)
  ((%procedure-name :initarg :procedure-name :reader procedure-name)
   (%arguments :initarg :arguments :reader arguments)))

(defmethod print-object ((instruction mir-call) stream)
  (format stream "~a(" (procedure-name instruction))
  (loop for argument in (arguments instruction)
        do (format stream "[~a : ~a] " (first argument) (second argument)))
  (format stream ")"))

(defclass mir-call-assign (mir-instruction
                           mir-has-left-mixin
                           mir-list-expression-mixin)
  ((%procedure-name :initarg :procedure-name :reader procedure-name)
   (%arguments :initarg :arguments :reader arguments)))

(defmethod print-object ((instruction mir-call-assign) stream)
  (format stream "~a <- ~a("
          (left instruction)
          (procedure-name instruction))
  (loop for argument in (arguments instruction)
        do (format stream "[~a : ~a] "
                          (first argument)
                          (second argument)))
  (format stream ")"))

(defclass mir-return (mir-instruction mir-no-expression-mixin)
  ())

(defmethod print-object ((instruction mir-return) stream)
  (format stream "return"))

(defclass mir-return-value (mir-instruction mir-unary-expression-mixin)
  ())

(defmethod print-object ((instruction mir-return-value) stream)
  (format stream "return ~a" (operand instruction)))

(defclass mir-sequence (mir-instruction mir-no-expression-mixin)
  ())

(defmethod print-object ((instruction mir-sequence) stream)
  (format stream "sequence"))

(defclass mir-operand ()
  ())

(defclass mir-variable (mir-operand)
  ((%name :initarg :name :reader name)))

(defmethod print-object ((variable mir-variable) stream)
  (format stream "~a" (name variable)))

(defclass mir-constant (mir-operand)
  ((%value :initarg :value :reader value)))

(defmethod print-object ((constant mir-constant) stream)
  (format stream "~a" (value constant)))

(defclass mir-parameter-type (mir-operand)
  ((%name :initarg :name :reader name)))

(defmethod print-object ((type mir-parameter-type) stream)
  (format stream "~a" (name type)))

(defclass mir-operator ()
  ((%name :initarg :name :reader name)))

(defmethod print-object ((operator mir-operator) stream)
  (format stream "~a" (name operator)))

;;; all the operators

(defmacro make-mir-operator (varname opname)
  `(defparameter ,varname (make-instance 'mir-operator :name ',opname)))

(make-mir-operator +mir-operator-add+ +)
(make-mir-operator +mir-operator-subtract+ -)
(make-mir-operator +mir-operator-multiply+ *)
(make-mir-operator +mir-operator-divide+ /)
(make-mir-operator +mir-operator-modulo+ mod)
(make-mir-operator +mir-operator-min+ min)
(make-mir-operator +mir-operator-max+ max)
(make-mir-operator +mir-operator-equal+ =)
(make-mir-operator +mir-operator-not-equal+ !=)
(make-mir-operator +mir-operator-less+ <)
(make-mir-operator +mir-operator-less-or-equal+ <=)
(make-mir-operator +mir-operator-greater+ >)
(make-mir-operator +mir-operator-greater-or-equal+ >=)
(make-mir-operator +mir-operator-shift-left+ shl)
(make-mir-operator +mir-operator-shift-right+ shr)
(make-mir-operator +mir-operator-shift-right-arithmetic+ shra)
(make-mir-operator +mir-operator-and+ and)
(make-mir-operator +mir-operator-or+ or)
(make-mir-operator +mir-operator-exclusive-or+ xor)
(make-mir-operator +mir-operator-element+ elt)
(make-mir-operator +mir-operator-indirection+ *)
(make-mir-operator +mir-operator-negation+ -)
(make-mir-operator +mir-operator-not+ !)
(make-mir-operator +mir-operator-address+ addr)
(make-mir-operator +mir-operator-cast+ *)
(make-mir-operator +mir-operator-indirect-assignment+ lind)
(make-mir-operator +mir-operator-conditional-assignment+ lcond)
(make-mir-operator +mir-operator-indirect-element-assignment+ lindelt)
(make-mir-operator +mir-operator-element-assignment+ lelt)

(defparameter +mir-type-t+ (make-instance 'mir-parameter-type :name t))

;;; A small test compiler

(defun compile-to-mir (expr result)
  (cond ((symbolp expr)
         (list (make-instance 'mir-value-assign
                              :left result
                              :operand (make-instance 'mir-variable :name expr))))
        ((numberp expr)
         (list (make-instance 'mir-value-assign
                              :left result
                              :operand (make-instance 'mir-constant :value expr))))
        ((eq (first expr) 'if)
         (let ((outlabel (make-instance 'mir-label :name (gensym)))
               (elselabel (make-instance 'mir-label :name (gensym)))
               (condresult (make-instance 'mir-variable :name (gensym))))
           (append (compile-to-mir (second expr) condresult)
                   (list (make-instance 'mir-unary-if
                                        :operation +mir-operator-not+
                                        :operand condresult
                                        :label elselabel))
                   (compile-to-mir (third expr) result)
                   (list (make-instance 'mir-goto
                                        :label outlabel)
                         elselabel)
                   (compile-to-mir (fourth expr) result)
                   (list outlabel))))
        ((eq (first expr) 'defun)
         (append (mapcar (lambda (var)
                           (make-instance 'mir-receive
                                          :left (make-instance 'mir-variable
                                                               :name var)
                                          :parameter-type +mir-type-t+))
                         (third expr))
                 (compile-to-mir (fourth expr) result)
                 (list (make-instance 'mir-return-value
                                      :operand result))))
        (t
         (let ((results (loop for arg in (rest expr)
                              collect (make-instance 'mir-variable
                                           :name (gensym)))))
           (append
            (loop for arg in (rest expr)
                  for new-result in results
                  append (compile-to-mir arg new-result))
            (list (make-instance
                   'mir-call-assign
                   :left result
                   :procedure-name (first expr)
                   :arguments
                   (loop for arg in results
                         collect (list arg +mir-type-t+)))))))))

;;; Computing basic blocks

(defclass basic-block ()
  ((%instructions :initform '() :initarg :instructions :accessor instructions)
   (%successors :initform '() :initarg :successors :accessor successors)
   (%predecessors :initform '() :initarg :predecessors :accessor predecessors)))

(defclass mir-program ()
  ((%basic-blocks :initform '() :initarg :basic-blocks :accessor basic-blocks)))

(defun make-blocks (instructions)
  (let ((pos (position-if (lambda (x) (typep x 'mir-label))
                          instructions
                          :start 1)))
    (if (null pos)
        (list (make-instance 'basic-block
                             :instructions instructions))
        (cons (make-instance 'basic-block
                             :instructions (subseq instructions 0 pos))
              (make-blocks (subseq instructions pos))))))

(defun make-program (instructions)
  ;; Make sure that there are not two consecutive labels. 
  ;; If two consecutive labels are found, eliminate the first
  ;; one and replace tansfers to it by transfers to the second one. 
  (setf instructions
        (loop for (i1 i2) on instructions
              if (and (typep i1 'mir-label)
                      (typep i2 'mir-label))
                do (loop for instruction in instructions
                         do (when (and (typep instruction 'mir-transfer-mixin)
                                       (eq (label instruction) i1))
                              (setf (label instruction) i2)))
              else
                collect i1))
  ;; eliminate any label that is not the target of any Goto
  (let ((targets (loop for instruction in instructions
                       when (typep instruction 'mir-transfer-mixin)
                         collect (label instruction))))
    (setf instructions
          (remove-if (lambda (instruction)
                       (and (typep instruction 'mir-label)
                            (not (member instruction targets))))
                     instructions)))
  ;; Eliminate unlabeled instructions following a goto
  (setf instructions
        (loop with collecting = t
              for instruction in instructions
              do (when (typep instruction 'mir-label)
                   (setf collecting t))
              when collecting
              collect instruction
              do (when (typep instruction 'mir-goto)
                   (setf collecting nil))))
  ;; Make sure a sequence of IFs is followed by a goto or a return.
  (setf instructions 
        (loop for (i1 i2) on instructions
              collect i1
              when (and (typep i1 'mir-transfer-mixin)
                        (not (typep i1 'mir-goto))
                        (not (typep i2 'mir-transfer-mixin))
                        (not (typep i2 'mir-return))
                        (not (typep i2 'mir-return-value)))
                append (let ((label (make-instance 'mir-label
                                                   :name (gensym))))
                         (list (make-instance 'mir-goto :label label)
                               label))))
  ;; Make sure  there are no fall-througs
  (setf instructions
        (loop for (i1 i2) on instructions
              collect i1
              when (and (typep i2 'mir-label)
                        (not (typep i1 'mir-goto)))
                collect (make-instance 'mir-goto :label i2)))
  (let* ((label (make-instance 'mir-label :name (gensym)))
         (blocks (append (cons (make-instance
                                'basic-block ; the entry block
                                :instructions (list (make-instance
                                                     'mir-goto
                                                     :label label)))
                               (make-blocks
                                (cons label instructions)))
                         (list (make-instance
                                'basic-block ; the exit block
                                :instructions '())))))
    ;; Add the exit block as a successor to any block that
    ;; ends with a return instruction, and add that block as
    ;; a predecessor to the exit block.
    (loop with exit-block = (car (last blocks))
          for block in blocks
          do (let ((last-instruction (car (last (instructions block)))))
               (when (or (typep last-instruction 'mir-return)
                         (typep last-instruction 'mir-return-value))
                 (push exit-block (successors block))
                 (push block (predecessors exit-block)))))
    ;; For any two blocks b1 and b2 such that b1 has a a transfer
    ;; to a label that is the first instruction of b2, add
    ;; b2 as a successor of b1 and b1 as a predecessor of b2, and
    ;; replace the label of the transfer by b2 itself
    (loop for b1 in blocks
          do (loop for instruction in (instructions b1)
                   do (when (typep instruction 'mir-transfer-mixin)
                        (let ((b2 (find (label instruction)
                                        blocks
                                        :key (lambda (b)
                                               (first (instructions b))))))
                          (setf (label instruction) b2)
                          (push b2 (successors b1))
                          (push b1 (predecessors b1))))))
    ;; Finally remove all label instructions
    (loop for block in blocks
          do (when (typep (first (instructions block)) 'mir-label)
               (pop (instructions block))))
    blocks))
