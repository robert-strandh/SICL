(cl:in-package #:cleavir-dataflow-analysis)

(defclass port ()
  (;; The operation to which this port belongs.
   (%operation :initarg :operation :reader operation)
   ;; The datum that this port is using or defining.
   (%datum :initarg :datum :reader datum)))

(defclass input (port)
  ((%outputs :initform '() :initarg outputs :accessor outputs)))

(defclass output (port)
  ((%inputs :initform '() :initarg inputs :accessor inputs)))

(defclass operation ()
  ((%instruction :initarg :instruction :reader instruction)
   (%inputs :initform '() :initarg inputs :reader inputs)
   (%outputs :initform '() :initarg outputs :reader outputs)))

(defclass dataflow ()
  (;; This hash table maps each instruction to its corresponding
   ;; operation
   (%operations :initform (make-hash-table :test #'eq)
                :initarg :operations
                :accessor operations)))

(defun propagage-output (output operations)
  (let ((instruction (instruction (operation output)))
        (datum (datum output))
        (work-list '())
        (on-work-list (make-hash-table :test #'eq)))
    (flet ((possibly-add-to-work-list (instruction)
             (unless (gethash instruction on-work-list)
               (setf (gethash instruction on-work-list) t)
               (push instruction work-list))))
      (loop for successor in (cleavir-ir:successors instruction)
            do (possibly-add-to-work-list successor))
      (loop until (null work-list)
            do (let* ((instruction (pop work-list))
                      (operation (gethash instruction operations))
                      (input (find datum (inputs operation)
                                   :key #'car :test #'eq)))
                 (unless (null input)
                   (pushnew output (outputs input)))
                 (unless (member datum (cleavir-ir:outputs instruction)
                                 :test #'eq)
                   (loop for successor in (cleavir-ir:successors instruction)
                         do (possibly-add-to-work-list successor))))))))

(defun dataflow-analysis (initial-instruction)
  (let ((result (make-instance 'dataflow)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (let* ((operation (make-instance 'operation
                           :instruction instruction))
              (inputs (loop for input in (cleavir-ir:inputs instruction)
                            collect (make-instance 'input
                                      :operation operation
                                      :datum input)))
              (outputs (loop for output in (cleavir-ir:outputs instruction)
                             collect (make-instance 'output
                                       :operation operation
                                       :datum output))))
         (reinitialize-instance operation
                                :inputs inputs
                                :outputs outputs)
         (setf (gethash instruction (operations result))
               operation)))
     initial-instruction)
    result))
