(cl:in-package #:cleavir-hir-transformations)

(defun data (instruction)
  (append (cleavir-ir:inputs instruction)
          (cleavir-ir:outputs instruction)))

;;; By SEGREGATING lexical locations, we mean taking each lexical
;;; location and determining whether it is an exclusive location or a
;;; shared location.  If it is an exclusive location, it can be
;;; allocated on the stack or in a register.  If it a shared location,
;;; we may have to transmit it to inner functions through the static
;;; environment of those functions.  In the worst case, if the
;;; location is assigned to, it may have to be contained in a cell in
;;; the static environment.

;;; The method used here is very simple, and not particularly
;;; sophisticated.  It assumes that every nested function can escape
;;; in arbitrary ways, so that every lexical location that is shared
;;; by some function F and some other function G nested inside F must
;;; be treated as a shared location accordingly.
;;;
;;; LOCATION-OWNERS is an EQ hash table mapping each lexical location
;;; to its owner.
;;;
;;; We return a list of the lexical locations that have been
;;; categorized as shared lexical locations.

(defun segregate-lexicals (initial-instruction location-owners)
  (let ((result '()))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (loop for datum in (data instruction)
             do (when (typep datum 'cleavir-ir:lexical-location)
                  (unless (eq owner (gethash datum location-owners))
                    (pushnew datum result)))))
     initial-instruction)
    result))

;;; Given a shared lexical location and an EQ hash table giving the
;;; owner of each instruction, return a list of all the functions
;;; (represented by ENTER-INSTRUCTIONs) that use (read or write) the
;;; lexical location, i.e., a list of the owners of all instructions
;;; that use the lexical location.
;;;
;;; We are calling the two function CLEAVIR-IR:DEFINING-INSTRUCTIONS
;;; and CLEAVIR-IR:USING-INSTRUCTIONS, so this information must be up
;;; to date.
(defun functions-using-location (location instruction-owners)
  (let ((instructions (append (cleavir-ir:defining-instructions location)
                              (cleavir-ir:using-instructions location)))
        (result '()))
    (loop for instruction in instructions
          do (pushnew (gethash instruction instruction-owners) result
                      :test #'eq))
    result))

;;; Given a list of ENTER-INSTRUCTIONs, return an alist mapping each
;;; ENTER-INSTRUCTION to a new exclusive lexical location.
(defun allocate-exclusive-locations (enter-instructions location)
  (let ((name (if (read-only-location-p location)
                  (string (cleavir-ir:name location))
                  "CELL")))
    (loop for enter-instruction in enter-instructions
          collect (cons enter-instruction
                        (cleavir-ir:new-temporary name)))))

;;; Given an ENCLOSE-INSTRUCTION and the exclusive lexical location ,
;;; import the exclusive lexical location to the ENCLOSE-INSTRUCTION
;;; (IMPORT).
;;;
;;; We add the import to the end of the import list in order to
;;; preserve the index of cells in the static environment.
(defun transmit-exclusive-location (enclose import instruction-owners)
  (let ((initializer (cleavir-ir:initializer enclose)))
    (unless initializer
      (setf initializer
            ;; The exclusive lexical variable that holds the static
            ;; environment is the first output of the enter instruction.
            (make-instance 'cleavir-ir:initialize-closure-instruction
                           :input (first (cleavir-ir:outputs enclose))))
      ;; Defer initialization until all potentially mutually recurisve
      ;; functions are available.
      (do ((current-enclose enclose (first (cleavir-ir:successors current-enclose))))
          ((not (typep (first (cleavir-ir:successors current-enclose))
                       'cleavir-ir:enclose-instruction))
           (cleavir-ir:insert-instruction-after initializer current-enclose)))
      (setf (cleavir-ir:initializer enclose) initializer)
      (setf (gethash initializer instruction-owners)
            (gethash enclose instruction-owners)))
    ;; Start by adding the new import to the end of the existing
    ;; imports of the INITIALIZE-CLOSURE-INSTRUCTION.
    (let ((imports (cleavir-ir:inputs initializer)))
      (setf (cleavir-ir:inputs initializer)
            (append imports (list import))))))

;;; Add a fetch instruction after ENTER so that the exclusive-location is accessible.
(defun add-fetch (enter dloc)
  (let ((env-location (cleavir-ir:static-environment enter))
        ;; the index of the new cell in the closure will just be the
        ;; size of the existing closure vector.
        (new-index (cleavir-ir:closure-size enter)))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:fetch-instruction
       :inputs (list env-location
                (make-instance 'cleavir-ir:constant-input :value new-index))
       :output dloc)
     enter)
    ;; Make sure we keep track of the closure vector expansion.
    (setf (cleavir-ir:closure-size enter) (1+ new-index))))

;;; For a single shared lexical location to be eliminated, make sure
;;; that the corresponding exclusive location, be it a cell or not, is
;;; available in all functions in which the shared location is either
;;; read or written, as well as in the intermediate functions that
;;; neither read nor write the shared location, but that occur between
;;; such a function and the owner of the shared lexical location.
;;; FUNCTION-DAG is a function dag defining the nesting of functions.
;;; DYNAMIC-LOCATIONS is an alist mapping an ENTER-INSTRUCTION to the
;;; exclusive lexical location that holds the cell for the shared
;;; lexical location in the function represented by that
;;; ENTER-INSTRUCTION.  ENTER-INSTRUCTIONs of intermediate functions
;;; are also present in EXCLUSIVE-LOCATIONS.  OWNER is the
;;; ENTER-INSTRUCTION that is the owner of the shared lexical location
;;; to be eliminated.
;;;
;;; We proceed as follows: If the exclusive lexical locations are cells,
;;; we insert new instructions after each ENTER-INSTRUCTION to write
;;; the cell into the exclusive lexical variable supplied for that
;;; purpose.  After OWNER, we insert a CREATE-CELL instruction.  For
;;; every other ENTER-INSTRUCTION in EXCLUSIVE-LOCATIONS we add a FETCH
;;; instruction after it, and we import the exclusive location of the
;;; parent function into the corresponding ENCLOSE-INSTRUCTION.
(defun ensure-exclusive-location-available (function-dag exclusive-locations owner read-only-location-p instruction-owners)
  (unless read-only-location-p
    ;; Start by creating a CREATE-CELL-INSTRUCTION after the owner of
    ;; the static lexical location to be eliminated.
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:create-cell-instruction
                    :output (cdr (assoc owner exclusive-locations)))
     owner))
  ;; Next, for each entry in EXCLUSIVE-LOCATIONS other than OWNER, transmit
  ;; the cell from the corresponding ENCLOSE-INSTRUCTION to the
  ;; ENTER-INSTRUCTION of that entry.
  (loop with dag-nodes = (dag-nodes function-dag)
	for (enter . exclusive-location) in exclusive-locations
	unless (eq enter owner)
	  do (loop for node in (gethash enter dag-nodes)
                   for enclose = (enclose-instruction node)
                   for parents = (parents node)
                   do (loop for parent in parents
                            for parent-enter = (enter-instruction parent)
                            for import = (cdr (assoc parent-enter exclusive-locations))
                            do (transmit-exclusive-location enclose import instruction-owners)))
             (add-fetch enter exclusive-location)))

;;; Given a list of ENTER-INSTRUCTIONs representing the functions that
;;; read or write some particular shared lexical location to
;;; eliminate, return a another list of ENTER-INSTRUCTIONs which is
;;; like the one passed as an argument, except that ENTER-INSTRUCTIONs
;;; representing intermediate functions have been added.  An
;;; intermediate function is one that neither reads nor writes the
;;; location, but that has an ancestor and a descendant that both do.
;;; FUNCTION-DAG is a function dag representing all the functions of
;;; the program.  OWNER is the ENTER-INSTRUCTION of the outermost
;;; function in the program that reads or writes the location.
(defun add-intermediate-functions (enter-instructions function-dag owner)
  (loop with todo = (loop for enter in enter-instructions
                          appending (gethash enter (dag-nodes function-dag)))
        with result = nil
        until (null todo)
        do (let* ((node (pop todo))
                  (enter (enter-instruction node)))
             (pushnew enter result :test #'eq)
             #+(or)
             (format t "Node: ~a~% Parents: ~a~%"
                     node (parents node))
             (unless (eq enter owner)
               (setf todo (append todo (parents node)))))
        finally (return result)))

;;; Given a single shared lexical location SLOC, a exclusive lexical
;;; location CLOC holding the cell that replaces SLOC, and a single
;;; instruction INSTRUCTION that reads that location, create a new
;;; temporary exclusive lexical location D to hold the value of CLOC,
;;; insert a new READ-CELL instruction before INSTRUCTION that puts
;;; the contents of CLOC in SLOC, and replace all occurrences of SLOC
;;; in the inputs of I by D.
(defun replace-inputs (sloc cloc instruction)
  (let ((d (cleavir-ir:new-temporary)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:read-cell-instruction
       :input cloc
       :output d)
     instruction)
    (cleavir-ir:substitute-input d sloc instruction)))

;;; Given a single shared lexical location SLOC, a exclusive lexical
;;; location CLOC holding the cell that replaces SLOC, and a single
;;; instruction INSTRUCTION that writes that location, create a new
;;; temporary exclusive lexical location D to hold the value of CLOC,
;;; replace all occurrences of SLOC in the outputs of I by D, and
;;; insert a new WRITE-CELL instruction after INSTRUCTION that puts
;;; the value of D in CLOC.
(defun replace-outputs (sloc cloc instruction)
  (let ((d (cleavir-ir:new-temporary)))
    (cleavir-ir:substitute-output d sloc instruction)
    ;; CATCH has two successors and one output. There are other instructions
    ;; like that, but CATCH is the only one whose output is closed over in
    ;; normal code. Due to its unusual control flow, we special case it here.
    (if (typep instruction 'cleavir-ir:catch-instruction)
        (cleavir-ir:insert-instruction-between
         (make-instance 'cleavir-ir:write-cell-instruction
           :inputs (list cloc d)
           :outputs '())
         instruction
         (first (cleavir-ir:successors instruction)))
        (cleavir-ir:insert-instruction-after
         (make-instance 'cleavir-ir:write-cell-instruction
           :inputs (list cloc d)
           :outputs '())
         instruction))))

(defun read-only-location-p (location)
  (let ((definers (cleavir-ir:defining-instructions location)))
    (and definers (null (rest definers)))))

;;; Given a single shared lexical location LOCATION, if the lexical is
;;; assigned to more than once, eliminate it by replacing all accesses
;;; to it by accesses to a corresponding CELL.  Otherwise, it can be
;;; allocated directly to the static environment.  FUNCTION-DAG is the
;;; function dag of the entire program. INSTRUCTION-OWNERS is a hash
;;; table mapping an instruction to its owner.  OWNER is the owner of
;;; LOCATION.
(defun process-location
    (location function-dag instruction-owners owner)
  (let* (;; Determine all the functions (represented by
         ;; ENTER-INSTRUCTIONs) that use (read or write) the location.
         (users (functions-using-location location instruction-owners))
         ;; To that set, add the intermediate functions.
         (enters (add-intermediate-functions users function-dag owner))
         (read-only-location-p (read-only-location-p location))
         ;; Compute a dictionary that associates each
         ;; ENTER-INSTRUCTION with an exclusive location.
         (exclusive-locations (allocate-exclusive-locations enters location)))
    (loop for instruction in (cleavir-ir:using-instructions location)
          for instruction-owner = (gethash instruction instruction-owners)
          for exclusive-location = (cdr (assoc instruction-owner exclusive-locations))
          do (if read-only-location-p
                 (cleavir-ir:substitute-input exclusive-location location instruction)
                 (replace-inputs location exclusive-location instruction)))
    (loop for instruction in (cleavir-ir:defining-instructions location)
          for instruction-owner = (gethash instruction instruction-owners)
          for exclusive-location = (cdr (assoc instruction-owner exclusive-locations))
          do (if read-only-location-p
                 (cleavir-ir:substitute-output exclusive-location location instruction)
                 (replace-outputs location exclusive-location instruction)))
    ;; We do this step last, so that we are sure that the CREATE-CELL
    ;; and FETCH instructions are inserted immediately after the ENTER
    ;; instruction.
    (ensure-exclusive-location-available function-dag
                                         exclusive-locations
                                         owner
                                         read-only-location-p
                                         instruction-owners)))

(defun process-captured-variables (initial-instruction)
  ;; Make sure everything is up to date.
  (cleavir-ir:reinitialize-data initial-instruction)
  (let* ((instruction-owners (compute-instruction-owners initial-instruction))
         (location-owners (compute-location-owners initial-instruction))
         (function-dag (build-function-dag initial-instruction))
         (shared-locations
           (segregate-lexicals initial-instruction location-owners)))
    (loop for shared-location in shared-locations
          for owner = (gethash shared-location location-owners)
          do (process-location shared-location
                               function-dag
                               instruction-owners
                               owner))))

(defun segregate-only (initial-instruction)
  ;; Make sure everything is up to date.
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((location-owners (compute-location-owners initial-instruction)))
    (segregate-lexicals initial-instruction location-owners)))
