(cl:in-package #:cleavir-hir-transformations)

(defun data (instruction)
  (append (cleavir-ir:inputs instruction)
	  (cleavir-ir:outputs instruction)))

;;; By SEGREGATING lexical locations, we mean taking each lexical
;;; location and turning it into either a dynamic lexical location
;;; (which can be allocated in a register or on the stack) or a
;;; static lexical location (which may or may not be possible to
;;; allocate on the stack, and might have to be allocated in some
;;; other place, possibly on the heap).
;;;
;;; The method used here is very simple, and not particularly
;;; sophisticated.  It assumes that every nested function can escape
;;; in arbitrary ways, so that every lexical location that is shared
;;; by some function F and some other function G nested inside F must
;;; be an static lexical location.
;;;
;;; LOCATION-OWNERS is an EQ hash table mapping each lexical location
;;; to its owner.
;;;
;;; We return a list of the lexical locations that have been
;;; categorized as static lexical locations.

(defparameter *segregate-lexicals-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "SEGREGATE-LEXICALS-METER"))

(defun segregate-lexicals (initial-instruction location-owners)
  (cleavir-meter:with-meter (m *segregate-lexicals-meter*)
    (let ((result '()))
      (cleavir-ir:map-instructions-with-owner
       (lambda (instruction owner)
	 (cleavir-meter:increment-size m)
	 (loop for datum in (data instruction)
	       do (when (typep datum 'cleavir-ir:lexical-location)
		    (unless (eq owner (gethash datum location-owners))
                      (pushnew datum result)))))
       initial-instruction)
      result)))

;;; Given a static lexical location and an EQ hash table giving the
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
;;; ENTER-INSTRUCTION to a new dynamic lexical location for the
;;; purpose of holding a CELL associated with a static lexical
;;; location to be eliminated.
(defun allocate-cell-locations (enter-instructions)
  (loop for enter-instruction in enter-instructions
	collect (cons enter-instruction
                      (cleavir-ir:new-temporary "CELL"))))

;;; Given an ENCLOSE-INSTRUCTION and the associated ENTER-INSTRUCTION,
;;; as well as the dynamic lexical location holding a cell in the
;;; function defined by the owner of the ENCLOSE-INSTRUCTION (IMPORT)
;;; and the dynamic lexical location holding the same cell in function
;;; defined by ENTER-INSTRUCTION (EXPORT), import the cell to the
;;; ENCLOSE-INSTRUCTION and add a FETCH-INSTRUCTION immediately after
;;; ENTER-INSTRUCTION that makes the cell available in the function
;;; defined by ENTER-INSTRUCTION.
;;;
;;; We add the import to the end of the import list in order to
;;; preserve the index of cells in the static environment.
(defun transmit-cell (enclose import enter export)
  (let ((imports (cleavir-ir:inputs enclose))
	;; The dynamic lexical variable that holds the static
	;; environment is the first output of the enter instruction.
	(env-location (first (cleavir-ir:outputs enter)))
	(cleavir-ir:*policy* (cleavir-ir:policy enter)))
    ;; Start by adding the new import to the end of the existing
    ;; imports of the ENCLOSE-INSTRUCTION.
    (setf (cleavir-ir:inputs enclose) (append imports (list import)))
    ;; Finally, add a new FETCH-INSTRUCTION after ENTER.
    (cleavir-ir:insert-instruction-after
     (cleavir-ir:make-fetch-instruction
      env-location
      ;; The index of the cell in the static environment is the
      ;; position of the import in the ENCLOSE-INSTRUCTION which is
      ;; the same as the length of the imports before we add the new
      ;; one.
      (cleavir-ir:make-immediate-input (length imports))
      export)
     enter)))

;;; For a single static lexical location to be eliminated, make sure
;;; that the corresponding cell is available in all functions in which
;;; the static location is either read or written, as well as in the
;;; intermediate functions that neither read nor write the static
;;; location, but that occur between such a function and the owner of
;;; the static lexical location.  FUNCTION-TREE is a function tree
;;; defining the nesting of functions.  CELL-LOCATIONS is an alist
;;; mapping an ENTER-INSTRUCTION to the dynamic lexical location that
;;; holds the cell for the static lexical location in the function
;;; represented by that ENTER-INSTRUCTION.  ENTER-INSTRUCTIONs of
;;; intermediate functions are also present in CELL-LOCATIONS.  OWNER
;;; is the ENTER-INSTRUCTION that is the owner of the static lexical
;;; location to be eliminated.
;;;
;;; We proceed as follows: We insert new instructions after each
;;; ENTER-INSTRUCTION to write the cell into the dynamic lexical
;;; variable supplied for that purpose.  After OWNER, we insert a
;;; CREATE-CELL instruction.  For every other ENTER-INSTRUCTION in
;;; CELL-LOCATIONS we add a FETCH instruction after it, and we import
;;; the cell location of the parent function into the corresponding
;;; ENCLOSE-INSTRUCTION.
(defun ensure-cell-available (function-tree cell-locations owner)
  ;; Start by creating a CREATE-CELL-INSTRUCTION after the owner of
  ;; the static lexical location to be eliminated.
  (let ((cleavir-ir:*policy* (cleavir-ir:policy owner)))
    (cleavir-ir:insert-instruction-after
     (cleavir-ir:make-create-cell-instruction (cdr (assoc owner cell-locations)))
     owner))
  ;; Next, for each entry in CELL-LOCATIONS other than OWNER, transmit
  ;; the cell from the corresponding ENCLOSE-INSTRUCTION to the
  ;; ENTER-INSTRUCTION of that entry.
  (loop with tree-nodes = (tree-nodes function-tree)
	for (enter . cell-location) in cell-locations
	unless (eq enter owner)
	  do (let* ((node (gethash enter tree-nodes))
		    (enclose (enclose-instruction node))
		    (parent-enter (parent-enter function-tree enter))
		    (import (cdr (assoc parent-enter cell-locations))))
	       (transmit-cell enclose import enter cell-location))))

;;; Given a list of ENTER-INSTRUCTIONs representing the functions that
;;; read or write some particular static lexical location to
;;; eliminate, return a another list of ENTER-INSTRUCTIONs which is
;;; like the one passed as an argument, except that ENTER-INSTRUCTIONs
;;; representing intermediate functions have been added.  An
;;; intermediate function is one that neither reads nor writes the
;;; location, but that has an ancestor and a descendant that both do.
;;; FUNCTION-TREE is a function tree representing all the functions of
;;; the program.  OWNER is the ENTER-INSTRUCTION of the outermost
;;; function in the program that reads or writes the location.
(defun add-intermediate-functions (enter-instructions function-tree owner)
  (let ((result enter-instructions))
    (loop for enter-instruction in enter-instructions
	  do (loop with enter = enter-instruction
		   until (eq enter owner)
		   do (pushnew enter result :test #'eq)
		      (setf enter (parent-enter function-tree enter))))
    result))

;;; Given a single static lexical location SLOC, a dynamic lexical
;;; location CLOC holding the cell that replaces SLOC, and a single
;;; instruction INSTRUCTION that reads that location, create a new
;;; temporary dynamic lexical location D to hold the value of CLOC,
;;; insert a new READ-CELL instruction before INSTRUCTION that puts
;;; the contents of CLOC in SLOC, and replace all occurrences of SLOC
;;; in the inputs of I by D.
(defun replace-inputs (sloc cloc instruction)
  (let ((d (cleavir-ir:new-temporary))
	(cleavir-ir:*policy* (cleavir-ir:policy instruction)))
    (cleavir-ir:insert-instruction-before
     (cleavir-ir:make-read-cell-instruction cloc d)
     instruction)
    (setf (cleavir-ir:inputs instruction)
	  (substitute d sloc (cleavir-ir:inputs instruction)))))

;;; Given a single static lexical location SLOC, a dynamic lexical
;;; location CLOC holding the cell that replaces SLOC, and a single
;;; instruction INSTRUCTION that writes that location, create a new
;;; temporary dynamic lexical location D to hold the value of CLOC,
;;; replace all occurrences of SLOC in the outputs of I by D, and
;;; insert a new WRITE-CELL instruction after INSTRUCTION that puts
;;; the value of D in CLOC.
(defun replace-outputs (sloc cloc instruction)
  (let ((d (cleavir-ir:new-temporary))
	(cleavir-ir:*policy* (cleavir-ir:policy instruction)))
    (setf (cleavir-ir:outputs instruction)
	  (substitute d sloc (cleavir-ir:outputs instruction)))
    (cleavir-ir:insert-instruction-after
     (cleavir-ir:make-write-cell-instruction cloc d)
     instruction)))

;;; Given a single static lexical location LOCATION, eliminate it by
;;; replacing all accesses to it by accesses to a corresponding CELL.
;;; FUNCTION-TREE is the function tree of the entire program.
;;; INSTRUCTION-OWNERS is a hash table mapping an instruction to its
;;; owner.  OWNER is the owner of LOCATION.
(defun process-location
    (location function-tree instruction-owners owner)
  (let* (;; Determine all the functions (represented by
	 ;; ENTER-INSTRUCTIONs) that use (read or write) the location.
	 (users (functions-using-location location instruction-owners))
	 ;; To that set, add the intermediate functions.
	 (enters (add-intermediate-functions users function-tree owner))
	 ;; Compute a dictionary that associates each
	 ;; ENTER-INSTRUCTION with a cell location.
	 (cell-locations (allocate-cell-locations enters)))
    (loop for instruction in (cleavir-ir:using-instructions location)
	  for instruction-owner = (gethash instruction instruction-owners)
	  for cell-location = (cdr (assoc instruction-owner cell-locations))
	  do (replace-inputs location cell-location instruction))
    (loop for instruction in (cleavir-ir:defining-instructions location)
	  for instruction-owner = (gethash instruction instruction-owners)
	  for cell-location = (cdr (assoc instruction-owner cell-locations))
	  do (replace-outputs location cell-location instruction))
    ;; We do this step last, so that we are sure that the CREATE-CELL
    ;; and FETCH instructions are inserted immediately after the ENTER
    ;; instruction.
    (ensure-cell-available function-tree cell-locations owner)))

(defun process-captured-variables (initial-instruction)
  ;; Make sure everything is up to date.
  (cleavir-ir:reinitialize-data initial-instruction)
  (let* ((instruction-owners (compute-instruction-owners initial-instruction))
	 (location-owners (compute-location-owners initial-instruction))
	 (function-tree (build-function-tree initial-instruction))
	 (static-locations
	   (segregate-lexicals initial-instruction location-owners)))
    (loop for static-location in static-locations
	  for owner = (gethash static-location location-owners)
	  do (process-location static-location
			       function-tree
			       instruction-owners
			       owner))))

(defun segregate-only (initial-instruction)
  ;; Make sure everything is up to date.
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((location-owners (compute-location-owners initial-instruction)))
    (segregate-lexicals initial-instruction location-owners)))
