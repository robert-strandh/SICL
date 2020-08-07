;;;; Do value numbering, and constrain properties of value numbers
;;;; with type constraints.  Everything is basic block granularity for
;;;; speed and space efficiency. This works because our instructions
;;;; which provide type information (TYPEW and TYPEQ) form basic block
;;;; boundaries.

;;;; The "value numbering" prepass analysis used here is designed to
;;;; make the actual type analysis sparse, in the sense that the type
;;;; analysis only needs to annotate value "numbers" instead of
;;;; number/congruency class + control point tuples, which is a huge
;;;; efficiency win, without loss of precision.

;;;; "Value numbering" is really a form of alias analysis on HIR
;;;; lexical locations done via abstract interpretation over the graph
;;;; where the abstract domain is value numbers at control points. For
;;;; efficiency reasons, we also have a global table which has
;;;; abstract information that holds for every block, coming from
;;;; immutable data.

;;;; Tyoe analysis is then just a straightforward conservative
;;;; interpretation where value numbers are matched to types at every
;;;; control point. Impossible branches are then marked for deletion.

;;;; TODO actually use the executable flag to reanalyze merge points
;;;; for more precise type constraints. Requires better worklist
;;;; management.

;;;; TODO make value numbering actually aware of
;;;; EQ-INSTRUCTION. Probably requires something like a branch effect.

(in-package #:cleavir-hir-transformations)

(defvar *debug-type-infer* nil)

(defclass augmented-basic-block (cleavir-basic-blocks:basic-block)
  (;; Will this block actually get executed?
   (%executablep :accessor executablep :initarg :executablep)
   ;; Congruence classes of eq data represented by a value numbering
   ;; data structure.
   (%in-eq-data :accessor in-eq-data :initarg :in-eq-data)
   (%out-eq-data :accessor out-eq-data :initarg :out-eq-data)
   ;; For all incoming constraints.
   (%in-constraints :accessor in-constraints :initarg :in-constraints)
   ;; From out constraints coming from stuff like THE.
   ;; Right now basically always a copy of IN but will soon change.
   (%out-constraints :accessor out-constraints :initarg :out-constraints)
   ;; Used for asserting complementary constraints.
   (%branch-constraint :accessor branch-constraint :initarg :branch-constraint :initform nil)))

;; Need a fresh object for the value numbering table to enable sparse
;; analysis.
(defclass value-number () ())
;; Why do we need a phi value number? It is required for us to
;; fixpoint in DAGs like A -> B; B -> B; B -> C; C -> B. Essentially
;; normal value-numbers transition to phi-value-numbers, so called
;; since they resemble the contexts in which you see a phi, which then
;; do not change on additional iterations of the algorithm, enabling
;; easy fixpoint detection.
(defclass phi-value-number (value-number) ())

(defclass value-number-table ()
  ((%table :accessor table :initarg :table)))

;;; Immutable data need not be annotated in block local tables. They
;;; can be annotated in a global table instead.
(defvar *global-value-numbers*)

(defun immutable-p (datum)
  (null (rest (cleavir-ir:defining-instructions datum))))

;;; Either get the number from the table or use the datum itself if not.
(defun immutable-value-number (datum)
  (assert (immutable-p datum))
  (or (gethash datum *global-value-numbers*)
      (assert (not (typep (first (cleavir-ir:defining-instructions datum))
                          'cleavir-ir:assignment-instruction)))
      datum))

(defun (setf immutable-value-number) (new-number datum)
  (assert (immutable-p datum))
  (assert (typep (first (cleavir-ir:defining-instructions datum))
                 'cleavir-ir:assignment-instruction))
  (setf (gethash datum *global-value-numbers*) new-number))

(defun make-value-number-table (&key (size 0))
  (make-instance 'value-number-table
                 :table (make-hash-table :test #'eq :size size)))

(defun make-value-number () (make-instance 'value-number))
(defun make-phi-value-number () (make-instance 'phi-value-number))

(defun initialize-block-in-eq-data (block predecessors)
  (cond ((null predecessors)
         (assert (typep (cleavir-basic-blocks:first-instruction block)
                        'cleavir-ir:enter-instruction))
         (setf (in-eq-data block) (make-value-number-table)))
        ((null (rest predecessors))
         ;; Can share the same value number table in this case because
         ;; the in tables should never get modified during the block
         ;; local phase.
         (setf (in-eq-data block) (out-eq-data (first predecessors))))
        (t
         ;; Basically intersect over all predecessor value tables.
         (let* ((pred-tables (mapcar #'out-eq-data predecessors))
                (initialized-tables
                  (remove-if (lambda (table)
                               (zerop (hash-table-count table)))
                             pred-tables :key #'table))
                (value-table (make-value-number-table
                              :size
                              (loop for table in initialized-tables
                                    minimize (hash-table-count (table table))))))
           (when initialized-tables
             (maphash (lambda (datum number)
                        (let ((all-same t)
                              (overdefined nil))
                          (dolist (table (rest initialized-tables))
                            (let ((other-number (gethash datum (table table))))
                              ;; When the other number exists and is
                              ;; different, then the value is
                              ;; overdefined.
                              (when other-number
                                (unless (eq number other-number)
                                  (setf all-same nil)
                                  (setf overdefined t)
                                  (return)))))
                          (when all-same
                            (setf (gethash datum (table value-table))
                                  number))
                          (when overdefined
                            (setf (gethash datum (table value-table))
                                  (make-value-number)))))
                      (table (first initialized-tables))))
           (setf (in-eq-data block) value-table)))))

(defun reanalyze-block-in-eq-data (block predecessors)
  (cond ((null predecessors) (error "unreachable"))
        ((null (rest predecessors))
         (setf (in-eq-data block) (out-eq-data (first predecessors))))
        (t
         ;; Basically intersect over all predecessor value tables.
         (let* ((pred-tables (mapcar #'out-eq-data predecessors))
                (value-table (in-eq-data block)))
           (maphash (lambda (datum number)
                      (let ((all-same t)
                            (overdefined nil))
                        (dolist (table (rest pred-tables))
                          (let ((other-number (gethash datum (table table))))
                            (when other-number
                              (unless (eq number other-number)
                                (setf all-same nil)
                                (setf overdefined t)
                                (return)))))
                        (when all-same
                          (unless (eq (gethash datum (table value-table))
                                      number)
                            (setf (gethash datum (table value-table))
                                  number)))
                        (when overdefined
                          ;; Only allocate a new phi value number
                          ;; during reanalysis if it has not been
                          ;; reanalyzed as such yet to prevent
                          ;; infinite loops.

                          ;; Why does this work?

                          ;; In the initial reanalysis pass, all
                          ;; predecessor value tables are already
                          ;; initialized. Therefore, we are
                          ;; guaranteed to get a fresh congruency
                          ;; class distinct from all the
                          ;; predecessors. As flow propagation
                          ;; goes around and reinitializes all the
                          ;; predecessors, the true congruency
                          ;; class will stabilize the second time
                          ;; around.
                          (unless (typep (gethash datum (table value-table)) 'phi-value-number)
                            (setf (gethash datum (table value-table)) (make-phi-value-number))))))
                    (table (first pred-tables)))
           (setf (in-eq-data block) value-table)))))

(defun copy-hash-table (hash-table)
  (let ((new-hash-table (make-hash-table
                         :test #'eq
                         :size (hash-table-size hash-table))))
    (maphash (lambda (k v)
               (setf (gethash k new-hash-table) v))
             hash-table)
    new-hash-table))

;;; Transfer in data to out data.
(defun initial-block-value-transfer (block)
  (let* ((in-eq-data (in-eq-data block))
         (in-table (table in-eq-data))
         (out-table (copy-hash-table in-table)))
    ;; Copy the in hash table.
    (setf (table (out-eq-data block)) out-table)
    (cleavir-basic-blocks:map-basic-block-instructions
     (lambda (instruction)
       (typecase instruction
         (cleavir-ir:assignment-instruction
          (let* ((input (first (cleavir-ir:inputs instruction)))
                 (output (first (cleavir-ir:outputs instruction)))
                 (input-number (if (immutable-p input)
                                   (immutable-value-number input)
                                   (or (gethash input out-table)
                                       (gethash input in-table)
                                       (error "WHY?")))))
            (if (immutable-p output)
                (setf (immutable-value-number output) input-number)
                (setf (gethash output out-table) input-number))))
         (t
          ;; This is where having known functions would plug into
          ;; recording the actual value numbers. For now, just
          ;; invalidate whatever was in the table and assign a new
          ;; value number to this datum if it previously existed. If
          ;; it's not in the table, then leave it as
          ;; nothing. Basically treating all non-assignment
          ;; instructions as totally opaque.
          (dolist (output (cleavir-ir:outputs instruction))
            (unless (immutable-p output)
              (setf (gethash output out-table) (make-value-number)))))))
     block)))

(defun block-value-transfer-reanalyze (block)
  (let* ((in-eq-data (in-eq-data block))
         (in-table (table in-eq-data))
         (out-eq-data (out-eq-data block))
         (out-table (table out-eq-data))
         ;; Need a temporary table to accumulate the effects of
         ;; assignments without yet committing so we can check if
         ;; state has changed or not.
         (temp-table (make-hash-table :test #'eq))
         (changed nil))
    (cleavir-basic-blocks:map-basic-block-instructions
     (lambda (instruction)
       (typecase instruction
         ;; During reanalysis, push the effects of assignments
         ;; normally.
         (cleavir-ir:assignment-instruction
          (let* ((input (first (cleavir-ir:inputs instruction)))
                 (output (first (cleavir-ir:outputs instruction)))
                 (input-number (if (immutable-p input)
                                   (immutable-value-number input)
                                   (or (gethash input temp-table)
                                       (gethash input in-table)
                                       (gethash input out-table)
                                       (error "WHY?A")))))
            (if (immutable-p output)
                ;; Reanalyze if the global table has changed.
                (unless (eq input-number (immutable-value-number output))
                  (setf (immutable-value-number output) input-number)
                  (dolist (use (cleavir-ir:using-instructions output))
                    (when (typep use 'cleavir-ir:assignment-instruction)
                      (push use changed))))
                (setf (gethash output temp-table) input-number))))
         (t
          ;; When hitting any other type of instruction, restore the
          ;; existing entry if it has changed, since we accumulate all
          ;; effects of numbers to the end of the block.
          (dolist (output (cleavir-ir:outputs instruction))
            (unless (immutable-p output)
              (setf (gethash output temp-table) (gethash output out-table)))))))
     block)
    ;; Commit the existing or new value numbers of existing data to
    ;; the out table, setting stuff up for reanalysis.
    (maphash (lambda (datum in-number)
               (let ((old-number (gethash datum out-table))
                     (new-number (or (gethash datum temp-table) in-number)))
                 (unless (eq old-number new-number)
                   (setf (gethash datum out-table) new-number)
                   (unless changed
                     (setf changed t)))))
             in-table)
    (maphash (lambda (datum temp-number)
               (unless (gethash datum in-table)
                 (unless (eq (gethash datum out-table) temp-number)
                   (setf (gethash datum out-table) temp-number))))
             temp-table)
    changed))

(defun value-number (start instruction-basic-blocks)
  (let* ((initial-ordering
           (cleavir-utilities:depth-first-search-reverse-postorder
            start
            #'cleavir-basic-blocks:successors))
         (reanalyze (and '() initial-ordering))
         (block-count (make-hash-table)))
    ;; Drain the initial ordering.
    (loop
      (when (null initial-ordering)
        (return))
      (let ((block (pop initial-ordering)))
        (initialize-block-in-eq-data block (cleavir-basic-blocks:predecessors block))
        (initial-block-value-transfer block)
        (dolist (succ (cleavir-basic-blocks:successors block))
          ;; Backedge.
          (unless (member succ initial-ordering)
            (push succ reanalyze)))))
    ;; Now that the initial forward flow is done, we fix up
    ;; blocks-to-be-reanalyzed and probably replace a few
    ;; value-numbers with phi-numbers.
    (loop
      (when (null reanalyze)
        (return))
      (let ((block (pop reanalyze)))
        ;; KLUDGE: we detect if it looks like a block is being
        ;; repeatedly analyzed and manually exit in such a
        ;; case. Ideally, this shouldn't happen, but it is sometimes
        ;; possible in convoluted flowgraphs where numbers can be
        ;; assigned in a periodic manner. Detect such a scenario and
        ;; warn about it.
        (when block-count
          (let ((count (gethash block block-count)))
            (if count
                (progn
                  (incf (gethash block block-count))
                  (when (> count 750)
                    (warn "value numbering: Fixpoint iterations exceeded threshold limit.")
                    (return)))
                (setf (gethash block block-count) 0))))
        (reanalyze-block-in-eq-data block (cleavir-basic-blocks:predecessors block))
        (let ((changed (block-value-transfer-reanalyze block)))
          (when changed
            (dolist (succ (cleavir-basic-blocks:successors block))
              (pushnew succ reanalyze))
            (when (consp changed)
              (dolist (inst changed)
                (pushnew (gethash inst instruction-basic-blocks) changed)))))))))

;;;; Type analysis

(defclass constraint ()
  ((%value :accessor constraint-value :initarg :value)))

(defclass type-constraint (constraint)
  ((%ctype :accessor type-constraint-ctype :initarg :ctype)))

(defclass typep-constraint (type-constraint) ())
;; Unused for now, but obviously desirable in numerical code.
(defclass <-constraint (type-constraint) ())
(defclass >-constraint (type-constraint) ())

(defmethod print-object ((obj typep-constraint) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "<constraint ~s>" (type-constraint-ctype obj))))

(defclass constraint-table ()
  ((%table :accessor table :initform (make-hash-table :test #'eq))))

(defun make-constraint-table ()
  (make-instance 'constraint-table))

(defun make-typeq-constraint (value ctype)
  (make-instance 'typep-constraint :value value :ctype ctype))

(defgeneric constrain-branch-instruction (instruction block system))
(defmethod constrain-branch-instruction (instruction block system)
  (declare (ignore instruction block system)))

(defun constrain-type-instruction (instruction block system)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         ;; Get the abstract value number at this program point.
         (input-value (if (immutable-p input)
                          (immutable-value-number input)
                          (gethash input (table (out-eq-data block)))))
         (ctype (if (typep instruction 'cleavir-ir:typeq-instruction)
                    (cleavir-ir:value-type instruction)
                    (cleavir-ir:ctype instruction)))
         (successors (cleavir-ir:successors instruction))
         (then (first successors))
         (else (second successors))
         (then-block (find then (cleavir-basic-blocks:successors block)
                           :key #'cleavir-basic-blocks:first-instruction))
         (else-block (find else (cleavir-basic-blocks:successors block)
                           :key #'cleavir-basic-blocks:first-instruction))
         ;; FIXME: replace with ensure gethash or something
         (existing (or (gethash input-value (table (in-constraints block)))
                       (setf (gethash input-value (table (in-constraints block)))
                             (make-typeq-constraint input-value (cleavir-ctype:top system)))))
         (existing-ctype (type-constraint-ctype existing)))
    (cond ((cleavir-ctype:subtypep existing-ctype ctype system)
           #+(or)
           (print "ALWAYS TRUE")
           ;; Always taken, so mark only the then successor as
           ;; non-executable.
           (setf (executablep else-block) nil))
          ;; XXX: Switch this to bottom-p when things are worked out
          ;; more.
          ((multiple-value-bind (disjoint certain)
               (cleavir-ctype:subtypep (cleavir-ctype:conjoin/2 ctype existing-ctype system)
                                       (cleavir-ctype:bottom system)
                                       system)
             (and disjoint certain))
           #+(or)
           (print "NEVER TRUE")
           ;; Never executable.

           ;; TODO make this effect the worklist as well, so that
           ;; merge points for unreachable branches are not taken
           ;; into account.
           (setf (executablep then-block) nil)))
    (setf (branch-constraint block)
          ;; Do NOT intersect here.
          (make-typeq-constraint input-value ctype))))

;;; Rethink what it means to EXECUTE.
(defmethod constrain-branch-instruction ((instruction cleavir-ir:typeq-instruction) block system)
  (constrain-type-instruction instruction block system))

(defmethod constrain-branch-instruction ((instruction cleavir-ir:typew-instruction) block system)
  (constrain-type-instruction instruction block system))

(defun union-constraint-into-table (constraint constraint-table system)
  (let* ((value (constraint-value constraint))
         (table (table constraint-table))
         (existing (gethash value table))
         (new (type-constraint-ctype constraint)))
    (setf (gethash value table)
          (make-typeq-constraint
           value
           (if existing
               (cleavir-ctype:disjoin/2 new (type-constraint-ctype existing) system)
               new)))))

;;; Assumption: choke-instruction terminates basic blocks. This way we
;;; don't need to do an instruction walk and can push information
;;; solely on the block level.
(defun compute-in-constraints (block predecessors system)
  ;; Take the out sets of the predecessors and the branch conditions
  ;; of the predecessors and merge them. Type union is our meet
  ;; operation.
  (case (length predecessors)
    (0
     (assert (typep (cleavir-basic-blocks:first-instruction block)
                    'cleavir-ir:enter-instruction))
     (setf (in-constraints block) (make-constraint-table)))
    (t
     (let ((constraint-table (make-constraint-table)))
       (dolist (predecessor predecessors)
         ;; Rely on the fact that choke-instruction will appear as the
         ;; last instruction of a basic block.
         (unless (typep (cleavir-basic-blocks:last-instruction predecessor)
                        'cleavir-ir:choke-instruction)
           (let* ((branch-constraint (branch-constraint predecessor))
                  (branch-value
                    (and branch-constraint
                         (constraint-value branch-constraint)))
                  (position
                    ;; Check the relationship of this block with respect
                    ;; to the predecessor.  Cannot just use block
                    ;; successors since instruction successors may
                    ;; differ :(.
                    (and branch-constraint
                         (position (cleavir-basic-blocks:first-instruction block)
                                   (cleavir-ir:successors
                                    (cleavir-basic-blocks:last-instruction
                                     predecessor))))))
             (when (and branch-constraint (/= position 2))
               ;; Intersect the constraint with any existing constraints
               ;; based on the branch taken in the predecessor then
               ;; union it in.
               (let* ((value (constraint-value branch-constraint))
                      (existing (gethash value (table (out-constraints predecessor))))
                      (new (ecase position
                             (0 (type-constraint-ctype branch-constraint))
                             (1 (cleavir-ctype:negate (type-constraint-ctype branch-constraint) system)))))
                 (union-constraint-into-table
                  (make-typeq-constraint
                   value
                   (if existing
                       (cleavir-ctype:conjoin/2 new (type-constraint-ctype existing) system)
                       new))
                  constraint-table
                  system)))
             ;; Now union in the rest.
             (maphash
              (lambda (value constraint)
                (unless (eq value branch-value)
                  (union-constraint-into-table constraint constraint-table system))
                ;; If there is no constraint in any of the predecessors,
                ;; that is an implicit T type, so make sure to take note of
                ;; that.
                (dolist (predecessor predecessors)
                  ;; Make sure not to take into account the effect of CHOKE.
                  (unless (typep (cleavir-basic-blocks:last-instruction predecessor)
                                 'cleavir-ir:choke-instruction)
                    (unless (gethash value (table (out-constraints predecessor)))
                      (setf (gethash value (table constraint-table))
                            (make-typeq-constraint value
                                                   (cleavir-ctype:top system)))))))
              (table (out-constraints predecessor))))))
       (setf (in-constraints block) constraint-table))))
  (in-constraints block))

(defun subconstraintp (constraint1 constraint2 system)
  ;; ASSUMPTION type constraints are the only constraint.
  (cleavir-ctype:subtypep (type-constraint-ctype constraint1)
                          (type-constraint-ctype constraint2)
                          system))

;;; The block local portion is in charge of killing or augmenting the
;;; constraints in a block.
;;; The global portion merges information from the blocks, especially
;;; at merge points.
(defun analyze-types (start system)
  ;; Type inference is a forward data-flow problem.
  (let ((worklist (cleavir-utilities:depth-first-search-reverse-postorder
                   start
                   #'cleavir-basic-blocks:successors)))
    (loop
      (when (null worklist)
        (return))
      (let* ((block (pop worklist))
             (last (cleavir-basic-blocks:last-instruction block))
             (in-constraints (compute-in-constraints block
                                                     (cleavir-basic-blocks::predecessors block)
                                                     system))
             (in-table (table in-constraints))
             (out-constraints-data (out-constraints block))
             (out-table (table out-constraints-data))
             (changed nil))
        ;; We have in constraints, so make the new out constraints
        ;; based on the in constraints and then add the final branch
        ;; constraint.

        ;; For now, OUT = IN. The only thing is that we check whether
        ;; we have reached a fixpoint for this block.
        (maphash (lambda (value in-constraint)
                   (let* ((out-constraint (gethash value out-table)))
                     (if out-constraint
                         ;; Test if the IN constraint is narrower than
                         ;; the OUT constraint. If not, then don't
                         ;; mark changed.
                         (setf changed (not (subconstraintp out-constraint in-constraint system)))
                         (progn
                           (setf (gethash value out-table) in-constraint)
                           (setf changed t)))))
                 in-table)
        (when changed
          (dolist (succ (cleavir-basic-blocks:successors block))
            ;; Use pushnew so we preserve our reverse post order initial walk.
            (pushnew succ worklist)))
        ;; Don't need to check for changed here i think because the
        ;; general maphash above does... XXX
        (when (rest (cleavir-ir:successors last))
          (constrain-branch-instruction last block system))))))

;;; The reason why we track executablep on the blocks is that
;;; unreachable blocks will actually make the assertion at merge
;;; points better, if one of the predecessor blocks is determined not
;;; executable. TODO.
(defun eliminate-redundant-typeqs (initial-instruction system)
  ;; As a prepass, copy propagate all data to make value numbering
  ;; more efficient, as things will fixpoint faster.
  (copy-propagate initial-instruction)
  (let* ((basic-blocks (cleavir-basic-blocks:basic-blocks initial-instruction))
         (instruction-basic-blocks (cleavir-basic-blocks:instruction-basic-blocks basic-blocks))
         (starting-blocks '())
         (*global-value-numbers* (make-hash-table)))
    (dolist (block basic-blocks)
      (change-class block 'augmented-basic-block
                    :executablep t
                    :out-eq-data (make-value-number-table)
                    :out-constraints (make-constraint-table))
      (when (typep (cleavir-basic-blocks:first-instruction block) 'cleavir-ir:enter-instruction)
        (push block starting-blocks)))
    (dolist (start starting-blocks)
      (value-number start instruction-basic-blocks)
      #+(or)
      (let ((list (cleavir-utilities:depth-first-search-reverse-postorder
                   start
                   #'cleavir-basic-blocks:successors)))
        (dolist (block list)
          (print block)
          (format t "~&in: ~a" (table (in-eq-data block)))
          (format t "~&out: ~a" (table (out-eq-data block))))
        (format t "~&GLOBAL: ~a" *global-value-numbers*)))
    (dolist (start starting-blocks)
      (analyze-types start system)
      #+(or)
      (let ((list (cleavir-utilities:depth-first-search-reverse-postorder
                   start
                   #'cleavir-basic-blocks:successors)))
        (dolist (block list)
          (print block)
          (cleavir-basic-blocks:map-basic-block-instructions #'print block)
          (format t "~&in: ~a" (table (in-constraints block)))
          (format t "~&out: ~a" (table (out-constraints block)))
          (format t "~&branch: ~a" (branch-constraint block))
          (format t "~&executable: ~a" (executablep block)))))
    ;; We should really fixpoint iterate both the value numbering and
    ;; type analysis passes together, in the sense that deleting
    ;; blocks/marking them not executable will actually improve value
    ;; numbering as well. However, Cleavir does not know anything
    ;; about incremental/reentrant analyses, so it is pretty hard to
    ;; make this more general. Instead of passes, Cleavir should
    ;; really develop a notion of reentrant phases that can be invoked
    ;; on localized portions of the graph at any time to solve pass
    ;; ordering issues. Such a system would need support from the IR
    ;; or some kind of pass manager system. Some other compilers like
    ;; the Python compiler used in CMU CL and SBCL do this.
    (dolist (block basic-blocks)
      (when (executablep block)
        (let ((last (cleavir-basic-blocks:last-instruction block)))
          (typecase last
            (cleavir-ir:typeq-instruction
             (let* ((succs (cleavir-ir:successors last))
                    (then (first succs))
                    (else (second succs)))
               (when (not (executablep (gethash then instruction-basic-blocks)))
                 (when *debug-type-infer*
                   (format t "~& deleting typeq: ~a" (cleavir-ir:value-type last)))
                 (cleavir-ir:bypass-instruction else last))
               (when (not (executablep (gethash else instruction-basic-blocks)))
                 (when *debug-type-infer*
                   (format t "~& deleting typeq: ~a" (cleavir-ir:value-type last)))
                 (cleavir-ir:bypass-instruction then last))))
            (cleavir-ir:typew-instruction
             (let* ((succs (cleavir-ir:successors last))
                    (then (first succs))
                    (else (second succs)))
               (when (not (executablep (gethash then instruction-basic-blocks)))
                 (when *debug-type-infer*
                   (format t "~& deleting typew: ~a" (cleavir-ir:ctype last)))
                 (cleavir-ir:bypass-instruction else last))
               (when (not (executablep (gethash else instruction-basic-blocks)))
                 (when *debug-type-infer*
                   (format t "~& deleting typew: ~a" (cleavir-ir:ctype last)))
                 (cleavir-ir:bypass-instruction then last))))))))
    (cleavir-ir:reinitialize-data initial-instruction)
    (cleavir-ir:set-predecessors initial-instruction)))
