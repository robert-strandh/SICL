(in-package #:cleavir-partial-inlining)

;;; Copy propagate an assignment and return the old defining
;;; instructions of the input for the sake of incremental analysis.
(defun copy-propagate-assignment (assignment)
  ;; Can only deal with trivial assignments
  (let ((inputs (cleavir-ir:inputs assignment))
        (outputs (cleavir-ir:outputs assignment)))
    (when (and (null (rest inputs))
               (null (rest outputs)))
      (let ((input (first inputs))
            (output (first outputs)))
        (prog1 (cleavir-ir:using-instructions output)
          (cleavir-ir:replace-datum input output)
          (cleavir-ir:delete-instruction assignment))))))

;;; A lightweight copy propagation utility to get rid of pesky
;;; assignments blocking optimizations.
;;; Copy propagate forward one datum.
(defun copy-propagate-1 (datum)
  (let ((worklist (cleavir-ir:using-instructions datum)))
    (loop (unless worklist
            (return))
          (let ((use (pop worklist)))
            (typecase use
              (binding-assignment-instruction
               ;; Don't do anything with binding assignments. We don't want to disappear them.
               )
              (cleavir-ir:assignment-instruction
               (dolist (use (copy-propagate-assignment use))
                 (push use worklist))))))))

;;; Analyze the flow graph to see if there are local functions that
;;; can be usefully integrated (contified). And if so, then interpolate.
(defun interpolable-function-analyze (initial-instruction)
  ;; Find every enclose instruction and analyze those.
  (let ((*instruction-ownerships* (cleavir-hir-transformations:compute-instruction-owners initial-instruction))
        (*location-ownerships* (cleavir-hir-transformations:compute-location-owners initial-instruction))
        (*binding-assignments* '()))
    (mapc #'interpolable-function-analyze-1
          (cleavir-ir:instructions-of-type initial-instruction
                                           'cleavir-ir:enclose-instruction))
    (convert-binding-instructions *binding-assignments*)))

;; Do a simple interpolable analysis on one enclose, and act if
;; possible.
(defun interpolable-function-analyze-1 (enclose)
  (let ((code (cleavir-ir:code enclose))
        (fun (first (cleavir-ir:outputs enclose))))
    ;; FIXME: Bail if the function arg processing is too hairy somehow.
    (unless (all-parameters-required-p code)
      (return-from interpolable-function-analyze-1))
    (copy-propagate-1 fun)
    ;; Copy propagate so silly assignments don't trip us up.
    (let ((users (cleavir-ir:using-instructions fun)))
      (multiple-value-bind (return-point common-output common-dynenv)
          (common-return-cont enclose users fun)
        (case return-point
          ;; This function should get deleted somehow.
          (:uncalled)
          ;; Can't do anything with a non-constant continuation.
          (:unknown)
          ;; If there is a common return point, integrate the local
          ;; function into the call graph and rewire the calls,
          ;; replacing every call's output with the return values of the
          ;; function.
          (otherwise
           (unless (every (lambda (user)
                            ;; We want error checking.
                            (and (call-valid-p code user)
                                 ;; Explicit declaration should
                                 ;; inhibit interpolation.
                                 ;; Fix this when we start
                                 ;; interpolating MVC.
                                 (not (eq (inline user) 'notinline))))
                          users)
             (return-from interpolable-function-analyze-1))
           (interpolate-function return-point common-output common-dynenv code)
           (dolist (call users)
             (assert (typep call 'cleavir-ir:funcall-instruction))
             (rewire-user-into-body call code))
           ;; Now clean up the enclose and other stuff hanging off of
           ;; the enter instruction.
           (cleavir-ir:delete-instruction code)
           (cleavir-ir:delete-instruction enclose)))))))

;;; A return point is either a logical continuation (i.e. successor
;;; instruction or function to be returned into), :unknown (if the
;;; return point is conceptually non-constant), or :uncalled.

(defun effectless-local-unwind (instruction)
  ;; FIXME copied from clasp.
  (let* ((succ (first (cleavir-ir:successors instruction)))
         (outer-dynenv (cleavir-ir:dynamic-environment succ)))
    (loop for dynenv = (cleavir-ir:dynamic-environment instruction)
          ;; Note that this is the definer from the previous loop iteration.
            then (cleavir-ir:dynamic-environment definer)
          for definer = (first (cleavir-ir:defining-instructions dynenv))
          until (eq dynenv outer-dynenv)
          always (typep definer '(or cleavir-ir:catch-instruction cleavir-ir:assignment-instruction)))))

(defun successor-skipping-nops (instruction)
  (do ((successor (first (cleavir-ir:successors instruction)) (first (cleavir-ir:successors successor))))
      ((not (or (typep successor 'cleavir-ir:nop-instruction)
                ;; Skip local unwinds that have no effect.
                (and (typep successor 'cleavir-ir:local-unwind-instruction)
                     (effectless-local-unwind successor))))
       successor)))

;;; This is a basically the A_call analysis in Fluet & Weeks.  When we
;;; are tail recursing, simply return the enclose as a sentinel.  It
;;; would be nice to use the A_dom analysis, as that is provably
;;; optimal, and generalizes to all tail calls, not just tail
;;; recursive ones.
(defun logical-continuation (enclose user)
  (assert (null (rest (cleavir-ir:successors user))))
  ;; When checking for self tail call recursion, skip over nops or
  ;; local unwinds that prevent tail call recognition. But do not do
  ;; so in the non-tail call case because we need the return point to
  ;; happen before local unwinds.
  (let ((skip-succ (successor-skipping-nops user)))
    (if (and (typep skip-succ 'cleavir-ir:return-instruction)
             (eq (instruction-owner skip-succ)
                 (cleavir-ir:code enclose)))
        enclose
        (first (cleavir-ir:successors user)))))

;; Think about which instruction each logical user of the enclose
;; ultimately must return to. Make sure the dynamic environments of
;; the users and the outputs are also constant.
(defun common-return-cont (enclose users fun)
  (let ((return-point :uncalled)
        common-output
        common-dynenv)
    (dolist (user users)
      ;; Make sure all users are actually call instructions that only
      ;; use FUN in call position.
      (unless (and (typep user 'cleavir-ir:funcall-instruction)
                   (eq (first (cleavir-ir:inputs user)) fun)
                   (not (member fun (rest (cleavir-ir:inputs user)))))
        (setf return-point :unknown)
        (return))
      (let ((cont (logical-continuation enclose user))
            (dynenv (cleavir-ir:dynamic-environment user))
            (output (first (cleavir-ir:outputs user))))
        (copy-propagate-1 output)
        (cond ((eq return-point :uncalled)
               (unless (eq enclose cont)
                 (setf common-dynenv dynenv)
                 (setf return-point cont)
                 (setf common-output output)))
              ((or (eq cont enclose)
                   (and (eq return-point cont)
                        (eq dynenv common-dynenv)
                        (eq output common-output))))
              (t
               (setf return-point :unknown)
               (return)))))
    (values return-point common-output common-dynenv)))

;; Rewire the call's body into the given ENTER instruction.
(defun rewire-user-into-body (call enter)
  (let ((cleavir-ir:*origin* (cleavir-ir:origin call))
        (cleavir-ir:*policy* (cleavir-ir:policy call))
        (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment call)))
    (loop for location in (cleavir-ir:parameters enter)
          for arg in (rest (cleavir-ir:inputs call))
          for assign = (make-instance 'binding-assignment-instruction
                                      :inputs (list arg)
                                      :outputs (list location))
          do (push assign *binding-assignments*)
             (cleavir-ir:insert-instruction-before assign call)))
  ;; Replace the call with a regular control arc into the function.
  (cleavir-ir:bypass-instruction (first (cleavir-ir:successors enter)) call))

;;; Cut and paste a function to inline - i.e. don't copy much of
;;; anything, which is nice, but means the original is destroyed. The
;;; function will get pasted before the given return-point in the
;;; given dynamic environment.
(defun interpolate-function (return-point common-output new-dynenv enter)
  (let (;; We need to alter these. We find them before doing any alteration-
        ;; interleaving modification and finds results in unfortunate effects.
        (returns '())
        (unwinds '())
        (target-enter (instruction-owner return-point))
        (old-dynenv (cleavir-ir:dynamic-environment enter)))
    ;; Update the ownerships of each local instruction and datum and
    ;; find the exit point instructions. Also update the dynamic
    ;; environments of instructions whose dynamic environment is the
    ;; same as the one established by ENTER.
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (setf (instruction-owner instruction) target-enter)
       (when (eq (cleavir-ir:dynamic-environment instruction)
                 old-dynenv)
         (setf (cleavir-ir:dynamic-environment instruction)
               new-dynenv))
       (dolist (input (cleavir-ir:inputs instruction))
         (when (eq (location-owner input) enter)
           (setf (location-owner input) target-enter)))
       (dolist (output (cleavir-ir:outputs instruction))
         (when (eq (location-owner output) enter)
           (setf (location-owner output) target-enter)))
       (when (typep instruction 'cleavir-ir:return-instruction)
         (push instruction returns))
       (when (typep instruction 'cleavir-ir:unwind-instruction)
         (push instruction unwinds)))
     enter)
    ;; Turn any unwinds in the body to the function being inlined
    ;; into direct control transfers.
    (loop for unwind in unwinds
          for destination = (cleavir-ir:destination unwind)
          ;; Recapitulates local-catch-p in inline-one-instruction.lisp, a bit.
          when (eq (instruction-owner destination) target-enter)
            ;; it's local: replace it. (If not local, there is nothing to do.)
            ;; (Similar to the unwind-instruction method on inline-one-instruction)
            do (let* ((target (nth (cleavir-ir:unwind-index unwind)
                                   (cleavir-ir:successors destination)))
                      (new (let ((cleavir-ir:*origin* (cleavir-ir:origin unwind))
                                 (cleavir-ir:*policy* (cleavir-ir:policy unwind))
                                 (cleavir-ir:*dynamic-environment*
                                   (cleavir-ir:dynamic-environment unwind)))
                             (cleavir-ir:make-local-unwind-instruction target))))
                 (cleavir-ir:bypass-instruction new unwind)))
    ;; Fix up the return values, and replace return instructions with NOPs that go to after the call.
    (loop with caller-values = common-output
          for return in returns
          for values = (first (cleavir-ir:inputs return))
          do (cleavir-ir:replace-datum caller-values values)
             (let ((nop (let ((cleavir-ir:*origin* (cleavir-ir:origin return))
                              (cleavir-ir:*policy* (cleavir-ir:policy return))
                              (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment return)))
                          (cleavir-ir:make-nop-instruction nil))))
               (cleavir-ir:insert-instruction-before nop return-point)
               (cleavir-ir:bypass-instruction nop return))))
  ;; Done!
  (values))
