(in-package #:cleavir-partial-inlining)

;;; Analyze the flow graph to see if there are local functions that
;;; can be usefully integrated (contified). And if so, then interpolate.
(defun interpolable-function-analyze (initial-instruction)
  ;; This removes catches which might keep returns around and block
  ;; contification.
  (cleavir-hir-transformations:eliminate-catches initial-instruction)
  ;; Find every enclose instruction and analyze those.
  (let ((*instruction-ownerships* (cleavir-hir-transformations:compute-instruction-owners initial-instruction))
        (*location-ownerships* (cleavir-hir-transformations:compute-location-owners initial-instruction))
        (*binding-assignments* '()))
    ;; Going in map-instructions order means top down order in the
    ;; function nesting tree, which captures more functions.
    (cleavir-ir:map-instructions
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:enclose-instruction)
         (interpolable-function-analyze-1 instruction)))
     initial-instruction)
    (convert-binding-instructions *binding-assignments*)))

;; Do a simple interpolable analysis on one enclose, and act if
;; possible.
(defun interpolable-function-analyze-1 (enclose)
  (let ((code (cleavir-ir:code enclose))
        (fun (first (cleavir-ir:outputs enclose))))
    ;; FIXME: Bail if the function arg processing is too hairy somehow.
    (when (lambda-list-too-hairy-p (cleavir-ir:lambda-list code))
      (return-from interpolable-function-analyze-1))
    ;; Copy propagate so silly assignments don't trip us up.
    (cleavir-hir-transformations:copy-propagate-1 fun)
    (let ((users (cleavir-ir:using-instructions fun)))
      (multiple-value-bind (return-point common-output common-dynenv target-owner)
          (common-return-cont enclose users fun)
        (case return-point
          ;; This function should get deleted somehow.
          (:uncalled)
          ;; Can't do anything with escaping stuff.
          (:escape)
          (otherwise
           ;; Per HIR rules we can't really interpolate any function
           ;; when it's ambiguous what its dynenv or owners should be.
           (when (and common-dynenv target-owner
                      (every (lambda (user)
                               ;; We want error checking.
                               (and (call-valid-p code user)
                                    ;; Explicit declaration should inhibit
                                    ;; interpolation. Fix this when we start
                                    ;; interpolating MVC.
                                    (not (eq (cleavir-ir:inline-declaration user) 'notinline))))
                             users))
             (let ((returns (cleavir-ir:local-instructions-of-type code 'cleavir-ir:return-instruction)))
               ;; If there is a common return point, integrate the local
               ;; function into the call graph and rewire the calls,
               ;; replacing every call's output with the return values of the
               ;; function.
               ;; If the return continuation is unknown, it can still get
               ;; contified as long as the local function never returns
               ;; normally.
               (unless (and returns (eq return-point :unknown))
                 (interpolate-function target-owner common-dynenv code)
                 (unless (eq return-point :unknown)
                   (rewire-return-cont returns return-point common-output))
                 (dolist (call users)
                   (assert (typep call 'cleavir-ir:funcall-instruction))
                   (rewire-user-into-body call code))
                 ;; Attempt to convert the values locations into a
                 ;; lexical locations with ordinary assignments if possible.
                 (when returns
                   (cleavir-hir-transformations:maybe-convert-values-location common-output))
                 ;; Now clean up the enclose and other stuff hanging off of
                 ;; the enter instruction.
                 (cleavir-ir:delete-instruction code)
                 (cleavir-ir:delete-instruction enclose))))))))))

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
        common-dynenv
        target-owner)
    (dolist (user users)
      ;; Make sure all users are actually call instructions that only
      ;; use FUN in call position.
      (unless (and (typep user 'cleavir-ir:funcall-instruction)
                   (eq (first (cleavir-ir:inputs user)) fun)
                   (not (member fun (rest (cleavir-ir:inputs user)))))
        (setf return-point :escape)
        (return))
      (let ((cont (logical-continuation enclose user))
            (dynenv (cleavir-ir:dynamic-environment user))
            (output (first (cleavir-ir:outputs user)))
            (owner (instruction-owner user)))
        (cleavir-hir-transformations:copy-propagate-1 output)
        ;; Check which properties every user shares.
        (cond ((eq return-point :uncalled)
               (unless (eq enclose cont)
                 (setf common-dynenv dynenv)
                 (setf return-point cont)
                 (setf common-output output)
                 (setf target-owner owner)))
              ((eq cont enclose))
              (t
               (unless (eq return-point cont)
                 (setq return-point :unknown))
               (unless (eq dynenv common-dynenv)
                 (setq dynenv nil))
               (unless (eq output common-output)
                 (setq output nil))
               (unless (eq owner target-owner)
                 (setq target-owner nil))))))
    (values return-point common-output common-dynenv target-owner)))

;; Rewire the call's body into the given ENTER instruction.
(defun rewire-user-into-body (call enter)
  (let ((cleavir-ir:*origin* (cleavir-ir:origin call))
        (cleavir-ir:*policy* (cleavir-ir:policy call))
        (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment call))
        (state :required)
        (args (rest (cleavir-ir:inputs call)))
        (position 0))
    ;; KLUDGE: Another weird parser.
    (dolist (item (cleavir-ir:lambda-list enter))
      (if (eq item '&optional)
          (setq state :optional)
          (let* ((arg (nth position args))
                 (assign (make-instance
                          'binding-assignment-instruction
                          :inputs (list (or arg (cleavir-ir:make-load-time-value-input nil t)))
                          :outputs (list (ecase state
                                           (:required item)
                                           (:optional (first item)))))))
            (push assign *binding-assignments*)
            (cleavir-ir:insert-instruction-before assign call)
            (when (eq state :optional)
              (change-class (second item)
                            'cleavir-ir:load-time-value-input
                            :form (if arg '(quote t) nil)
                            :read-only-p t))
            (incf position)))))
  ;; Replace the call with a regular control arc into the function.
  (cleavir-ir:bypass-instruction (first (cleavir-ir:successors enter)) call))


;; Fix up the return values, and replace return instructions with NOPs
;; that go to after the call.
(defun rewire-return-cont (returns return-point common-output)
  (loop for return in returns
        for values = (first (cleavir-ir:inputs return))
        do (cleavir-ir:replace-datum common-output values)
           (let ((nop (let ((cleavir-ir:*origin* (cleavir-ir:origin return))
                            (cleavir-ir:*policy* (cleavir-ir:policy return))
                            (cleavir-ir:*dynamic-environment* (cleavir-ir:dynamic-environment return)))
                        (cleavir-ir:make-nop-instruction nil))))
             (cleavir-ir:insert-instruction-before nop return-point)
             (cleavir-ir:bypass-instruction nop return))))

;;; Cut and paste a function to inline - i.e. don't copy much of
;;; anything, which is nice, but means the original is destroyed. The
;;; function will get pasted before the given return-point in the
;;; given dynamic environment.
(defun interpolate-function (target-enter new-dynenv enter)
  (let (;; We need to alter these. We find them before doing any alteration-
        ;; interleaving modification and finds results in unfortunate effects.
        (unwinds '())
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
       (when (typep instruction 'cleavir-ir:unwind-instruction)
         (push instruction unwinds)))
     enter)
    (cleavir-ir:replace-datum new-dynenv old-dynenv)
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
                 (cleavir-ir:bypass-instruction new unwind))))
  ;; Done!
  (values))
