(in-package #:cleavir-hir-transformations)

;;; In some cases, the dynamic environment of a nonlocal unwind
;;; can be understood statically. For example, in
;;; (block nil (mapcar (lambda (x) (return x)) list)),
;;; the lambda will be called in the same dynamic environment as
;;; the mapcar is called in, assuming a reasonable implementation
;;; of mapcar, and so the return/unwind dynamic environment can
;;; be understood by the compiler to have no unwind-protects or
;;; etc. to worry about. This can power optimizations.

;;; For example, the unwind need not run a dynamic search to ensure
;;; the exit point is still valid, or to find unwind-protect cleanups
;;; to execute. On a low level, they can be more or less a frame
;;; pointer reset followed by a jump.

;;; We call such unwinds "simple".

;;; If all unwinds to a given catch are simple, additional
;;; optimizations are possible. The catch instruction does not need
;;; to augment the dynamic environment, as it's only unwound to by
;;; unwinds that don't use the dynamic environment anyway.
;;; (Though indicating its existence to debugging tools may be prudent.)

;;; This transformation marks unwinds that can be seen to be
;;; called in the dynamic environment output by their destination.
;;; It also marks catch instructions, if every unwind to them
;;; is marked.

;;; This pass can and probably should be run after eliminate-catches
;;; and inlining have been completed.

;;; TODO: Much extension is possible:

;;; In cases where the unwind's dynamic environment has been
;;; altered by the function that's unwinding, this pass does not mark
;;; the unwind as simple. Those unwinds could be made simple by
;;; preceding them with an appropriate local-unwind.
;;; This insertion can and probably should be done back in AST-to-HIR,
;;; not here, since it's valid regardless.

;;; Cases where the call's dynamic environment has been augmented
;;; from the catch may also be possible to handle, but this might
;;; require some changes in the HIR representation of nonlocal exits.

;;; Nesting of dynamically safe functions is not understood by this
;;; pass, e.g. the unwind and catch in
;;; (block nil (mapc (lambda (x) (mapc (lambda (y) (return y)) x)) z))
;;; are considered non-simple. This may not happen much in real code
;;; anyway, though, so it may not be worth the effort?

;;; find the single defining instruction for a dynenv location
(defun dynenv-definer (location)
  (let ((definers (cleavir-ir:defining-instructions location)))
    ;; FIXME: Actual condition? If this assertion fails it's a bug.
    (assert (= (length definers) 1))
    (first definers)))

;;; skip assignment instructions that alias the dynenv
(defun original-definer (location)
  (loop for definer = (dynenv-definer location)
          then (dynenv-definer (first (cleavir-ir:inputs definer)))
        while (typep definer 'cleavir-ir:assignment-instruction)
        finally (return definer)))

;;; A using-instruction of a function location is amenable to the
;;; simplification process if
;;; a) it's irrelevant (e.g. initialization of it as a closure), or
;;; b) it's a call to the function, or
;;; c) it's a call to a function that doesn't augment the dynamic
;;;    environment for calls to the function.
(defun simplifiable-user-p (user fn)
  (typecase user
    (cleavir-ir:initialize-closure-instruction
     ;; Implies that it's the first input.
     (not (member fn (rest (cleavir-ir:inputs user)))))
    (cleavir-ir:abstract-call-instruction
     (or (cleavir-attributes:has-boolean-attribute-p
          (cleavir-ir:attributes user)
          :dyn-call)
         (and (eq (cleavir-ir:callee user) fn)
              ;; Make sure it's not also an argument.
              (= 1 (count fn (cleavir-ir:inputs user) :test #'eq)))))
    (t nil)))

;;; First make sure the unwind takes place in a function's initial
;;; dynamic environment.
;;; Then check calls to that function to see if they're amenable,
;;; both in the above sense, and being in the corresponding catch's
;;; dynamic environment.
(defun simplifiable-unwind-p (unwind dag)
  (let ((definer (original-definer (cleavir-ir:dynamic-environment unwind)))
        (catch (cleavir-ir:destination unwind)))
    ;; FIXME: Stupid sanity check, delete
    (check-type catch cleavir-ir:catch-instruction)
    (unless (typep definer 'cleavir-ir:enter-instruction)
      (return-from simplifiable-unwind-p nil))
    (loop for node
            in (gethash definer (dag-nodes dag))
          for enclose = (enclose-instruction node)
          do (loop with fn = (first (cleavir-ir:outputs enclose))
                   for user in (cleavir-ir:using-instructions fn)
                   for udefiner = (original-definer
                                   (cleavir-ir:dynamic-environment user))
                   unless (and (eq udefiner catch)
                               (simplifiable-user-p user fn))
                     do (return-from simplifiable-unwind-p nil)))))

(defun mark-simple-unwinds (initial-instruction)
  (let* ((dag (build-function-dag initial-instruction))
         ;; An alist from catches to lists of unwinds going to them.
         (catch-map nil))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:unwind-instruction)
         (let ((catch (cleavir-ir:destination instruction)))
           ;; Put this in the catch map.
           (let ((p (assoc catch catch-map)))
             (if p
                 (push instruction (cdr p))
                 (push (list catch instruction) catch-map)))
           ;; See if this unwind is simplifiable, and if so mark it.
           (when (simplifiable-unwind-p instruction dag)
             (setf (cleavir-ir:simple-p instruction) t)))))
     initial-instruction)
    ;; All simple unwinds are now marked.
    ;; Now mark any catches that have all-simple unwinds.
    (loop for (catch . unwinds) in catch-map
          when (every #'cleavir-ir:simple-p unwinds)
            do (setf (cleavir-ir:simple-p catch) t))))
