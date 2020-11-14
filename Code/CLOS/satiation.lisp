(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satiate a generic function.
;;;
;;; We assume we have a standard generic function.  We also assume
;;; that the specializers of each method are classes as opposed to EQL
;;; specializers.  Finally, we assume that the generic function uses
;;; the standard method combination.
;;;
;;; For each primary method of the generic function, compute all the
;;; combinations of argument classes that would make it applicable.
;;; Compute a unique list of such combinations of classes.  For each
;;; combination, do the same thing as is done if the generic function
;;; were actually called with those classes, i.e., compute the
;;; applicable methods and the effective method and load up the call
;;; history.  Finally, compute and set the discriminating function.

;;; Return all descendants of a class, including the class itself.
(defun all-descendants (class)
  (let ((subclasses (class-direct-subclasses class)))
    (remove-duplicates (cons class
                             (reduce #'append
                                     (mapcar #'all-descendants subclasses))))))

(defun cartesian-product (sets)
  (if (null (cdr sets))
      (mapcar #'list (car sets))
      (loop for element in (car sets)
            append (mapcar (lambda (set)
                             (cons element set))
                           (cartesian-product (cdr sets))))))

(defun at-least-one-primary-method-p (methods)
  (loop for method in methods
        for qualifiers = (method-qualifiers method)
        when (null qualifiers)
          return t
        finally (return nil)))

;;; CLASSES-OF-METHOD is a list of specializers (which much be classes)
;;; of a single method of the generic function.
(defun add-to-call-history (generic-function classes-of-method profile)
  (let* ((sets (loop for class in classes-of-method
                     for flag in profile
                     collect (if (null flag)
                                 (list class)
                                 (all-descendants class))))
         (all-combinations (cartesian-product sets)))
    (loop for combination in all-combinations
          for methods = (compute-applicable-methods-using-classes
                         generic-function combination)
          for relevant-classes = (loop for class in combination
                                       for flag in profile
                                       when flag collect class)
          do (unless (member relevant-classes (call-history generic-function)
                             :key #'class-cache :test #'equal)
               (when (at-least-one-primary-method-p methods)
                 (add-call-cache generic-function
                                 combination
                                 relevant-classes
                                 methods))))))

(defun load-call-history (generic-function)
  (setf (call-history generic-function) '())
  (loop with profile = (specializer-profile generic-function)
        for method in (generic-function-methods generic-function)
        for specializers = (method-specializers method)
        do (add-to-call-history generic-function specializers profile)))

(defun satiate-generic-function (generic-function)
  (load-call-history generic-function)
  (compute-and-install-discriminating-function generic-function))
