(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun fix-variables (client e3 e4)
  ;; In phase 4, we define *STANDARD-OBJECT* in E3 to be the class
  ;; named STANDARD-OBJECT in E4.  We must now define
  ;; *STANDARD-OBJECT* to also be the class named STANDARD-OBJECT in
  ;; E4, because the Clostrophilia function DEFAULT-SUPERCLASSES
  ;; accesses the value of that variable.
  (clo:make-variable
   client e4 @clostrophilia:*standard-object*
   (clo:find-class client e4 'standard-object))
  ;; And similar for *FUNCALLABLE-STANDARD-OBJECT*.
  (clo:make-variable
   client e4 @clostrophilia:*funcallable-standard-object*
   (clo:find-class client e4 @clostrophilia:funcallable-standard-object))
  ;; The variable *CLASS-UNIQUE-NUMBER* in E3 reflects the value that
  ;; it should have in E4 when the graph is cyclic.
  (setf (clo:symbol-value
         client e4 @clostrophilia:*class-unique-number*)
        (clo:symbol-value
         client e3 @clostrophilia:*class-unique-number*))
  ;; The variable *STANDARD-METHOD-COMBINATION** in E3 reflects the
  ;; value that it should have in E4 when the graph is cyclic.
  (setf (clo:symbol-value
         client e4 @clostrophilia:*standard-method-combination*)
        (clo:symbol-value
         client e3 @clostrophilia:*standard-method-combination*)))

;;; In phase 4, we set a number of function names of the form NAME+1
;;; in E3 to refer to the analogous function NAME in E4.  In
;;; subsequent code, we detect functions that are shared between E3
;;; and E4, and we assume that the function in E4 must be changed, but
;;; that's not the case for the functions of the form NAME+1 and NAME.
;;; So we remove the NAME+1 function from E3, and we set NAME+1 in E4,
;;; to refer to NAME in E4.
(defun fix-forward-referring-functions (client e3 e4)
  (flet ((fixup (name+1 name)
           (clo:fmakunbound client e3 name+1)
           (setf (clo:fdefinition client e4 name+1)
                 (clo:fdefinition client e4 name))))
    (fixup @sicl-clos:find-class+1 @sicl-clos:find-class)
    (fixup @clostrophilia:slot-boundp-using-location+1
           @clostrophilia:slot-boundp-using-location)
    (let ((s+1 @clostrophilia:slot-value-using-location+1)
          (s @clostrophilia:slot-value-using-location))
      (fixup s+1 s)
      (fixup `(setf ,s+1) `(setf ,s)))
    (fixup @clostrophilia:slot-makunbound-using-location+1
           @clostrophilia:slot-makunbound-using-location)
    (fixup @clostrophilia:ensure-generic-function+1
           'ensure-generic-function)))

(defun find-operator-in-e3 (operator4 e3)
  (loop with table = (clostrum-basic::functions e3)
        for entry being each hash-value of table using (hash-key name)
        for cell = (clostrum-basic::cell entry)
        for operator3 = (car cell)
        when (eq operator3 operator4)
          return name
        finally (return nil)))

(defun fix-backward-referring-functions (client e3 e4)
  (loop with table = (clostrum-basic::functions e4)
        for entry being each hash-value of table using (hash-key name4)
        for cell = (clostrum-basic::cell entry)
        for operator = (car cell)
        when (or (object-is-an-impure-ersatz-object-p operator)
                 (typep operator 'common-boot-ast-interpreter::closure))
          do (let ((name3 (find-operator-in-e3 operator e3)))
               (unless (null name3)
                 (setf (clo:fdefinition client e4 name4)
                       (clo:fdefinition client e4 name3))))))

;;; Create a hash table mapping each class in E3 to the analogous
;;; class in E4.
(defun create-class-mapping (e3 e4)
  (let ((table3 (clostrum-basic::types e3))
        (table4 (clostrum-basic::types e4))
        (result (make-hash-table :test #'eq)))
    (loop for name being each hash-key of table3 using (hash-value entry3)
          for cell3 = (clostrum-basic::cell entry3)
          for type3 = (car cell3)
          when (typep type3 'sb:header)
            do (let ((entry4 (gethash name table4)))
                 (assert (not (null entry4)))
                 (let* ((cell4 (clostrum-basic::cell entry4))
                        (class4 (car cell4)))
                   (setf (gethash type3 result) class4))))
    result))

;;; This variable holds a hash table mapping each class in E3 to the
;;; analogous class in E4.
(defvar *class-map*)

;;; This variable holds a hash table of visited objects.
(defvar *visited*)

;;; This variable holds the list of objects yet to be processed.
(defvar *worklist*)

(defun object-is-an-impure-ersatz-object-p (object)
  (and (typep object 'sb:header)
       (not (typep (sb:class object) 'sb:header))))

(defun add-to-worklist (item)
  (assert (not (object-is-an-impure-ersatz-object-p item)))
  (push item  *worklist*))

(defun load-worklist-with-classes (e4)
  (loop with table = (clostrum-basic::types e4)
        for entry being each hash-value of table
        for cell = (clostrum-basic::cell entry)
        for class = (car cell)
        do (add-to-worklist class)))

(defun load-worklist-with-operators (e4)
  (loop with table = (clostrum-basic::functions e4)
        for entry being each hash-value of table
        for cell = (clostrum-basic::cell entry)
        for operator = (car cell)
        do (add-to-worklist operator)))

(defun load-worklist-with-method-combination-templates (e4)
  (loop with table = (sicl-environment::method-combination-templates e4)
        for template being each hash-value of table
        do (add-to-worklist template)))

(defun replacement (item)
  (if (object-is-an-impure-ersatz-object-p item)
      ;; It had better be an E3 class.
      (let ((new-item (gethash item *class-map*)))
        (assert (not (null new-item)))
        new-item)
      (progn (unless (gethash item *visited*)
               (setf (gethash item *visited*) t)
               (add-to-worklist item))
             item)))

(defgeneric process-item (item))

(defmethod process-item (item)
  nil)

(defvar *class-slots-function*)

(defmethod process-item ((item sb:header))
  (if (object-is-an-impure-ersatz-object-p (sb:class item))
      ;; This is the case we want to fix up.
      (let* ((class (sb:class item))
             (new-class (gethash class *class-map*)))
        (assert (not (null new-class)))
        ;; Update the class slot.
        (setf (sb:class item) new-class)
        ;; Update the slot with the list of effective slots.
        (setf (aref (sb:rack item) 1)
              (funcall *class-slots-function* new-class))
        (setf (gethash item *visited*) t)
        (let ((rack (sb:rack item)))
          (loop with length = (length rack)
                for i from 0 below length
                do (setf (aref rack i) (replacement (aref rack i))))))
      ;; Otherwise this object already has a pure class.  Do nothing.
      nil))

(defmethod process-item ((item cons))
  (setf (car item) (replacement (car item)))
  (setf (cdr item) (replacement (cdr item))))

(defun tie-the-knot (client e3 e4)
  (let ((*worklist* '())
        (*class-map* (create-class-mapping e3 e4))
        (*visited* (make-hash-table :test #'eq))
        (*class-slots-function*
          (clostrum:fdefinition client e3 @clostrophilia:class-slots)))
    (load-worklist-with-classes e4)
    (load-worklist-with-operators e4)
    (load-worklist-with-method-combination-templates e4)
    (loop until (null *worklist*)
          do (process-item (pop *worklist*)))))
