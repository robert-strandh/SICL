(cl:in-package #:sicl-boot-phase-6)

(defun create-class-translation-table (e3 e5)
  (let ((result (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((e3-class (env:find-class (env:client e3) e3 symbol)))
          (unless (null e3-class)
            (let ((e5-class (env:find-class (env:client e5) e5 symbol)))
              (assert (not (null e5-class)))
              (setf (gethash e3-class result) e5-class))))))
    result))

(defun update-class-slot (object translation-table)
  (let* ((current-slot-value (slot-value object 'sicl-boot::%class))
         (translation (gethash current-slot-value translation-table)))
    (if (null translation)
        (format *trace-output*
                "Class slot ~s of object ~s is not in table~%"
                object current-slot-value)
        (setf (slot-value object 'sicl-boot::%class) translation))))

(defun find-all-functions (e5)
  (let ((visited (make-hash-table :test #'eq))
        (result '()))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (when (and (env:fboundp (env:client e5) e5 symbol)
                   (not (env:special-operator (env:client e5) e5 symbol))
                   (null (env:macro-function (env:client e5) e5 symbol)))
          (push (env:fdefinition (env:client e5) e5 symbol) result))
        (when (env:fboundp (env:client e5) e5 `(setf ,symbol))
          (push (env:fdefinition (env:client e5) e5 `(setf ,symbol)) result))))
    result))

(defun find-all-sicl-functions (e5)
  (remove-if-not (lambda (x) (typep x 'sicl-boot::header))
                 (find-all-functions e5)))

;;; All SICL objects have a class slot that may contain a non-SICL
;;; class.  TRANSLATION-TABLE maps such non-SICL classes to
;;; corresponding SICL-CLASSES.  CLASS-SLOTS-FUNCTION is a function
;;; that, when applied to a SICL class, returns the list of the
;;; effective slots of that class.  All SICL objects have a list of
;;; effective slots in element 1 of the rack, so we update that list
;;; as well.
(defun update-object (object translation-table class-slots-function)
  (let ((old-class (slot-value object 'sicl-boot::%class))
        (rack (slot-value object 'sicl-boot::%rack)))
    (unless (typep old-class 'sicl-boot::header)
      (let ((new-class (gethash old-class translation-table)))
        (setf (slot-value object 'sicl-boot::%class)
              new-class)
        (setf (aref rack 1)
              (funcall class-slots-function new-class))))))

(defun find-all-classes (e5)
  (let ((result '())
        (visited (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((class (env:find-class (env:client e5) e5 symbol)))
          (unless (null class)
            (push class result)))))
    result))

(defun update-all-class-slots (e3 e5)
  (let ((table (create-class-translation-table e3 e5))
        (class-slots-function
          (env:fdefinition (env:client e5) e5 'sicl-clos:class-slots)))
    (loop for function in (find-all-sicl-functions e5)
          do (update-object function table class-slots-function))
    (loop for class in (find-all-classes e5)
          do (update-object class table class-slots-function))))
