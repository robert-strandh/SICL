(cl:in-package #:sicl-sequence)

(defmacro once-only (specs &body body)
  (once-only-expander specs body))

(defun once-only-expander (specs body)
  (if (null specs)
      `(progn ,@body)
      (multiple-value-bind (var form)
          (parse-once-only-spec (first specs))
        (let ((tmp (gensym "TMP")))
          `(let ((,var (gensym ,(symbol-name var)))
                 (,tmp ,form))
             `(let ((,,var ,,tmp))
                ,,(once-only-expander (rest specs) body)))))))

(defun parse-once-only-spec (spec)
  (typecase spec
    (symbol
     (values spec spec))
    ((cons symbol (cons symbol null))
     (values (first spec) (second spec)))
    (error "Malformed ONCE-ONLY spec: ~S" spec)))

