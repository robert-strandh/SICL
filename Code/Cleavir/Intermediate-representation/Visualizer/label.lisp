(cl:in-package #:cleavir-ir-visualizer)

(defgeneric label (instruction))

(defmethod label (instruction)
  (let* ((class-name (class-name (class-of instruction)))
         (name-string (string-downcase (symbol-name class-name)))
         (length (length name-string))
         (suffix-length (length "-instruction")))
    (subseq name-string 0 (- length suffix-length))))

(defun format-item (item)
  (cond ((symbolp item)
         item)
        ((listp item)
         (mapcar #'format-item item))
        ((typep item 'cleavir-ir:lexical-location)
         (cleavir-ir:name item))
        (t
         (error "unknown item in lambda list ~s" item))))

(defmethod label ((instruction cleavir-ir:enter-instruction))
  (with-output-to-string (stream)
    (format stream "enter ~a"
            (mapcar #'format-item (cleavir-ir:lambda-list instruction)))))

(defmethod label ((instruction cleavir-ir:the-instruction))
  (format nil "the ~a" (cleavir-ir:value-type instruction)))

(defmethod label ((instruction cleavir-ir:typeq-instruction))
  (format nil "typeq ~a" (cleavir-ir:value-type instruction)))

(defmethod label ((instruction cleavir-ir:the-values-instruction))
  (format nil "the (values ~@[~{~s ~}~]~@[&optional ~{~s ~}~]~@[&rest ~s~])"
          (cleavir-ir:required-types instruction)
          (cleavir-ir:optional-types instruction)
          (cleavir-ir:rest-type instruction)))

(defmethod label ((instruction cleavir-ir:dynamic-allocation-instruction))
  "DX")

(defmethod label ((instruction cleavir-ir:compute-argument-count-instruction))
  "ArgC")

(defmethod label ((instruction cleavir-ir:nop-instruction)) "nop")

(defmethod label ((instruction cleavir-ir:unreachable-instruction)) "unreachable")

(defmethod label ((instruction cleavir-ir:assignment-instruction)) "<-")

(defmethod label ((instruction cleavir-ir:funcall-instruction)) "funcall")

(defmethod label ((instruction cleavir-ir:funcall-no-return-instruction))
  "funcall-no-return")

(defmethod label ((instruction cleavir-ir:tailcall-instruction)) "tailcall")

(defmethod label ((instruction cleavir-ir:return-instruction)) "ret")

(defmethod label ((instruction cleavir-ir:fdefinition-instruction)) "fdefinition")

(defmethod label ((instruction cleavir-ir:enclose-instruction))
  (format nil "enclose~:[~; DX~]"
          (cleavir-ir:dynamic-extent-p instruction)))

(defmethod label ((instruction cleavir-ir:unwind-instruction)) "unwind")

(defmethod label ((instruction cleavir-ir:catch-instruction)) "catch")

(defmethod label ((instruction cleavir-ir:eq-instruction)) "eq")

(defmethod label ((instruction cleavir-ir:consp-instruction)) "consp")

(defmethod label ((instruction cleavir-ir:fixnump-instruction)) "fixnump")

(defmethod label ((instruction cleavir-ir:phi-instruction)) "phi")

(defmethod label ((instruction cleavir-ir:symbol-value-instruction)) "symbol-value")

(defmethod label ((instruction cleavir-ir:set-symbol-value-instruction)) "set-symbol-value")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum instructions.

(defmethod label ((instruction cleavir-ir:fixnum-add-instruction)) "fixnum +")

(defmethod label ((instruction cleavir-ir:fixnum-sub-instruction)) "fixnum -")

(defmethod label ((instruction cleavir-ir:fixnum-less-instruction)) "fixnum <")

(defmethod label ((instruction cleavir-ir:fixnum-not-greater-instruction)) "fixnum <=")

(defmethod label ((instruction cleavir-ir:fixnum-equal-instruction)) "fixnum =")

(defmethod label ((instruction cleavir-ir:fixnum-divide-instruction)) "fixnum /")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array instructions.

(defmethod label ((instruction cleavir-ir:aref-instruction))
  (format nil "~:[ns~;s~] ~s aref"
          (cleavir-ir:simple-p instruction)
          (cleavir-ir:element-type instruction)))

(defmethod label ((instruction cleavir-ir:aset-instruction))
  (format nil "~:[ns~;s~] ~s aset"
          (cleavir-ir:simple-p instruction)
          (cleavir-ir:element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Box instructions.

(defmethod label ((instruction cleavir-ir:box-instruction))
  (format nil "~s box"
          (cleavir-ir:element-type instruction)))

(defmethod label ((instruction cleavir-ir:unbox-instruction))
  (format nil "~s unbox"
          (cleavir-ir:element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-point arithmetic instructions.

(defmethod label ((instruction cleavir-ir:float-add-instruction)) "float +")
(defmethod label ((instruction cleavir-ir:float-sub-instruction)) "float -")
(defmethod label ((instruction cleavir-ir:float-mul-instruction)) "float *")
(defmethod label ((instruction cleavir-ir:float-div-instruction)) "float /")
(defmethod label ((instruction cleavir-ir:float-sin-instruction)) "float sin")
(defmethod label ((instruction cleavir-ir:float-cos-instruction)) "float cos")
(defmethod label ((instruction cleavir-ir:float-sqrt-instruction)) "float sqrt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General accessors.

(defmethod label ((instruction cleavir-ir:car-instruction)) "car")

(defmethod label ((instruction cleavir-ir:cdr-instruction)) "cdr")

(defmethod label ((instruction cleavir-ir:rplaca-instruction)) "rplaca")

(defmethod label ((instruction cleavir-ir:rplacd-instruction)) "rplacd")

(defmethod label ((instruction cleavir-ir:nook-read-instruction)) "nook read")

(defmethod label ((instruction cleavir-ir:nook-write-instruction)) "nook write")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for multiple values.

(defmethod label ((instruction cleavir-ir:multiple-to-fixed-instruction)) "M->F")

(defmethod label ((instruction cleavir-ir:fixed-to-multiple-instruction)) "F->M")

(defmethod label ((instruction cleavir-ir:multiple-value-call-instruction)) "MVC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions related to the static runtime environment.

(defmethod label ((instruction cleavir-ir:create-cell-instruction))
  (format nil "Create cell~:[~; DX~]"
          (cleavir-ir:dynamic-extent-p instruction)))

(defmethod label ((instruction cleavir-ir:fetch-instruction)) "Fetch")

(defmethod label ((instruction cleavir-ir:read-cell-instruction)) "Read cell")

(defmethod label ((instruction cleavir-ir:write-cell-instruction)) "Write cell")

(defmethod label ((instruction cleavir-ir:add-activation-record-instruction)) "AddAR")

(defmethod label ((instruction cleavir-ir:remove-activation-record-instruction)) "RemAR")

(defmethod label ((instruction cleavir-ir:load-from-static-environment-instruction))
  "Load")

(defmethod label ((instruction cleavir-ir:store-to-static-environment-instruction))
  "Store")
