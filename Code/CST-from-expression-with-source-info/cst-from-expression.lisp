(cl:in-package #:sicl-cst-from-expression-with-source-info)

(defclass client () ())

(defmethod incless:write-object ((client client) object stream)
  (if *print-pretty*
      (funcall (in:pprint-dispatch
                client
                inravina-extrinsic:*print-pprint-dispatch*
                object)
               stream object)
      (write object :stream stream)))

(defvar *cst*)

(defun assign-source-info (cst object source)
  (cond ((and (null (cst:source cst))
              (eq object (cst:raw cst)))
         (reinitialize-instance cst :source source)
         t)
        ((typep cst 'cst:atom-cst)
         nil)
        ((assign-source-info (cst:rest cst) object source)
         nil)
        (t
         (assign-source-info (cst:first cst) object source))))

(defmethod incless:write-object :around
    ((client client) object (stream vector-of-lines-stream))
  (let* ((start (make-instance 'sicl-source-tracking:source-position
                  :lines (contents stream)
                  :line-index (current-line-index stream)
                  :character-index (current-character-index stream)))
         (result (call-next-method))
         (end (make-instance 'sicl-source-tracking:source-position
                :lines (contents stream)
                :line-index (current-line-index stream)
                :character-index (current-character-index stream)))
         (source (cons start end)))
    (assign-source-info *cst* object source)
    result))

(defvar inravina-extrinsic:*client*)

(defun cst-from-expression (expression)
  (let* ((*cst* (cst:cst-from-expression expression))
         (client (make-instance 'client))
         (stream (make-instance 'vector-of-lines-stream)))
    (setq inravina-extrinsic:*client* client)
    (incless:write-object client expression stream)
    *cst*))

;;; For some reason, the stream object is not passed on to nested
;;; calls to INCLESS:WRITE-OBJECT, so the :AROUND method is invoked
;;; only for the top expression.
