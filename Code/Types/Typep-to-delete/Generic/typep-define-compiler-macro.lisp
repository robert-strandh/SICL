(cl:in-package #:sicl-type)

(define-compiler-macro typep (&whole form object type &optional environment)
  (declare (ignore environment))
  (if (and (consp type)
	   (eq (first type) 'quote)
	   (consp (rest type))
	   (null (rest (rest type))))
      ;; The type argument is a constant.  Return a replacement form
      ;; using TYPEQ.  Since TYPEQ can only be used as the TEST form
      ;; in an IF, we must build an IF form.  We rely on subsequent
      ;; optimizations to get rid of the T and NIL constants whenever
      ;; possible.
      `(if (cleavir-primop:typeq ,object ,(second type)) t nil)
      form))
