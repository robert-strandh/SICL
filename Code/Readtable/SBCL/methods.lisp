(cl:in-package #:sbcl-readtable)

(defmethod sicl-readtable:make-dispatch-macro-character
    ((readtable readtable) char &optional non-terminating-p)
  (make-dispatch-macro-character char non-terminating-p readtable))

(defmethod sicl-readtable:get-macro-character ((readtable readtable) char)
  (get-macro-character char readtable))

(defmethod sicl-readtable:set-macro-character
    ((readtable readtable) char function &optional non-terminating-p)
  (set-macro-character char function non-terminating-p readtable))

(defmethod sicl-readtable:get-dispatch-macro-character
    ((readtable readtable) disp-char sub-char)
  (get-dispatch-macro-character disp-char sub-char readtable))

(defmethod sicl-readtable:set-dispatch-macro-character
    ((readtable readtable) disp-char sub-char function)
  (set-dispatch-macro-character disp-char sub-char function readtable))

(defmethod sicl-readtable:syntax-type ((readtable readtable) char)
  (cond ((sb-impl::whitespace[2]p char readtable) :whitespace)
        ((sb-impl::constituentp char readtable) :constituent)
        ((sb-impl::terminating-macrop char readtable) :terminating-macro)
        ((sb-impl::single-escape-p char readtable) :single-escape)
        ((sb-impl::multiple-escape-p char readtable) :multiple-escape)
        (t :non-terminating-macro)))

(defmethod sicl-readtable:copy-readtable-into
    ((from-readtable readtable) (to-readtable readtable))
  (copy-readtable from-readtable to-readtable))

(defmethod sicl-readtable:copy-readtable ((readtable readtable))
  (copy-readtable readtable))
