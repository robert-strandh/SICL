(cl:in-package #:cleavir-internationalization)

(setf *locale*
      (make-instance 'locale
	:language (make-instance 'english)))
