(cl:in-package #:sicl-boot-inspector)

(defun present-pure-object-slots (object stream)
  (loop with e4 = (sicl-boot::e4 *boot*)
        for slot in (aref (rack-of-object object) 1)
        for slot-name = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-name e4) slot)
        for slot-location = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-location e4) slot)
        for slot-value = (aref (rack-of-object object) slot-location)
        do (clouseau:format-place-row
            stream
            object
            'clouseau:pseudo-place
            slot-value
            :label (symbol-name slot-name))))
