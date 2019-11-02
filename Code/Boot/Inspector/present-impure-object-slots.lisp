(cl:in-package #:sicl-boot-inspector)

(defun present-impure-object-slots (object stream)
  (loop with e3 = (sicl-boot::e3 *boot*)
        for slot in (aref (rack-of-object object) 1)
        for slot-name = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-name e3) slot)
        for slot-location = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-location e3) slot)
        for slot-value = (aref (rack-of-object object) slot-location)
        do (clouseau:format-place-row
            stream
            object
            'clouseau:pseudo-place
            slot-value
            :label (symbol-name slot-name))))
