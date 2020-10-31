(cl:in-package #:sicl-method-combination)

(defun find-method-combination (name options)
  (let ((template (find-method-combination-template name)))
    (when (null template)
      (error "no such method combination ~s" name))
    (let* ((variant-determiner (variant-signature-determiner template))
           (variant-signature (apply variant-determiner options)))
      (loop for variant in (variants template)
            do (when (equal (sicl-clos:variant-signature variant)
                            variant-signature)
                 (return-from find-method-combination variant)))
      (let ((new-variant (make-instance 'method-combination
                           :variant-signature variant-signature
                           :template template)))
        (push new-variant (variants template))
        new-variant))))
