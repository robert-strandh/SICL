(defun remaining-stack-space ()
  (- sb-vm:*control-stack-end* sb-vm:*control-stack-start*
     (sb-kernel::control-stack-usage)))
