(defun remaining-stack-space ()
  (- (/ (sb-sys:sap-int (sb-kernel:control-stack-pointer-sap)) 2)
     sb-vm:*control-stack-start*))
