(cl:in-package #:sicl-boot-phase-8)

(defmethod symbol-name :around (symbol)
  (if (host-symbolp symbol)
      (host-symbol-name symbol)
      (call-next-method)))
