(cl:in-package #:sicl-boot-phase3)

(loop for symbol in sicl-boot-phase1:*more-names*
      for name = (symbol-name symbol)
      do (import (intern name '#:aspiring-sicl-clos)))
