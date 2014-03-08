(cl:in-package #:sicl-boot-phase3)

(loop for name in sicl-boot-phase1:*more-names*
      do (import (intern name '#:aspiring-sicl-clos)))
