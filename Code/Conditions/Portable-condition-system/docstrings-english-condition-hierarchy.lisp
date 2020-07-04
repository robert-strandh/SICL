(cl:in-packate #:portable-condition-system)

(setf (documentation 'restart-not-found 'type)
      #.(format
         nil
         "A condition type signaled when a restart with a given name~@
          was not found, even thought it was expected."))

(setf (documentation 'abort-failure 'type)
      #.(format
         nil
         "A condition type signaled when the ABORT restart invoked by function~@
          ABORT failed to transfer control outside of the function.")) 

(setf (documentation 'case-failure 'type)
      #.(format
         nil
         "A condition type signaled when a case assertion (such as ECASE,~@
          ETYPECASE, CCASE, or CTYPECASE) fails to match its keyform."))
