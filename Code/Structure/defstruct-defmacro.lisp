(cl:in-package #:sicl-structure)

(defmacro defstruct (&environment environment name-and-options &rest slots)
  ;; Due to some bootstrapping concerns, the environment is currently ignored.
  ;; "<beach> So to summarize the "rule": any generic function that takes a
  ;; first-class global environment object as an argument during bootstrapping
  ;; is a host function imported to some environment."
  (declare (ignore environment))
  ;; Passing NIL as the environment works out fine for now.
  (expand-defstruct (parse-defstruct name-and-options slots) nil))
