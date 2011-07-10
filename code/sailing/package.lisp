(defpackage "SAILING"
  (:documentation "Sailing strategy simulations")
  (:use "COMMON-LISP")
  (:export "*RANDOMIZE-COST*"
           "MAKE-INITIAL-STATE"
           "REACH-GOAL-STATE"
           "*TRACE-STATE*"
           "*UCT-EXPLORATION-FACTOR*"
           "*EXPLORATION-DEPTH*"
           "*SAMPLE-COUNT*"
           "RND-SELECT"
           "UCT-SELECT"
           "GCT-SELECT"
           "QCT-SELECT"
           "UQT-SELECT"
           "RCT-SELECT"))
(in-package "SAILING")
