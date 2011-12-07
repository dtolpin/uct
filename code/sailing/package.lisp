(defpackage "SAILING"
  (:documentation "Sailing strategy simulations")
  (:use "COMMON-LISP")
  (:export "*RANDOMIZE-COST*"
           "MAKE-INITIAL-STATE"
           "REACH-GOAL-STATE"
           "*TRACE-STATE*"
           "*UCB-EXPLORATION-FACTOR*"
           "*UQB-EXPLORATION-FACTOR*"
           "*EXPLORATION-DEPTH*"
           "*SAMPLE-COUNT*"
           "RND-SELECT"
           "UCT-SELECT"
           "GCT-SELECT"
           "QCT-SELECT"
           "CCT-SELECT"
           "UQT-SELECT"
           "RCT-SELECT"))
(in-package "SAILING")
