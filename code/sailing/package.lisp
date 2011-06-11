(defpackage "SAILING"
  (:documentation "Sailing strategy simulations")
  (:use "COMMON-LISP")
  (:export "MAKE-INITIAL-STATE"
           "REACH-GOAL-STATE"
           "RANDOM-SELECT"
           "*UCT-EXPLORATION-FACTOR*"
           "*SAMPLE-COUNT*"
           "UCT-SELECT"
           "GCT-SELECT"
           "RCT-SELECT"
           "UVT-SELECT"
           "VCT-SELECT"
           "CRT-SELECT"
           "VRT-SELECT"))
(in-package "SAILING")
