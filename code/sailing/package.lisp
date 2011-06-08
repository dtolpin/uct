(defpackage "SAILING"
  (:documentation "Sailing strategy simulations")
  (:use "COMMON-LISP")
  (:export "MAKE-INITIAL-STATE"
           "REACH-GOAL-STATE"
           "UCT-SELECT"
           "UVT-SELECT"))
(in-package "SAILING")
