(defpackage "MCTS"
  (:documentation "algorithms for Monte Carlo Tree Sampling")
  (:use "COMMON-LISP")
  (:export "SWITCH" "MAKE-SWITCH"
           "ARM" "MAKE-ARM" "ARM-MEAN"
           "PLAY"
           "DRAW"
           "SELECT-RANDOMLY"))
(in-package "MCTS")

;;; Monte-Carlo Tree Sampling
;;; Common Notions

;; Simple unique identifier generator for nodes
(defvar *node-id* 0)

(defstruct node
  "search tree node, either a switch or an arm"
  (id (incf *node-id*) :read-only t))

(defstruct (switch (:include node))
  "a switch (internal) node"
  nodes)

(defstruct (arm (:include node))
  "an arm"
  mean)

(defgeneric play (node select level n-levels)
  (:documentation "play from this node"))

(defmethod play ((switch switch) select level n-levels)
  (multiple-value-bind (node select) 
      (funcall select switch level n-levels)
    (play node select (1+ level) n-levels))) 

(defmethod play ((arm arm) select level n-levels)
  (declare (ignore select level n-levels))
  arm)

(defstruct (arm (:include node))
  "an arm"
  mean)

(defgeneric draw (arm)
  (:documentation "draw the arm and return the outcome"))

;; Random selection
(defun select-randomly (switch level n-levels)
  (declare (ignore level n-levels))
  (values (nth (random (list-length (switch-nodes switch)))
               (switch-nodes switch))
          #'select-randomly))
 
;; Basic arm kinds
(defstruct (armf (:include arm))
  "fixed arm")
(defstruct (armb (:include arm))
   "Bernulli arm")

(defmethod draw ((arm armf))
  "fixed arm: always the mean"
  (arm-mean arm))

(defmethod draw ((arm armb))
  "Bernoulli arm: either 0.0 or 1.0"
  (if (< (random 1.0) (arm-mean arm)) 1.0 0.0)))

;;; UCB



;;; UVB


;;; UCT


;;; UVT


;;; VCT