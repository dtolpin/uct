(defpackage "TREEEXP"
  (:documentation "Comparing MCTS algorithms on search trees")
  (:use "COMMON-LISP" "MCTS" "COMMON-LISP-USER")
  (:export "EXPERIMENT"
           "MAKE-TREE"
           "MAX-MEAN"
           "+RANDOM-TREE-2X2+"
           "+FIXED-TREE-3X2+"))
(in-package "TREEEXP")

(defun make-fringe (make-arm)
  "make a random fringe with mean 0.5"
  (map 'vector (lambda (m) (funcall make-arm :mean m))
       (loop as i below 1
          append (let ((v (random 1.0))) (list v (- 1.0 v))))))
                    
(defun make-tree (levels branching make-arm)
  "make a random tree"
  (make-switch 
   :nodes (if (= levels 1)
              (make-fringe make-arm)
              (coerce (loop as i below branching
                         collect (make-tree (1- levels) branching make-arm))
                      'vector))))

(defun max-mean (tree)
  "find the max mean of arms"
  (etypecase tree
    (arm (arm-mean tree))
    (switch (reduce #'max (map 'list #'max-mean (switch-nodes tree))))))

(defun experiment (levels branching make-arm sampling-factor nruns)
  (flet ((avgrwd (select)
           (/ (float (loop as i below nruns
                        sum (let ((tree (make-tree levels branching
                                                   make-arm)))
                              (- (max-mean tree) 
                                 (pull-best-arm tree
                                                select sampling-factor)))))
              nruns)))
    (let* ((vcreg (avgrwd #'vct-select))
           (ucreg (avgrwd #'uct-select))
           (uvreg (avgrwd #'uvt-select))
           (rnreg (avgrwd #'random-select)))
      (format t "~S ~S ~S ~S~%" vcreg ucreg uvreg rnreg))))

;; Predefined trees for testing 

(defparameter +random-tree-2x2+ 
  (make-switch 
   :nodes (vector (make-switch 
                   :nodes (vector (make-armb :mean 0.4)
                                  (make-armb :mean 0.6)))
                  (make-switch
                   :nodes (vector (make-armb :mean 0.1)
                                  (make-armb :mean 0.9))))))

(defparameter +fixed-tree-3x2+
  (make-switch 
   :nodes (vector (make-switch :nodes (vector (make-switch 
                                               :nodes (vector (make-armf :mean 0.4)
                                                              (make-armf :mean 0.6)))
                                              (make-switch
                                               :nodes (vector (make-armf :mean 0.1)
                                                              (make-armf :mean 0.9))))) 

                  (make-switch :nodes (vector (make-switch 
                                               :nodes (vector (make-armf :mean 0.3)
                                                              (make-armf :mean 0.7)))
                                              (make-switch
                                               :nodes (vector (make-armf :mean 0.2)
                                                              (make-armf :mean 0.8))))))))