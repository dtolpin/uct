(defpackage "TREEEXP"
  (:documentation "Comparing MCTS algorithms on search trees")
  (:use "COMMON-LISP" "MCTS" "COMMON-LISP-USER")
  (:export "EXPERIMENT"
           "EXPERIMENTS" "VARARM-EXPERIMENTS"
           "MANY-EXPERIMENTS"
           "MAKE-TREE"
           "BEST-MEAN"
           "*MAKE-ARM*"
           "*MAKE-ALPHA-SWITCH*"
           "*MAKE-BETA-SWITCH*"
           "*CHOOSE*"
           "*MAX-REWARD*"
           "+RANDOM-TREE-2X2+"
           "+FIXED-TREE-3X2+"))
(in-package "TREEEXP")

(defvar *make-arm* #'make-armf)
(defvar *make-alpha-switch* #'make-switch)
(defvar *make-beta-switch* #'make-switch)
(defvar *choose* #'max)

(defconstant +fringe-width+ 1)

(defvar *max-reward* 1.0)

(defun make-fringe ()
  "make a random fringe with mean 0.5"
  (map 'vector (lambda (m) (funcall *make-arm* :mean m))
       (loop repeat +fringe-width+
          append (let ((v (+ 0.5 (* (- *max-reward* 0.5) (random 1.0))))) (list v (- 1.0 v))))))

(defun make-tree (levels branching
                  &optional
                  (make-alpha-switch *make-alpha-switch*) (make-beta-switch *make-beta-switch*))
  "make a random tree"
  (funcall make-alpha-switch 
   :nodes (if (= levels 1)
              (make-fringe)
              (coerce (loop repeat branching
                         collect (make-tree (1- levels) branching
                                            make-beta-switch make-alpha-switch))
                      'vector))))

(defun best-mean (tree)
  "find the max mean of arms"
  (etypecase tree
    (arm (arm-mean tree))
    (switch (reduce *choose* (map 'list #'best-mean (switch-nodes tree))))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter +algorithms+ '("UCT" "GCT" "RCT" "RND")
    "algorithms to compare"))

(defun experiment (&key levels branching sampling-factor nruns vararm (algorithms +algorithms+))
  (compute-uqb-factor branching)
  (flet ((avgrwd (select)
           (/ (float (loop repeat nruns
                        sum (let* ((tree (with-unique-node-ids (make-tree levels branching))))
                              (- (best-mean tree)
                                 (pull-best-arm tree
                                                select sampling-factor)))))
              nruns)))
    (format t "~D~T~{~8F~^~T~}~%"
            (if vararm branching (* branching sampling-factor))
            (mapcar #'(lambda (alg)
                        (avgrwd (symbol-function
                                 (intern (format nil "~@:(~A~)-SELECT" alg)))))
                    algorithms)))
  (force-output *standard-output*))

(defun experiments (&key levels branching min-sf sf-step n-sf nruns (algorithms +algorithms+))
  (format t "nsamples~T~{r_~A~^~T~}~%" algorithms)
  (loop for sf = min-sf then (ceiling (* sf sf-step)) repeat n-sf
       do (experiment :levels levels :branching branching :sampling-factor sf :nruns nruns
                      :algorithms algorithms)))

(defun vararm-experiments (&key levels sampling-factor min-b b-step n-b nruns (algorithms +algorithms+))
  (format t "nsamples~T~{r_~A~^~T~}~%" algorithms)
  (loop for b = min-b then (round (* b b-step)) repeat n-b
     do (experiment :levels levels :branching b :sampling-factor sampling-factor
                    :nruns (ceiling (/ nruns (log b 2))) :vararm t
                    :algorithms algorithms)))

(defconstant +number-of-runs+ 16000)

(defun many-experiments ()
  (loop for b = 8 then (* b 2) repeat 5
     do (with-open-file (*standard-output* (format nil "twolevel-~S.txt" b)
                                                :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
          (experiments :levels 2 :branching b :min-sf 4 :sf-step 1.3 :n-sf 12 
                       :nruns (ceiling (/ +number-of-runs+ (log b 2)))))))

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