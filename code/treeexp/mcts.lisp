(defpackage "MCTS"
  (:documentation "algorithms for Monte Carlo Tree Sampling")
  (:use "COMMON-LISP")
  (:export "*SAMPLING-FACTOR*"
           "SWITCH" "MAKE-SWITCH"
           "ARM" "MAKE-ARM" "ARM-MEAN"
           "MAKE-ARMB" "MAKE-ARMF"
           "PULL-BEST-ARM"
           "RANDOM-SELECT"
           "UCT-SELECT"
           "UVT-SELECT"
           "VCT-SELECT"))
(in-package "MCTS")

;;; Monte-Carlo Tree Sampling
;;; Common Notions

(defparameter *sampling-factor* 0
  "see function `number-of-samples'")

(defvar *node-id* 0
  "node identifier, internally used to uniquely
   identify nodes")

(defun shuffled-indices (sequence)
  "generate a shuffled list of indices"
  (let* ((i -1)
         (indices (map 'list (lambda (x) (declare (ignore x)) (incf i))
                       sequence)))
    (sort indices #'> :key (lambda (x) (declare (ignore x)) (random 1.0)))))

(defstruct node
  "search tree node, either a switch or an arm"
  (id (incf *node-id*) :read-only t))

(defstruct (switch (:include node))
  "a switch (internal) node"
  nodes) ; node vector

(defstruct (arm (:include node))
  "an arm"
  mean)

(defun number-of-samples (switch)
  "compute number of samples per node"
  (* (1+ *sampling-factor*) (length (switch-nodes switch))))


(defstruct stat
  "play stat"
  (count 0)
  (sum 0.0))

(defun stat-avg (stat)
  (/ (stat-sum stat) (max (stat-count stat) 1)))

(defun stat-key (switch node) 
  "key of action in stats"
  (cons (node-id switch) (node-id node)))

(defvar *play-stats* nil
  "play stats used by adaptive sampling algorithms")

(defun get-stat (switch node)
  "get statistics for switch node pair, initializing
   to empty statistics if unsampled"
  (let ((key (stat-key switch node)))
    (or (gethash key *play-stats*)
        (setf (gethash key *play-stats*) (make-stat)))))

(defgeneric play (node select)
  (:documentation "play from this node, return reward"))

(defmethod play ((switch switch) select)
  "playing a switch --- selecting a direction"
  (multiple-value-bind (node select) (funcall select switch)
    (update-stats switch node (play node select))))

(defun update-stats (switch node reward)
  (let ((stat (get-stat switch node)))
    (incf (stat-count stat))
    (incf (stat-sum stat) reward))
  reward)

(defmethod play ((arm arm) select)
  "playing a leaf node --- drawing the arm"
  (declare (ignore select))
  (draw arm))

(defun mk-commit-select (sampling-select)
  "sample actions, choose the one with the highest average"
  (labels ((commit-select (switch)
             ;; gather playing statistics
             (dotimes (i (number-of-samples switch))
               (multiple-value-bind (node sampling-select)
                   (funcall sampling-select switch)
                 (update-stats switch node (play node sampling-select))))
             
             ;; extract statistics
             (let ((stats (map 'vector (lambda (node) (get-stat switch node))
                               (switch-nodes switch)))
                   (nodemax nil)
                   (avgmax -1.0))

               ;; select best action
               (dolist (i (shuffled-indices stats) (values nodemax #'commit-select))
                 (let ((avg (stat-avg (aref stats i))))
                   (when (> avg avgmax) 
                     (setf nodemax (aref (switch-nodes switch) i)
                           avgmax avg)))))))
    #'commit-select))

(defgeneric draw (arm)
  (:documentation "draw the arm and return the outcome"))
 
(defun pull-best-arm (tree sampling-select 
                      &optional (*sampling-factor* *sampling-factor*))
  "pull the best arm"
  (let ((*play-stats* (make-hash-table :test #'equal)))
    (play tree (mk-commit-select sampling-select))))

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
  (if (< (random 1.0) (arm-mean arm)) 1.0 0.0))

;;; Random sampling

(defun random-select (switch)
  (values (aref (switch-nodes switch) (random (length (switch-nodes switch))))
          #'random-select))

;; make sampling select function by closing on the statistics hash table

(defun ucb (switch)
  "UCB selection: max (avg+sqrt(2*log (n) / ni))"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (root-2-log-n
          (sqrt (* 2.0 (reduce #'+ node-stats :key #'stat-count :initial-value 0))))
         (nodemax nil)
         (bmax 0.0))
    (dolist (i (shuffled-indices (switch-nodes switch)) nodemax)
      (let ((b (if (> (stat-count (aref node-stats i)) 0)
                   (+ (aref avgs i)
                      (/ root-2-log-n (sqrt (stat-count (aref node-stats i)))))
                   most-positive-fixnum)))
        (when (> b bmax)
          (setf nodemax (aref (switch-nodes switch) i)
                bmax b))))))
                     
                      
(defun uvb (switch)
  "UVB selection: max [(1-1/k)/ni for best, 1/k/ni for rest]"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (avgmax (reduce #'max avgs))
         (kappa (/ 1.0 (length (switch-nodes switch))))
         (nodemax nil)
         (bmax 0.0))
    (dolist (i (shuffled-indices (switch-nodes switch)) nodemax)
      (let ((b (if (> (stat-count (aref node-stats i)) 0)
                   (/ (if (= (aref avgs i) avgmax) (- 1.0 kappa) kappa)
                      (stat-count (aref node-stats i)))
                   most-positive-fixnum)))
        (when (> b bmax)
          (setf nodemax (aref (switch-nodes switch) i)
                bmax b))))))

;;; UCT

(defun uct-select (switch)
  (values (ucb switch) #'uct-select))

;;; UVT

(defun uvt-select (switch)
  (values (uvb switch) #'uvt-select))

;;; VCT

(defun vct-select (switch)
  (values (uvb switch) #'uct-select))

