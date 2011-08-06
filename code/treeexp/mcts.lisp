(defpackage "MCTS"
  (:documentation "algorithms for Monte Carlo Tree Sampling")
  (:use "COMMON-LISP")
  (:export "*SAMPLING-FACTOR*" "WITH-UNIQUE-NODE-IDS"
           "SWITCH" "MAKE-SWITCH" "MAKE-ANTISWITCH" "SWITCH-NODES"
           "ARM" "MAKE-ARM" "ARM-MEAN"
           "MAKE-ARMF" "MAKE-ARMB" "MAKE-ARMV"
           "PULL-BEST-ARM"
           "RND-SELECT"
           "RCT-SELECT"
           "UCT-SELECT"
           "GCT-SELECT"
           "QCT-SELECT"
           "VCT-SELECT"
           "COMPUTE-UQB-FACTOR"
           "*UQB-ALPHA*"))
(in-package "MCTS")

(defvar *debug* nil)

(defparameter *uqb-alpha* 8)

;;; Monte-Carlo Tree Sampling
;;; Common Notions

(defparameter *sampling-factor* 0
  "see function `number-of-samples'")

(defvar *node-id* 0
  "node identifier, internally used to uniquely
   identify nodes")

(defmacro with-unique-node-ids (&rest body)
  "evaluate `body' inside a new binding for *node-id*,
   so that all nodes generated by calls from `body' have
   distinct ids"
  `(let ((*node-id* 0))
     ,@body))

(defun shuffled-indices (sequence)
  "generate a shuffled list of indices"
  (let* ((i -1)
         (indices (map 'list (lambda (x) (declare (ignore x)) (cons (incf i) (random 1.0)))
                       sequence)))
    (mapcar #'car (sort indices #'> :key #'cdr))))

(defstruct node
  "search tree node, either a switch or an arm"
  (id (incf *node-id*) :read-only t))

(defstruct (switch (:include node))
  "a maximizing switch (internal) node"
  nodes) ; node vector

(defstruct (antiswitch (:include switch))
  "minimizing switch")

(defgeneric better-reward (switch reward best-reward)
  (:documentation "true iff reward better than best-reward")
  (:method ((switch switch) reward best-reward)
    (> reward best-reward))
  (:method ((switch antiswitch) reward best-reward)
    (< reward best-reward)))

(defgeneric lowest-reward (switch)
  (:documentation "the worst reward value")
  (:method ((switch switch)) most-negative-single-float)
  (:method ((switch antiswitch)) most-positive-single-float))

(defgeneric highest-reward (switch)
  (:documentation "the best reward value")
  (:method ((switch switch)) most-positive-single-float)
  (:method ((switch antiswitch)) most-negative-single-float))

(defgeneric upper-bound (switch reward bound)
  (:documentation "upper bound on reward")
  (:method ((switch switch) reward bound) (+ reward bound))
  (:method ((switch antiswitch) reward bound) (- reward bound)))

(defstruct (arm (:include node))
  "an arm"
  mean)

(defun number-of-samples (switch)
  "compute number of samples per node"
  (* (1+ *sampling-factor*) (length (switch-nodes switch))))


;; Handling sampling statistics

(defstruct stat
  "play stat"
  (count 0)
  (sum 0.0)
  (rewards '(0.0 1.0)))

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

(defun update-stats (switch node reward)
  (let ((stat (get-stat switch node)))
    (push reward (stat-rewards stat))
    (incf (stat-count stat))
    (incf (stat-sum stat) reward))
  reward)

;; Playing a playout

(defgeneric play (node select)
  (:documentation "play from this node, return reward"))

(defmethod play ((switch switch) select)
  "playing a switch --- selecting a direction"
  (multiple-value-bind (node select) (funcall select switch)
    (multiple-value-bind (reward mean) (play node select)
      (update-stats switch node reward)
      (values reward mean))))

(defmethod play ((arm arm) select)
  "playing a leaf node --- drawing the arm"
  (declare (ignore select))
  (values (draw arm) (arm-mean arm)))

;; Move selection function: sampling, then committing

(defun mk-commit-select (sampling-select)
  "sample actions, choose the one with the best average"
  (labels ((commit-select (switch)
             ;; gather playing statistics
             (dotimes (i (number-of-samples switch))
               (multiple-value-bind (node sampling-select)
                   (funcall sampling-select switch)
                 (when node ; sample if node is not nil, skip otherwise
                   (update-stats switch node (play node sampling-select)))))
             
             ;; extract statistics
             (let ((stats (map 'vector (lambda (node) (get-stat switch node))
                               (switch-nodes switch)))
                   (best-node nil)
                   (best-avg (lowest-reward switch)))

               (when (member :print-stats *debug*)
                 (progn (format t "~&~%")
                        (map nil #'(lambda (stat) 
                                     (format t "~@{~S~^ ~}~%"
                                             (stat-count stat) (stat-avg stat)))
                             (sort (copy-seq stats) #'> :key #'stat-avg))))
               
               ;; select best action
               (dolist (i (shuffled-indices stats) (values best-node #'commit-select))
                 (let ((avg (stat-avg (aref stats i))))
                   (when (better-reward switch avg best-avg) 
                     (setf best-node (aref (switch-nodes switch) i)
                           best-avg avg)))))))
    #'commit-select))

;; Various arms with different reward distributions can be defined
(defgeneric draw (arm)
  (:documentation "draw the arm and return the outcome"))

;; Finally, the main function --- pulling the best arm
(defun pull-best-arm (tree sampling-select 
                      &optional (*sampling-factor* *sampling-factor*))
  "pull the best arm"
  (let ((*play-stats* (make-hash-table :test #'equal)))
    (nth-value 1 (play tree (mk-commit-select sampling-select)))))

;; Basic arm kinds
(defstruct (armf (:include arm))
  "fixed arm")
(defstruct (armb (:include arm))
   "Bernulli arm")

(defstruct (armv (:include arm))
  "Variance arm")

(defmethod draw ((arm armf))
  "fixed arm: always the mean"
  (arm-mean arm))

(defmethod draw ((arm armb))
  "Bernoulli arm: either 0.0 or 1.0"
  (if (< (random 1.0) (arm-mean arm)) 1.0 0.0))

(defmethod draw ((arm armv))
  "Fixed variance arm: either mean+delta or mean-delta"
  (if (< (random 1.0) 0.5)
      (+ (arm-mean arm) 0.5) (- (arm-mean arm) 0.5)))


;;; Sampling algorithms

;; Random sampling
(defun rnd (switch)
  "Uniform random sampling" 
  (aref (switch-nodes switch)
        (random (length (switch-nodes switch)))))

;; Single-level adaptive selection
(defun ucb (switch)
  "UCB selection: max (avg+sqrt(2*log (n) / ni))"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (root-2-log-n
          (sqrt (* 2.0 (log (max 1.0 (reduce #'+ node-stats :key #'stat-count :initial-value 0))))))
         (best-node nil)
         (best-reward (lowest-reward switch)))
    (dolist (i (shuffled-indices (switch-nodes switch)) best-node)
      (when (zerop (stat-count (aref node-stats i)))
        (return (aref (switch-nodes switch) i)))
      (let ((reward (upper-bound switch (aref avgs i)
                                 (/ root-2-log-n
                                    (sqrt (stat-count (aref node-stats i)))))))
        (when (better-reward switch reward best-reward)
          (setf best-node (aref (switch-nodes switch) i)
                best-reward reward))))))

(defvar *uqb-factor* 1.0)

(defun compute-uqb-factor (k &optional (alpha *uqb-alpha*))
  "Computes *uqb-factor* such that 
    *uqb-factor* * sqrt(n)=2*log(n) for 
    n: 2*log(n)==n/2/k"
  (flet ((equ (n) (- (* alpha (log n)) (coerce (/ n k 2) 'double-float))))
    (let* ((xa k) (xb (floor most-positive-fixnum 2))
           (fa (equ xa)) (fb (equ xb))
           (n (loop
                 (when (< (abs (- fa fb)) 1e-3) (return xa))
                 (let* ((xc (* 0.5 (+ xa xb)))
                        (fc (equ xc)))
                   (if (= (signum fc) (signum fb))
                       (setf xb xc
                             fb fc)
                       (setf xa xc
                             fa fc))))))
      (setf *uqb-factor* (/ (* 2.0 (log n)) (sqrt n))))))
           
(defun uqb (switch)
  "UQB selection: max (avg+sqrt(2*sqrt (n) / ni))"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (root-2-sqrt-n
          (sqrt (* *uqb-factor* (sqrt (max 1.0 (reduce #'+ node-stats :key #'stat-count
                                                       :initial-value 0))))))
         (best-node nil)
         (best-reward (lowest-reward switch)))
    (dolist (i (shuffled-indices (switch-nodes switch)) best-node)
      (when (zerop (stat-count (aref node-stats i)))
        (return (aref (switch-nodes switch) i)))
      (let ((reward (upper-bound switch (aref avgs i)
                                 (/ root-2-sqrt-n
                                    (sqrt (stat-count (aref node-stats i)))))))
        (when (better-reward switch reward best-reward)
          (setf best-node (aref (switch-nodes switch) i)
                best-reward reward))))))

(defun grd (switch)
  "0.5-greedy selection"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (k (length (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (best-node nil)
         (best-reward (lowest-reward switch)))
    (dolist (i (shuffled-indices (switch-nodes switch)) 
             (if (> (random 1.0) (* 0.5 (/ k (1- k))))
                 best-node
                 (aref (switch-nodes switch) (random k))))
      (when (zerop (stat-count (aref node-stats i)))
        (return (aref (switch-nodes switch) i)))
      (let ((reward (aref avgs i)))
        (when (better-reward switch reward best-reward)
          (setf best-node (aref (switch-nodes switch) i)
                best-reward reward))))))

(defun voi (avg rewards cmp)
  "complete voi"
  (let ((len (length rewards)))
    (/ (reduce (lambda (sum v) (+ sum (abs (- v avg))))
               (remove-if (lambda (x) (funcall cmp avg x)) rewards)
               :initial-value 0.0)
       (* len len))))
                     
(defun uvb (switch)
  "UVB selection: max [(1-1/k)/ni for best-, 1/k/ni for rest]"
  (let* ((node-stats (map 'vector (lambda (node) (get-stat switch node))
                          (switch-nodes switch)))
         (avgs (map 'vector #'stat-avg node-stats))
         (alpha 0.0)
         (beta 0.0)
         (best-node nil)
         (best-reward 0.0))
    (dotimes (i (length avgs))
      (let ((avg (aref avgs i)))
        (cond
          ((> avg alpha)
           (psetf beta alpha
                  alpha avg))
          ((> alpha avg beta)
           (psetf beta avg)))))
    (dolist (i (shuffled-indices (switch-nodes switch)) best-node)
      (when (zerop (stat-count (aref node-stats i)))
        (return (aref (switch-nodes switch) i)))
      (let ((reward (if (= (aref avgs i) alpha)
                        (voi beta (stat-rewards (aref node-stats i)) #'<)
                        (voi alpha (stat-rewards (aref node-stats i)) #'>))))
        (when (>= reward best-reward)
          (setf best-node (aref (switch-nodes switch) i)
                best-reward reward))))))

;;; Adaptive sampling selection functions for passing to `pull-best-arm'

;; Random

(defun rnd-select (switch)
  (values (rnd switch) #'rnd-select))

;; Random than UCT

(defun rct-select (switch)
  (values (rnd switch) #'uct-select))

;; UCT

(defun uct-select (switch)
  (values (ucb switch) #'uct-select))

;; GCT 

(defun gct-select (switch)
  (values (grd switch) #'uct-select))

;; QCT

(defun qct-select (switch)
  (values (uqb switch) #'uct-select))

;; VCT

(defun vct-select (switch)
  (values (uvb switch) #'uct-select))

;; Testing

(defmacro RUN-TEST (test-name)
  `(progn 
     (format *error-output* " ~(~A~)" ',test-name)
     (clear-output *error-output*)
     (,test-name)
     (format *error-output* " (ok)")
     (clear-output *error-output*)))

(let ((test-tree (make-switch :nodes (vector (make-armf :mean 0.0) (make-armf :mean 1.0))))
      (*sampling-factor* 1))

  (defun test-uct ()
    (assert (= (pull-best-arm test-tree #'uct-select) 1.0)))

  (defun test-vct ()
    (assert (= (pull-best-arm test-tree #'uct-select) 1.0))))

(defun test ()
  (format *error-output* "Testing ~A:" (package-name (symbol-package 'test)))
  (run-test test-uct)
  (run-test test-vct)
  (format *error-output* "~%") (clear-output *error-output*))


(eval-when (:execute :load-toplevel)
  (test))
