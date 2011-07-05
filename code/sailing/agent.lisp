(in-package "SAILING")

;;; Sailing Agent

(declaim (type (member :static :dynamic) *sample-count*))
;; static sample count is better for experiments, dynamic for deployment
(defparameter *sample-count* :static 
  "mode of counting samples: 
   :static - the number of playouts beginning in a path node is *nsamples*
   :dynamic - the number of playouts passed through a path node is *nsamples*")
(defparameter *uct-exploration-factor* 4.0
  "the greater the factor, the more exploratory is UCT")

(defvar *trace-state* #'identity
  "function called on each state")
(defvar *nsamples* 32
  "number of playouts per state")

;; Stopping discipline: stop randomly with probability  1/state-nsamples

(defun leaf-state-p (state)
  "true when the state should not be expanded"
  (< (random 1.0) (/ (+ 1.0 (state-nsamples state)))))

(defun evaluate-state (state)
  "state evaluation function"
  ; optimistic
  (* +away-cost+
     (dist (state-x state) (state-y state) *size* *size*)))

;; Bounding rewards
(defun Cp (state)
  (declare (ignore state))
  "UCT factor $C_p$ in $2 C_p \sqrt{\frac {log n_i} n}$"
  (* *uct-exploration-factor*
     (- (+ +up-cost+ +delay-cost+) +away-cost+)))

;; Playing (sampling and committing)

(defun play (state select)
  "play in the current state and return the reward,
   actual or estimated"
  (cond
    ((goal-state-p state) 0)
    (t (multiple-value-bind (leg select)
           (funcall select state)
         (update-stats
          state leg
          (+ (leg-cost state leg)
             (if (leaf-state-p state) 
                 (evaluate-state state)
                 (play (next-state state leg) select))))))))

;; Sampling statistics
(defstruct stat
  "play stat"
  (count 0 :type fixnum)
  (sum 0.0d0 :type double-float))

(defvar *play-stats* nil ; hash table, created in reach-goal-state
  "playout statistics")

(defun state-leg-key (state leg)
  (declare (type fixnum leg) (type state state))
  "key of state+leg combination"
  (+ (state-ptack state)
     (* 3
        (+ (state-wind state)
           (* +ndirs+
              (+ leg 
                 (* +ndirs+
                    (+ (1- (state-x state))
                       (* *size* (1- (state-y state)))))))))))

(defun get-stat (state leg)
  "returns statistics for the given (state,leg) combination"
  (let ((key (state-leg-key state leg)))
    (or (gethash key *play-stats*)
        (setf (gethash key *play-stats*) (make-stat)))))

(defun update-stats (state leg cost)
  "updates statistics for the given (state, leg) combination"
  (let ((stat (get-stat state leg)))
    (incf (stat-count stat))
    (incf (stat-sum stat) cost))
  cost)

(defvar *play-stat-vectors* nil ; hash table for memoizing state stat vectors
  "state stats vectors")

(defun get-stats (state)
    "return stats vector"
    (let ((key (state-leg-key state 0)))
      (or (gethash key *play-stat-vectors*)
          (setf (gethash key *play-stat-vectors*)
                (map 'vector (lambda (leg) (get-stat state leg)) +legs+)))))

(defun stat-avg (stat)
  "returns average cost"
  (if (plusp (stat-count stat))
      (/ (stat-sum stat) (stat-count stat))
      +into-cost+))

(defun state-nsamples (state)
  "number of samples (playouts) passed through the state"
  (reduce #'+ +legs+ :key (lambda (leg) (stat-count (get-stat state leg)))))

(defun stats-nsamples ()
  "total number of samples per search"
  (let ((total 0))
    (maphash (lambda (key stat) 
               (declare (ignore key))
               (incf total (stat-count stat)))
             *play-stats*)
    total))

(defun mk-commit-select (sampling-select)
  "sample actions, choose the one with the best average"
  (labels ((commit-select (state)
             (funcall *trace-state* state)
             (let ((isamples -1))
               ;; gather playing statistics
               (loop while (ecase *sample-count*
                             (:static (< (incf isamples) *nsamples*))
                             (:dynamic (< (state-nsamples state) *nsamples*)))
                  do (multiple-value-bind (leg sampling-select)
                         (funcall sampling-select state)
                       (update-stats 
                        state leg
                        (play (next-state state leg) sampling-select))))
               
               ;; extract statistics
               (let ((stats (get-stats state))
                     (best-leg nil)
                     (best-avg +into-cost+))

                 ;; select best action
                 (dolist (leg +legs+
                          (values best-leg #'commit-select))
                   (let ((avg (stat-avg (aref stats leg))))
                     (when (< avg best-avg) 
                       (setf best-leg leg
                             best-avg avg))))))))
      #'commit-select))

(defun reach-goal-state (initial-state select 
                         &key
                         ((:size *size*) *size*)
                         ((:nsamples *nsamples*) *nsamples*)
                         ((:trace-state *trace-state*) *trace-state*))
  "reaches the goal state from the initial state,
   returns the path cost"
  (let ((*play-stats* (make-hash-table :test #'eql))
        (*play-stat-vectors* (make-hash-table :test #'eql)))
    (values
     (play initial-state (mk-commit-select select))
     (stats-nsamples))))

;; Selectors

(defun rnd (state)
  "select random leg"
  (loop (let ((leg (random +ndirs+)))
          (unless (bad-leg-p state leg)
            (return leg)))))

(defun u*b (state fun alpha)
  "U*B selection: min (avg-Cp*sqrt(fun (n) / ni))"
  (let* ((state-stats (get-stats state))
         (Cp-root-fun-n
          (* alpha (Cp state)
             (sqrt (funcall fun (max 1.0 (reduce #'+ state-stats
                                                 :key #'stat-count))))))
         (best-leg nil)
         (best-cost +into-cost+))
    (dolist (leg +legs+ best-leg)
      (unless (bad-leg-p state leg)
        (when (zerop (stat-count (aref state-stats leg)))
          (return leg))
        (let ((cost (- (stat-avg (aref state-stats leg))
                       (/ Cp-root-fun-n
                          (sqrt (stat-count (aref state-stats leg)))))))
          (when (< cost best-cost)
            (setf best-leg leg
                  best-cost cost)))))))

(defun ucb (state) (u*b state #'log 1.0))
(defun uqb (state) (u*b state #'sqrt 0.5))

(defun grd (state)
  "0.5-greedy selection"
  (let* ((state-stats (get-stats state))
         (k (reduce #'+ +legs+
                    :key (lambda (leg) (if (bad-leg-p state leg) 0.0 1.0))))
         (best-leg nil)
         (best-cost +into-cost+))
    (dolist (leg +legs+
             (if (> (random 1.0) (* 0.5 (/ k (- k 1.0d0))))
                 best-leg
                 (rnd state)))
      (unless (bad-leg-p state leg)
        (when (zerop (stat-count (aref state-stats leg)))
          (return leg))
        (let ((cost (stat-avg (aref state-stats leg))))
          (when (< cost best-cost)
            (setf best-leg leg
                  best-cost cost)))))))

;; Adaptive selectors

;; Uniform random sampling
(defun rnd-select (state)
  (values (rnd state) #'rnd-select))

;; UCT (always UCB)
(defun uct-select (state)
  (values (ucb state) #'uct-select))

;; RCT (Random once, then UCB)
(defun rct-select (state)
  (values (rnd state) #'uct-select))

;; GCT (Greedy once, than UCB)

(defun gct-select (switch)
  (values (grd switch) #'uct-select))

;; QCT (UQB once, than UCT)

(defun qct-select (switch)
  (values (uqb switch) #'uct-select))

;; UQT (always UQB)

(defun uqt-select (switch)
  (values (uqb switch) #'uqt-select))

;; Testing

(defun test-agent ()
  (format *error-output* "~&No tests for sailing-agent~%"))