(in-package "SAILING")

;;; Sailing Agent

(declaim (type (member :static :dynamic) *sample-count*))
;; static sample count is better for experiments, dynamic for deployment
(defparameter *sample-count* :static 
  "mode of counting samples: 
   :static - the number of playouts beginning in a path node is *nsamples*
   :dynamic - the number of playouts passed through a path node is *nsamples*")
(defparameter *uct-exploration-factor* 0.5
  "the greater the factor, the more exploratory is UCT")

(defvar *trace-state* #'identity
  "function called on each state")
(defvar *nsamples* 32
  "number of playouts per state")

(defconstant +max-manhattans+ 2)

;; Helpers

;; legs are shuffled to avoid dependency on direction order
(defun shuffled-legs ()
  "generates a shuffled list of legs"
  (let ((legs (copy-seq +legs+)))
    (loop for i from 7 downto 1 do
         (let ((j (random (1+ i))))
           (rotatef (nth i legs) (nth j legs)))
         finally (return legs))))

;; Stopping discipline

;;   stop an episode if more than +max-manhattans+ long
;;   and evaluate the remaining cost as manhattan into wind.

(defun max-playout-time (state)
  "stops playout after +max-manhattans+ manhattans two the goal"
  (* +max-manhattans+
     (+ (- *size* (state-x state)) (- *size* (state-y state)))))

(defun evaluate-state (state)
  "pessimistic evaluation function, so that bad moves
   are visited rarely"
  ;; manhattan into wind and delay at each step
  (* (+ +up-cost+ +delay-cost+) 
     (+ (- *size* (state-x state)) (- *size* (state-y state)))))

;; Bounding rewards
;;   the worst feasible cost is manhattan into wind,
;;   the best is away by diagonal, lower-bound it as 0
(defun Cp (state)
  "UCT factor $C_p$ in $2 C_p \sqrt{\frac {log n_i} n}$"
  (* *uct-exploration-factor* (evaluate-state state)))

;; Playing (sampling and committing)

(defun play (state select &optional time-left)
  "play in the current state and return the reward,
   actual or estimated"
  (cond
    ((goal-state-p state) 0)
    ((= time-left 0) (evaluate-state state))
    (t (multiple-value-bind (leg select)
           (funcall select state)
         (update-stats
          state leg
          (+ (leg-cost state leg)
             (play (next-state state leg) select
                   (1- time-left))))))))

(defconstant +infinite-time-left+ -1
  "actual search continues infinitely")

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
             (let ((time-left (max-playout-time state))
                   (isamples -1))
               ;; gather playing statistics
               (loop while (ecase *sample-count*
                             (:static (< (incf isamples) *nsamples*))
                             (:dynamic (< (state-nsamples state) *nsamples*)))
                  do (multiple-value-bind (leg sampling-select)
                         (funcall sampling-select state)
                       (update-stats 
                        state leg
                        (play (next-state state leg) sampling-select
                              time-left))))
               
               ;; extract statistics
               (let ((stats (get-stats state))
                     (best-leg nil)
                     (best-avg +into-cost+))

                 #+nil(format *error-output* "~S~%" (map 'list (lambda (stat) (list (stat-count stat) (stat-avg stat))) stats))

                 ;; select best action
                 (dolist (leg (shuffled-legs) (values best-leg #'commit-select))
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
     (play initial-state (mk-commit-select select) (max-playout-time initial-state))
     (stats-nsamples))))

;; Selectors

(defun rnd (state)
  "select random leg"
  (loop (let ((leg (random +ndirs+)))
          (unless (bad-leg-p state leg)
            (return leg)))))

(defun ucb (state)
  "UCB selection: min (avg-Cp*sqrt(log (n) / ni))"
  (let* ((state-stats (get-stats state))
         (Cp-root-log-n
          (* (Cp state) (sqrt (reduce #'+ state-stats :key #'stat-count))))
         (best-leg nil)
         (best-cost +into-cost+))
    (dolist (leg (shuffled-legs) best-leg)
      (let ((cost (cond
                    ((bad-leg-p state leg) +into-cost+)
                    ((plusp (stat-count (aref state-stats leg)))
                     (- (stat-avg (aref state-stats leg))
                        (/ Cp-root-log-n (sqrt (stat-count (aref state-stats leg))))))
                    (t (- +into-cost+)))))
        (when (< cost best-cost)
          (setf best-leg leg
                best-cost cost))))))

(defun uvb (state)
  "UVB selection: max [(1-1/k)/ni for best-, 1/k/ni for rest]"
  (let* ((state-stats (get-stats state))
         (best-avg (reduce #'min state-stats :key #'stat-avg))
         (kappa (/ 1.0 (reduce #'+ +legs+
                               :key (lambda (leg) (if (bad-leg-p state leg) 0.0 1.0)))))
         (best-leg nil)
         (best-reward -1.0))
    (dolist (leg (shuffled-legs) best-leg)
      (let ((reward (cond
                      ((bad-leg-p state leg) -1.0)
                      ((plusp (stat-count (aref state-stats leg)))
                       (/ (if (= (stat-avg (aref state-stats leg)) best-avg)
                              (- 1.0 kappa) kappa)
                          (stat-count (aref state-stats leg))))
                      (t +1.0))))
        (when (> reward best-reward)
          (setf best-leg leg
                best-reward reward))))))

;; Adaptive selectors

;; Uniform random sampling
(defun rnd-select (state)
  (values (rnd state) #'rnd-select))

;; UCT (always UCB)
(defun uct-select (state)
  (values (ucb state) #'uct-select))

(defun rct-select (state)
  (values (rnd state) #'uct-select))

;; UVB once, then UCT 
(defun vct-select (state)
  (values (uvb state) #'uct-select))

;; Testing

(defun test-agent ()
  (format *error-output* "~&No tests for sailing-agent~%"))