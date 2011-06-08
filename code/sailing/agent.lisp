(in-package "SAILING")

;;; Sailing Agent

(defvar *nsamples* 8
  "number of playouts per state")
(defvar *uct-exploration-factor* 1.0
  "the greater the factor, the more exploratory is UCT")
(defvar *trace-state* #'identity
  "function called on each state")

(defconstant +max-manhattans+ 2)

;; Helpers

;; legs are shuffled to avoid dependency on direction order
(defun shuffled-legs ()
  "generates a shuffled list of legs"
  (sort (copy-seq +legs+)
        #'> :key (lambda (x) (declare (ignore x)) (random 1.0))))

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

(defvar *play-stats* nil
  "playout statistics")

(defun stat-key (state leg)
  "key of entry in stats"
  (+ (state-ptack state)
     (* 3
        (+ (state-wind state)
           (* +ndirs+ 
              (+ leg 
                 (* +ndirs+ 
                    (+ (state-x state)
                       (* *size* (state-y state))))))))))

(defun get-stat (state leg)
  "returns statistics for the given (state,leg) combination"
  (let ((key (stat-key state leg)))
    (or (gethash key *play-stats*)
        (setf (gethash key *play-stats*) (make-stat)))))

(defun update-stats (state leg cost)
  "updates statistics for the given (state, leg) combination"
  (let ((stat (get-stat state leg)))
    (incf (stat-count stat))
    (incf (stat-sum stat) cost))
  cost)

(defun stat-avg (stat)
  "returns average cost"
  (if (plusp (stat-count stat))
      (/ (stat-sum stat) (stat-count stat))
      +into-cost+))

(defun mk-commit-select (sampling-select)
  "sample actions, choose the one with the best average"
  (labels ((commit-select (state)
             (funcall *trace-state* state)
             (let ((time-left (max-playout-time state)))
               ;; gather playing statistics
               (dotimes (i *nsamples*)
                 (multiple-value-bind (leg sampling-select)
                     (funcall sampling-select state)
                   (update-stats 
                    state leg
                    (play (next-state state leg) sampling-select
                          time-left))))
               
               ;; extract statistics
               (let ((stats (map 'vector (lambda (leg) (get-stat state leg)) +legs+))
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
  (let ((*play-stats* (make-hash-table :test #'equal)))
    (play initial-state (mk-commit-select select) (max-playout-time initial-state))))

;; Selectors

(defun random-select (state)
  (values (loop (let ((leg (random +ndirs+)))
                  (unless (into-wind-p state leg)
                    (return leg))))
          #'random-select))

;; Bandit-based functions

(defun ucb (state)
  "UCB selection: min (avg-Cp*sqrt(log (n) / ni))"
  (let* ((state-stats (map 'vector (lambda (leg) (get-stat state leg))
                           +legs+))
         (avgs (map 'vector #'stat-avg state-stats))
         (Cp-root-log-n
          (* (Cp state) (sqrt (reduce #'+ state-stats :key #'stat-count))))
         (best-leg nil)
         (best-cost +into-cost+))
    (dolist (leg (shuffled-legs) best-leg)
      (let ((cost (cond
                    ((into-wind-p state leg) +into-cost+)
                    ((> (stat-count (aref state-stats leg)) 0)
                     (- (aref avgs leg)
                        (/ Cp-root-log-n (sqrt (stat-count (aref state-stats leg))))))
                    (t (- +into-cost+)))))
        (when (< cost best-cost)
          (setf best-leg leg
                best-cost cost))))))

(defun uvb (state)
  "UVB selection: max [(1-1/k)/ni for best-, 1/k/ni for rest]"
  (let* ((state-stats (map 'vector (lambda (leg) (get-stat state leg)) +legs+))
         (avgs (map 'vector #'stat-avg state-stats))
         (best-avg (reduce #'min avgs))
         (kappa (/ 1.0 +ndirs+))
         (best-leg nil)
         (best-reward -1.0))
    (dolist (leg (shuffled-legs) best-leg)
      (let ((reward (cond
                      ((into-wind-p state leg) -1.0)
                      ((> (stat-count (aref state-stats leg)) 0)
                       (/ (if (= (aref avgs leg) best-avg) (- 1.0 kappa) kappa)
                          (stat-count (aref state-stats leg))))
                      (t +1.0))))
        (when (> reward best-reward)
          (setf best-leg leg
                best-reward reward))))))

;; Adaptive selectors

;; UCT (always UCB)
(defun uct-select (state)
  (values (ucb state) #'uct-select))

;; UVB once, then UCT 
(defun vct-select (state)
  (values (uvb state) #'uct-select))

;; Always UVB
(defun uvt-select (state)
  (values (uvb state) #'uvt-select))

;; UVB once, then random
(defun vrt-select (state)
  (values (uvb state) #'random-select))

;; UCB once, then random
(defun crt-select (state)
  (values (ucb state) #'random-select))

;; Testing

(defun test-agent ()
  (format *error-output* "~&No tests for sailing-agent~%"))