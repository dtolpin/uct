(in-package "SAILING")

;;; Sailing Agent

(defvar *nsamples* 8
  "number of playouts per state")
(defvar *uct-exploration-factor* 0.25
  "the greater the factor, the more exploratory is UCT")

(defconstant +max-manhattans+ 4)

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
  (* (+ +in-cost+ +delay-cost+) 
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
          (play (next-state state leg) select
                (1- time-left)))))))

(defconstant +infinite-time-left+ -1
  "actual search continues infinitely")

;; Sampling statistics
(defvar *play-stats* nil
  "playout statistics")

(defun update-stats (state leg cost)
  "updates statistics for the given (state, leg) combination"
  cost)

(defun get-stat (state leg)
  "returns statistics for the given (state,leg) combination"
  )

(defun stat-avg (stat)
  "returns average cost"
  0.0)

(defparameter +legs+
  (loop for leg below +ndirs+ collect leg)
  "a list of leg indices, useful for iterations")

(defun mk-commit-select (sampling-select)
  "sample actions, choose the one with the best average"
  (labels ((commit-select (state)
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
                     (best-avg most-positive-single-float))

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
                         (trace #'identity)) ; callback to trace the path
  "reaches the goal state from the initial state,
   returns the path cost"
  (let ((*play-stats* (make-hash-table :test #'equal)))
    (play initial-state (mk-commit-select select) +infinite-time-left+)))

;; Selectors





