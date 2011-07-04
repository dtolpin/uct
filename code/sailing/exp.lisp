(defpackage "SAILEXP"
  (:documentation "Experiments with the Sailing Strategies")
  (:use "COMMON-LISP" "COMMON-LISP-USER" "SAILING")
  (:export "EXPER"
           "EXP0"
           "EXP1"
           ;; re-export from sailing
           "REACH-GOAL-STATE"
           "MAKE-INITIAL-STATE"
           "*UCT-EXPLORATION-FACTOR*"
           "*SAMPLE-COUNT*"
           "RND-SELECT"
           "UCT-SELECT"
           "GCT-SELECT"
           "QCT-SELECT"
           "UQT-SELECT"
           "RCT-SELECT"))
(in-package "SAILEXP")

(defun exper (nr ns size select)
  (let (cost nsamples)
    (loop repeat nr 
       do (multiple-value-setq (cost nsamples)
            (reach-goal-state (make-initial-state)
                              select :nsamples ns :size size))
       sum cost into cost-sum
       sum nsamples into nsamples-sum
       finally (return (values (float (/ cost-sum nr))
                               (round (/ nsamples-sum nr)))))))


(defparameter +selectors+ '(rnd rct uct gct qct)
  "list of selectors to compare")

(defun exp0 (&key (nruns 5000) (nsamples 100) (size 5)
             (min-ef 1.0) (ef-step 2.0) (max-ef 4.0))
  (format t "~&~%~{~,8T~A~}~%" (cons 'factor +selectors+))
  (do ((*uct-exploration-factor* min-ef (* ef-step *uct-exploration-factor*)))
      ((> *uct-exploration-factor* max-ef))
    (format t "~,8T~5F" *uct-exploration-factor*)
    (dolist (selector +selectors+)
      (let ((select (symbol-function
                     (intern (concatenate 'string (string selector) "-SELECT")))))
        (format t "~,8T~5F" (exper nruns nsamples size select))
        (clear-output *standard-output*)))
    (format t "~%")))

(defun exp1 (&key (size 5) (nruns 5000) (minsamples 20)
             (min-ef 1.0) (ef-step 2.0) (max-ef 4.0))
  (do ((nsamples minsamples (round (* (sqrt 2) nsamples))))
      ((> nsamples (* 200 size)))
    (format t "~&~%[~A samples]~%" nsamples)
    (exp0 :nruns nruns :nsamples nsamples :size size
          :min-ef min-ef :ef-step ef-step :max-ef max-ef)))