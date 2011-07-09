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

(defparameter *print-counts* nil
  "whether to print total sample counts")

(defun exper (nr ns size select)
  (let ((cost-sum 0)
        (nsamples-sum 0))
    (loop repeat nr 
       do (multiple-value-bind (cost nsamples)
              (reach-goal-state (make-initial-state)
                                select :nsamples ns :size size)
            (incf cost-sum cost)
            (incf nsamples-sum nsamples))
       finally (return (values (float (/ cost-sum nr))
                               (float (/ nsamples-sum nr)))))))

(defparameter *selectors* '(rnd rct uct gct qct)
  "list of selectors to compare")

(defun exp0 (&key (nruns 5000) (nsamples 100) (size 5)
             (min-ef 1.0) (ef-step 2.0) (max-ef 4.0))
  (if *print-counts* 
      (format t "~&~,8TFACTOR~{~,8TR_~A~,8TN_~:*~A~}~%" *selectors*)
      (format t "~&~{~,8T~A~}~%" (cons 'factor *selectors*)))
  (do ((*uct-exploration-factor* min-ef (* ef-step *uct-exploration-factor*)))
      ((> *uct-exploration-factor* max-ef))
    (format t "~,8T~5F" *uct-exploration-factor*)
    (dolist (selector *selectors*)
      (let ((select (symbol-function
                     (intern (concatenate 'string (string selector) "-SELECT")))))

        (if *print-counts*
            (format t "~{~,8T~5F~}" (multiple-value-list 
                                     (exper nruns nsamples size select)))
            (format t "~,8T~5F" (exper nruns nsamples size select)))
                    
        (clear-output *standard-output*)))
    (format t "~%")))

(defun exp1 (&key (size 5) (nruns 5000)
             (minsamples (* 4 size)) (samples-step (sqrt 2))
             (maxsamples (* 200 size))
             (min-ef 1.0) (ef-step 2.0) (max-ef 4.0))
  (do ((nsamples minsamples (round (* samples-step nsamples))))
      ((> nsamples maxsamples))
    (format t "~&~%[~A samples]~%" nsamples)
    (exp0 :nruns nruns :nsamples nsamples :size size
          :min-ef min-ef :ef-step ef-step :max-ef max-ef)))