(defpackage "SAILEXP"
  (:documentation "Experiments with the Sailing Strategies")
  (:use "COMMON-LISP" "COMMON-LISP-USER" "SAILING")
  (:import-from "SAILING"
                "*UCT-EXPLORATION-FACTOR*"
                "RANDOM-SELECT"
                "UCT-SELECT"
                "UVT-SELECT"
                "VCT-SELECT"
                "CRT-SELECT"
                "VRT-SELECT")
  (:export "EXPER"
           "EXP0"
           "EXP1"
           ; re-export from sailing
           "*UCT-EXPLORATION-FACTOR*"
           "RANDOM-SELECT"
           "UCT-SELECT"
           "UVT-SELECT"
           "VCT-SELECT"
           "CRT-SELECT"
           "VRT-SELECT"))
(in-package "SAILEXP")

(defun exper (nr ns size select)
  (float (/ (loop repeat nr 
               sum (reach-goal-state
                    (make-initial-state :wind 0 :ptack 1)
                    select
                    :nsamples ns
                    :size size))
            nr)))

(defun exp0 (&key (nruns 5000) (nsamples 100) (size 5))
  (format t "~&~%~{~,8T~A~}~%" '(factor random uct vct vrt crt))
  (do ((*uct-exploration-factor* 0.25 (* 1.5 *uct-exploration-factor*)))
      ((> *uct-exploration-factor* 2.0))
    (format t "~,8T~5F" *uct-exploration-factor*)
    (dolist (select (list #'random-select 
                          #'uct-select
                          #'vct-select
                          #'vrt-select
                          #'crt-select))
             (format t "~,8T~5F" (exper nruns nsamples size select)))
    (format t "~%")))

(defun exp1 (&key (nruns 5000) (size 5))
  (do ((nsamples 50 (round (* 1.3 nsamples))))
      ((> nsamples 500))
    (format t "~&~%[~A samples]~%" nsamples)
    (exp0 :nruns nruns :nsamples nsamples :size size)))