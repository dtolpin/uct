(defun foo (m n)
  (dotimes (i m)
    (format t "~d ~f~%"
            i (/ (float (loop for j below n
                             sum (if (> (random 1.0) (float (/ i m 10000.0))) 0.0 1.0)))
                 n)))))
