(let ((a1 0.254829592d0)
      (a2 -0.284496736d0)
      (a3 1.421413741d0)
      (a4 -1.453152027d0)
      (a5 1.061405429d0)
      (p 0.3275911d0))

  (defun erf (x)    
    "ERF, the error function, approximation according to A&S 7.1.26"
    (let* ((y (/ 1.0 (+ 1.0 (* p (abs x)))))
           (z (- 1.0 (* y (exp (- (* x x))) 
                        (+ a1 (* y (+ a2 (* y (+ a3 (* y (+ a4 (* y a5))))))))))))
      (declare (type double-float y z))
      (if (plusp x) z (- z)))))

