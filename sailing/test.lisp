(in-package "SAILING")

(defmacro run-test (test-name)
  `(progn 
     (format *error-output* " ~(~A~)" ',test-name)
     (clear-output *error-output*)
     (,test-name)
     (format *error-output* " (ok)")
     (clear-output *error-output*)))

(defun test ()
  (format *error-output* "Testing ~A:" (package-name (symbol-package 'test)))
  (run-test test-model)
  (format *error-output* "~%") (clear-output *error-output*))

(eval-when (:load-toplevel :execute)
  (test))