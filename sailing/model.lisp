(in-package "SAILING")

;; the map size, 
;;   the initial position is (1 . 1) 
;;   the final position is (*size* . *size*)
(defvar *size* 2)

(deftype direction () '(integer 0 7))

(defparameter +directions+
  #(( 1 .  0) ( 1 .  1)   ; E NE
    ( 0 .  1) (-1 .  1)   ; N NW
    (-1 .  0) (-1 . -1)   ; W SW
    ( 0 . -1) ( 1 . -1))) ; S SE

(defstruct state
  (x 1) (y 1)                    ; boat position
  (wind 0 :type direction))      ; wind direction

(defun action-cost (state action)
  "returns actio cost for the action in the state"
  0)

(defun next-state (state action)
  "returns the new state after performing 
   the action in the state"
  (let ((new-state state)
        (cost (action-cost state action)))
    (values new-state cost)))

(defun test-model ()
  (format *error-output* "~&*** no tests for sailing model ***~%"))

  
