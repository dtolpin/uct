(in-package "SAILING")

;; Lake and boat model

;; the map size, 
;;   the initial position is (1 . 1) 
;;   the final position is (*size* . *size*)
(defvar *size* 4)

(defconstant +delay-cost+ 3)
(defconstant +away-cost+ 1)
(defconstant +down-cost+ 2)
(defconstant +cross-cost+ 3)
(defconstant +up-cost+ 4)
(defconstant +in-cost+ 1024)
(defconstant +same-wind-prob+ 0.4)
(defconstant +adj-wind-prob+ (/ (- 1.0 +same-wind-prob+) 2.0))

(defconstant +ndirs+ 8)
(deftype direction () `(integer 0 ,+ndirs+))
(deftype tack () '(integer -1 1))

;; Vector operations to generate transition and cost matrices

(defun xprod (xa ya xb yb)
  "cross product"
  (- (* xa yb) (* xb ya)))

(defun sprod (xa ya xb yb)
  "scalar product"
  (+ (* xa xb) (* ya yb)))

(defun cprod (xa ya xb yb)
  "cosinus product"
  (/ (sprod xa ya xb yb)
     (sqrt (* (sprod xa ya xa ya) (sprod xb yb xb yb)))))

(defparameter +directions+
  ; I can write a function that generates the vector
  ; automatically, but such function would clutter the
  ; code and obscur understanding
  #(( 1 .  0) ( 1 .  1)   ; E NE
    ( 0 .  1) (-1 .  1)   ; N NW
    (-1 .  0) (-1 . -1)   ; W SW
    ( 0 . -1) ( 1 . -1))  ; S SE
  "a vector of direction vectors")

(defun dir-x (d) (first d))
(defun dir-y (d) (rest d))

(defparameter +tacks+ 
  (let ((tacks (make-array `(,+ndirs+ ,+ndirs+)
                           :element-type 'tack :initial-element 0)))
    (dotimes (l +ndirs+ tacks)
        (dotimes (w +ndirs+)
          (setf (aref tacks l w) 
                (signum (- (* (dir-x (aref +directions+ l))
                (dir-y (aref +directions+ w)))
             (* (dir-x (aref +directions+ w))
                (dir-y (aref +directions+ l)))))))))
  "tacks matrix: leg x wind -> (0, 1, -1)")

(defun opposite-tacks-p (ta tb)
  "true if tacks are opposite"
  (ecase ta
    (0 nil)
    (1 (= tb -1))
    (-1 (= tb 1))))

(defparameter +costs+ 
  (let ((costs (make-array `(,+ndirs+ ,+ndirs+)
                           :element-type 'float :initial-element 0.0)))
    (dotimes (l +ndirs+ costs)
      (dotimes (w +ndirs+)
        (let ((c (cprod (dir-x (aref +directions+ l))
                        (dir-y (aref +directions+ l))
                        (dir-x (aref +directions+ w))
                        (dir-y (aref +directions+ w)))))
          (setf (aref costs l w)
                (cond
                  ((> c 0.9) +away-cost+)
                  ((> c 0.5) +down-cost+)
                  ((> c -0.5) +cross-cost+)
                  ((> c -0.9) +up-cost+)
                  (t +in-cost+)))))))
  "costs of direction-wind combinations, not including delay cost")

(defparameter +wind-transitions+
  (let ((trans (make-array `(,+ndirs+ ,+ndirs+)
                           :element-type 'float :initial-element 0.0)))
    (dotimes (i +ndirs+ trans)
      (dotimes (j +ndirs+)
        (let ((c (cprod (dir-x (aref +directions+ i))
                        (dir-y (aref +directions+ i))
                        (dir-x (aref +directions+ j))
                        (dir-y (aref +directions+ j)))))
          (setf (aref trans i j)
                (cond
                  ((> c 0.9) +same-wind-prob+)
                  ((> c 0.5) +adj-wind-prob+)
                  (t 0.0)))))))
  "wind transition probabilities")

(defstruct state
  "world state"
  (x 1 :type fixnum)             ; boat position
  (y 1 :type fixnum)        
  (ptack 0 :type tack)           ; previous boat tack
  (pleg 0 :type direction)       ; previous boat leg
  (wind 0 :type direction))      ; current wind direction

(defun make-initial-state (&key 
                           (ptack (1- (random 3)))
                           (pleg (random +ndirs+))
                           (wind (random +ndirs+)))
  "makes an initial state, ptack, pleg and wind
   are initialized randomly if unspecified"
  (make-state :ptack ptack :pleg pleg :wind wind))

(defun goal-state-p (state)
  "tests whether the goal state is reached"
  (and (= (state-x state) *size*) (= (state-y state) *size*)))

(defun leg-cost (state leg)
  "returns actio cost for the action in the state"
  (with-slots (x y ptack wind) (the state state)
    (+ (if (opposite-tacks-p ptack (aref +tacks+ leg wind))
           +delay-cost+ 0)
       (aref +costs+ leg wind))))

(defun next-coord (z dz)
  "returns the next coordinate, stops at lake banks"
  (min *size* (max 1 (+ z dz))))

(defun next-wind (wind)
  "randomly selects the next wind direction"
  (let ((r (random 1.0)))
    (dotimes (next-wind +ndirs+ wind)
      (decf r (aref +wind-transitions+ wind next-wind))
      (if (< r 0.0) (return next-wind)))))

(defun next-state (state leg)
  "returns the new state after following `leg' in `state'"
  (with-slots (x y ptack pleg wind) (the state state)
    (values
     (make-state :x (next-coord x (dir-x (aref +directions+ leg)))
                 :y (next-coord y (dir-y (aref +directions+ leg)))
                 :ptack (aref +tacks+ leg wind)
                 :pleg leg
                 :wind (next-wind wind))
     (leg-cost state leg))))

;; Testing

(defun test-tack ()
  (assert (= 0 (aref +tacks+ 2 2)))
  (assert (= 1 (aref +tacks+ 0 1)))
  (assert (= -1 (aref +tacks+ 1 0))))

(defun test-cost ()
  (assert (= +away-cost+ (aref +costs+ 3 3)))
  (assert (= +cross-cost+ (aref +costs+ 4 2)))
  (assert (= +up-cost+ (aref +costs+ 3 0)))
  (assert (= +down-cost+ (aref +costs+ 7 6)))
  (assert (= +in-cost+ (aref +costs+ 0 4))))

(defun test-coord ()
  (assert (= (next-coord *size* 1) *size*))
  (assert (= (next-coord 1 -1) 1))
  (assert (= (next-coord 1 1) 2)))

(defun test-wind ()
  (assert (> (aref +wind-transitions+ 3 4) 0.0))
  (assert (= (aref +wind-transitions+ 1 5) 0.0)))
          
(defun test-model ()
  (test-tack)
  (test-cost)
  (test-coord)
  (test-wind))
