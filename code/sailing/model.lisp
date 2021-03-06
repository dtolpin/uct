(in-package "SAILING")

(defparameter *randomize-cost* nil)

;; Lake and boat model

;; the map size, 
;;   the initial position is (1 . 1) 
;;   the final position is (*size* . *size*)
(defvar *size* 4
  "the lake is a *size* x *size* square")

(defconstant +delay-cost+ 4.0)
(defconstant +away-cost+ 1.0)
(defconstant +down-cost+ 2.0)
(defconstant +cross-cost+ 3.0)
(defconstant +up-cost+ 4.0)
(defconstant +into-cost+ (coerce most-positive-fixnum 'double-float))
(defconstant +same-wind-prob+ 0.4)
(defconstant +adj-wind-prob+ (/ (- 1.0 +same-wind-prob+) 2.0))

;; type synonyms, mostly for documenting
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

(defun norm (x y)
  "scalar norm"
  (sqrt (sprod x y x y)))

(defun dist (xa ya xb yb)
  "distance between a and b"
  (norm (- xb xa) (- yb ya)))

(defun cprod (xa ya xb yb)
  "cosinus product"
  (/ (sprod xa ya xb yb)
     (* (norm xa ya) (norm xb yb))))

(defparameter +legs+
  (loop for leg below +ndirs+ collect leg)
  "a list of leg indices, useful for iterations")

(defparameter +dirs+
  ;; could write a function that generates the vector
  ;; automatically, but would clutter the code
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
                ;; sign of cross-product,
                ;; same sign if same tack, zero is neutral (in or away)
                (signum (xprod (dir-x (aref +dirs+ l))
                               (dir-y (aref +dirs+ l))
                               (dir-x (aref +dirs+ w))
                               (dir-y (aref +dirs+ w))))))))
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
        (let ((c (cprod (dir-x (aref +dirs+ l))
                        (dir-y (aref +dirs+ l))
                        (dir-x (aref +dirs+ w))1G
                        (dir-y (aref +dirs+ w)))))
          (setf (aref costs l w)
                (cond
                  ((> c 0.9) +away-cost+)
                  ((> c 0.5) +down-cost+)
                  ((> c -0.5) +cross-cost+)
                  ((> c -0.9) +up-cost+)
                  (t +into-cost+)))))))
  "costs of direction-wind combinations, not including delay cost")

(defparameter +wind-transitions+
  (let ((trans (make-array `(,+ndirs+ ,+ndirs+)
                           :element-type 'float :initial-element 0.0)))
    (dotimes (i +ndirs+ trans)
      (dotimes (j +ndirs+)
        (let ((c (cprod (dir-x (aref +dirs+ i))
                        (dir-y (aref +dirs+ i))
                        (dir-x (aref +dirs+ j))
                        (dir-y (aref +dirs+ j)))))
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
  (ptack 1 :type tack)           ; previous boat tack
  (wind 0 :type direction))      ; current wind direction

(defun state-leg-key (state leg)
  (declare (type fixnum leg) (type state state))
  "key of state+leg combination, for hashing and caching"
  (+ (state-ptack state)
     (* 3
        (+ (state-wind state)
           (* +ndirs+
              (+ leg 
                 (* +ndirs+
                    (+ (1- (state-x state))
                       (* *size* (1- (state-y state)))))))))))

(defun make-initial-state (&key 
                           (ptack (1- (* 2 (random 2))))
                           (wind (random +ndirs+)))
  "makes an initial state, ptack, and wind
   are initialized randomly if unspecified"
  (make-state :ptack ptack :wind wind))

(defun goal-state-p (state)
  "tests whether the goal state is reached"
  (and (= (state-x state) *size*) (= (state-y state) *size*)))

(defun into-wind-p (state leg)
  "true when the direction is into the wind"
  (and (= (dir-x (aref +dirs+ leg)) (- (dir-x (aref +dirs+ (state-wind state)))))
       (= (dir-y (aref +dirs+ leg)) (- (dir-y (aref +dirs+ (state-wind state)))))))

(defun into-shore-p (state leg)
  "true when the leg will throw the boat out of the water"
  (let ((next-x (+ (state-x state) (dir-x (aref +dirs+ leg))))
        (next-y (+ (state-y state) (dir-y (aref +dirs+ leg)))))
    (or (< next-x 1) (> next-x *size*)
        (< next-y 1) (> next-y *size*))))

(defun bad-leg-p (state leg)
  "true when the leg cannot be chosen in the state"
  (or (into-wind-p state leg) (into-shore-p state leg)))

(defun leg-cost (state leg)
  "returns action cost for the action in the state"
  (let ((cost (with-slots (x y ptack wind) (the state state)
                (+ (if (opposite-tacks-p ptack (aref +tacks+ leg wind))
                       +delay-cost+ 0)
                   (* (norm (dir-x (aref +dirs+ leg))
                            (dir-y (aref +dirs+ leg)))
                      (aref +costs+ leg wind))))))
    (if *randomize-cost* (* (+ 0.5 (random 1.0)) cost) cost)))

(defun next-coord (z dz)
  "returns the next coordinate, stops at lake shores"
  (min *size* (max 1 (+ z dz))))

(defun next-wind (wind)
  "randomly selects the next wind direction"
  (let ((r (random 1.0)))
    (dotimes (next-wind +ndirs+ wind)
      ;; roulette wheel on wind probabilities
      (decf r (aref +wind-transitions+ wind next-wind))
      (if (< r 0.0) (return next-wind)))))

(defun next-state (state leg)
  "returns the new state after following `leg' in `state'"
  (with-slots (x y ptack wind) (the state state)
    (make-state :x (next-coord x (dir-x (aref +dirs+ leg)))
                :y (next-coord y (dir-y (aref +dirs+ leg)))
                :ptack (if (zerop (aref +tacks+ leg wind))
                           ptack (aref +tacks+ leg wind))
                :wind (next-wind wind))))

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
  (assert (eql +into-cost+ (aref +costs+ 0 4))))

(defun test-coord ()
  (assert (= (next-coord *size* 1) *size*))
  (assert (= (next-coord 1 -1) 1))
  (assert (= (next-coord 1 1) 2)))

(defun test-wind ()
  (assert (> (aref +wind-transitions+ 3 4) 0.0))
  (assert (= (aref +wind-transitions+ 1 5) 0.0)))

(defun test-bad-leg ()
  (assert (bad-leg-p (make-state :x 1 :y 1) 6))
  (assert (bad-leg-p (make-state :x 1 :y 1) 4))
  (assert (bad-leg-p (make-state :x 1 :y 1 :wind 1) 5)))
          
(defun test-model ()
  (test-tack)
  (test-cost)
  (test-coord)
  (test-wind)
  (test-bad-leg))
