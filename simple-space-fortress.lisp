;; Author: Shawn Betts

(defpackage #:simple-space-fortress
  (:use :cl)
  (:shadow #:time #:speed)
  (:nicknames #:ssf)
  (:export #:get-autoturn-step-function #:get-explode-step-function))

(in-package #:ssf)

(defparameter *default-data-dir* (let ((base (or #.*compile-file-pathname* *load-pathname*)))
                                   (make-pathname :name nil
                                                  :type nil
                                                  :directory (append (pathname-directory base) (list "data"))
                                                  :defaults base))
  "By default, save our data files in the same directory as this file.")

(defclass point ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defun v (x y)
  (make-instance 'point :x x :y y))

(defun deg (radians)
  (* radians (/ 180 pi)))

(defun rad (degrees)
  (* degrees (/ pi 180)))

(defun copy-point (p)
  (make-instance 'point :x (x p) :y (y p)))

(defun add-points (v1 v2)
  (incf (x v1) (x v2))
  (incf (y v1) (y v2)))

(defun add-cartesian (v1 v2)
  (incf (x v1) (x v2))
  (decf (y v1) (y v2)))

(defun add-magnitude (v1 angle magnitude)
  (incf (x v1) (* (cos (rad angle)) magnitude))
  (incf (y v1) (* (sin (rad angle)) magnitude)))

(defun angle-to (from to)
  ;; XXX: ccl will only return a double-float if at least one argument
  ;; is a double-float.
  (mod (deg (atan (coerce (- (- (y to) (y from))) 'double-float)
                  (coerce (- (x to) (x from)) 'double-float)))
       360))

(defun dot-product (v1 v2)
  (+ (* (x v1) (x v2)) (* (y v1) (y v2))))

(defun distance (p1 p2)
  (sqrt (+ (expt (- (x p1) (x p2)) 2)
           (expt (- (y p1) (y p2)) 2))))

(defun norm (p)
  (sqrt (+ (expt (x p) 2)
           (expt (y p) 2))))

(defgeneric reset (obj))

(defclass object ()
  ((location :initform (make-instance 'point) :initarg :location :accessor location)
   (velocity :initform (make-instance 'point) :initarg :velocity :accessor velocity)
   (angle :initform 0 :initarg :angle :accessor angle)
   (start-location :initarg :start-location :initform (make-instance 'point) :accessor start-location)
   (start-velocity :initarg :start-velocity :initform (make-instance 'point) :accessor start-velocity)
   (start-angle :initarg :start-angle :initform 0 :accessor start-angle)
   (collision-radius :initform 0 :initarg :collision-radius :accessor collision-radius)
   (alive :initform t :accessor alive)
   (respawn :initform 0 :accessor respawn)))

(defmethod reset ((obj object))
  (setf (location obj) (copy-point (start-location obj))
        (velocity obj) (copy-point (start-velocity obj))
        (angle obj) (start-angle obj)
        (alive obj) t))

(defun collide-p (o1 o2)
  (<= (distance (location o1) (location o2))
      (+ (collision-radius o1)
         (collision-radius o2))))

(defclass hexagon ()
  ((points :initarg :points :accessor points)))

(defun make-hexagon (radius)
  (let ((x1 (truncate (- 355 radius)))
        (x2 (truncate (- 355 (/ radius 2))))
        (x3 (truncate (+ 355 (/ radius 2))))
        (x4 (truncate (+ 355 radius)))
        (y1 315)
        (y2 (truncate (- 315 (* radius (sin (* pi 2/3))))))
        (y3 (truncate (+ 315 (* radius (sin (* pi 2/3)))))))
    (make-instance 'hexagon :points (list (v x1 y1)
                                          (v x2 y2)
                                          (v x3 y2)
                                          (v x4 y1)
                                          (v x3 y3)
                                          (v x2 y3)))))

(defun inside-hexagon (hexagon point)
  (labels ((outside (p1 p2)
             ;; FIXME: excessive consing
             (let ((normal (make-instance 'point :x (- (y p1) (y p2)) :y (- (x p2) (x p1))))
                   (diff (make-instance 'point :x (- (x point) (x p1)) :y (- (y point) (y p1)))))
               (< (dot-product normal diff) 0))))
    (not (loop for i on '(0 1 2 3 4 5)
               for p1 = (first i)
               for p2 = (or (second i) 0)
               thereis (outside (elt (points hexagon) p1) (elt (points hexagon) p2))))))

(defclass ship (object)
  ((acceleration :initarg :acceleration :reader acceleration)
   (turn-speed :initarg :turn-speed :reader turn-speed)
   (max-velocity :initarg :max-velocity :reader max-velocity)
   (hit-points :initarg :hit-points))
  (:default-initargs
   :turn-speed 6
   :acceleration 0.3D0
   :hit-points 10
   :max-velocity 20
   :start-location (v 245 315)
   :start-velocity (v 0 0)
   :start-angle 90
   :collision-radius 10))

(defclass mine (object)
  ((life-span :initarg :life-span)
   (sleep-span :initarg :sleep-span)
   (num-foes :initarg :num-foes)
   (foe-letters :accessor foe-letters))
  (:default-initargs
   :alive nil
   :collision-radius 20))

;; (defmethod shared-initialize :after (obj slots &rest keys)
;;   (declare (ignore slots keys))
;;   (setf (foe-letters obj) '("A" "B" "C")))

(defclass fortress (object)
  ((sector-size :initarg :sector-size)
   (lock-time :initarg :lock-time :reader lock-time)
   (move-timer :initform 0 :accessor move-timer)
   (hit-timer :initform 0 :accessor hit-timer)
   (destroy-timer :initform 0 :accessor destroy-timer)
   (last-angle :initform 0 :accessor last-angle))
  (:default-initargs
   :start-location (v 355 315)
   :start-velocity (v 0 0)
   :start-angle 180
   :sector-size 10
   :lock-time 1000
   :collision-radius 18))

(defclass missile (object)
  ()
  (:default-initargs
   :collision-radius 5))

(defclass shell (object)
  ()
  (:default-initargs
   :collision-radius 3))

(defclass score ()
  ((points :initform 0 :accessor points)
   (raw-points :initform 0 :accessor raw-points)
   (control :initform 0 :accessor control)
   (velocity :initform 0 :accessor velocity)
   (speed :initform 0 :accessor speed)
   (interval :initform 0 :accessor interval)
   (shots :initform "inf" :accessor shots)
   (vulnerability :initform 0 :accessor vulnerability)))

(defclass keys ()
  ((thrust :initform nil :accessor thrust)
   (left :initform nil :accessor left)
   (right :initform nil :accessor right)
   (fire :initform nil :accessor fire)
   (iff :initform nil :accessor iff)
   (fired :initform nil :accessor fired)))

(defun clear-keys (keys)
  (setf (thrust keys) nil
        (left keys) nil
        (right keys) nil
        (fire keys) nil
        (iff keys) nil))

(defun set-keys (keys k)
  (clear-keys keys)
  (setf (thrust keys) (and (find :thrust k) t)
        (left keys) (and (find :left k) t)
        (right keys) (and (find :right k) t)
        (fire keys) (and (find :fire k) t)))

(defun get-keys (keys)
  (nconc (when (thrust keys) (list :thrust))
         (when (left keys) (list :left))
         (when (right keys) (list :right))
         (when (fire keys) (list :fire))))

(defclass game ()
  ((world :initform (v 710 626))
   (tick :initform 0 :reader tick)
   (time :initform 0 :reader time)
   (keys :initform (make-instance 'keys) :reader keys)
   (ship :initarg :ship :reader ship)
   (missiles :initform nil :reader missiles)
   (shells :initform nil :reader shells)
   (mine :initarg :mine :reader mine)
   (fortress :initarg :fortress :reader fortress)
   (score :initform (make-instance 'score) :reader score)
   (score-update-timer :initform 0)
   (small-hex :initarg :small-hex :reader small-hex)
   (big-hex :initarg :big-hex :reader big-hex)
   (reset-fortress :initform nil :accessor reset-fortress)
   (collisions :initform nil :accessor collisions)
   (events :initform nil :accessor events)
   ;;
   (tick-time :initarg :tick-time)
   (tick-list :initarg :tick-list)
   (max-time :initarg :max-time)
   (score-update-interval :initarg :score-update-interval)
   (wrap-penalty :initarg :wrap-penalty)
   (vulnerability-time :initarg :vulnerability-time)
   (vulnerability-threshold :initarg :vulnerability-threshold)
   (shell-speed :initarg :shell-speed)
   (missile-speed :initarg :missile-speed)
   (auto-turn :initarg :auto-turn)
   (hex-explode :initarg :hex-explode)
   (missile-penalty :initarg :missile-penalty)
   (ship-death-penalty :initarg :ship-death-penalty)
   (destroy-fortress-points :initarg :destroy-fortress-points)
   (control-increment :initarg :control-increment)
   (velocity-increment :initarg :velocity-increment)
   (min-speed-threshold :initarg :min-speed-threshold)
   (max-speed-threshold :initarg :max-speed-threshold)
   (negative-points :initarg :negative-points))
  (:default-initargs
   :tick-time 33
   ;; Use this stuff a preset list of game tick delays at the
   ;; beginning of the game. Used when playing back a .key file.
   :tick-list nil
   :max-time (* 3 60 1000)
   :score-update-interval 1000
   :small-hex (make-hexagon 40)
   :big-hex (make-hexagon 200)
   :wrap-penalty 10
   :negative-points :allowed
   :missile-penalty 0
   :ship-death-penalty 100
   :destroy-fortress-points 100
   :vulnerability-time 250
   :vulnerability-threshold 10
   :min-speed-threshold 0.9D0
   :max-speed-threshold 4
   :velocity-increment 7
   :control-increment 6
   :shell-speed 6
   :missile-speed 20
   :auto-turn nil
   :hex-explode nil))

(defun dump-key-changes-header (stream)
  (write-line "# generated with simple-space-fortress.lisp" stream))

(defun dump-key-changes (delta before after stream)
  (let ((release (set-difference before after))
        (press (set-difference after before))
        (codes '(:fire 32 :thrust 119 :right 100 :left 97)))
    (format stream "[~D, [~{[~D,~D,0]~^,~}]]~%"
            delta
            (nconc
             (mapcan (lambda (k) (list 2 (getf codes k))) press)
             (mapcan (lambda (k) (list 3 (getf codes k))) release)))))

(defun dump-game-state-header (game stream)
  (with-slots (mine) game
    (write-line "# log version 1.5" stream)
    (write-line "# generated with simple-space-fortress.lisp" stream)
    ;; (format stream "# Foe mines:~{ ~A~}~%" (when mine (slot-value mine 'foe-letters)))
    ;; (format stream "# Game condition (Mines Fortress): ~A ~A~%"
    ;;         (if (slot-value game 'mine) "True" "False")
    ;;         (if (slot-value game 'fortress) "True" "False"))
    (write-line "# non-hashed line notation:" stream)
    (write-line "# game_clock system_clock game_time ship_alive? ship_x ship_y ship_vel_x ship_vel_y ship_orientation mine_alive? mine_x mine_y fortress_alive? fortress_orientation [missile_x missile_y missile_orientation ...] [shell_x shell_y shell_orientation ...] bonus_symbol pnts cntrl vlcty vlner iff intervl speed shots thrust_key left_key right_key fire_key iff_key shots_key pnts_key" stream)))

(defun dump-game-state (game stream)
  (with-slots (time keys ship fortress mine missiles shells score) game
    (format stream "~D ~,6f ~D" time 0.0 time)
    (if (alive ship)
        (format stream " y ~,3f ~,3f ~,3f ~,3f ~,1f"
                (x (location ship)) (y (location ship))
                (x (velocity ship)) (y (velocity ship))
                (angle ship))
        (format stream " n - - - - -"))
    (if (and mine (alive mine))
        (format stream " y ~,3f ~,3f"
                (x (location mine)) (y (location mine)))
        (format stream " n - -"))
    (if (and fortress (alive fortress))
        (format stream " y ~,1f" (angle fortress))
        (format stream " n -"))
    (format stream " [")
    (loop for m in missiles
          do (format stream "~,3f ~,3f ~,1f " (x (location m)) (y (location m)) (angle m)))
    (format stream "]")
    (format stream " [")
    (loop for s in shells
          do (format stream "~,3f ~,3f ~,1f " (x (location s)) (y (location s)) (angle s)))
    (format stream "]")
    ;; bonus
    (format stream " -")
    ;; points
    (format stream "~@{ ~A~}"
            (points score)
            (control score)
            (velocity score)
            (vulnerability score)
            "-"                         ; iff
            (interval score)
            (speed score)
            (shots score))
    (format stream "~@{ ~A~}"
            (if (thrust keys) "y" "n")
            (if (left keys) "y" "n")
            (if (right keys) "y" "n")
            (if (fire keys) "y" "n")
            (if (iff keys) "y" "n")
            "n"
            "n")
    (terpri stream)))

(defun dump-game-state-for-model (game)
  (with-slots (time ship fortress missiles shells score collisions events) game
    (list :screen-type :game
          :game "explode"
          :mode "events"
          :time time
          :ship (list :alive :x :y :vx :vy :distance-from-fortress :angle :vdir :speed :orientation)
          :fortress (list :alive :x :y :orientation)
          :bighex
          :smallhex
          :missiles
          :shells
          :rawpnts (raw-points score)
          :pnts (points score)
          :active t
          :keys
          :collisions collisions
          :events events)))

(defun dump-game-state-to-string (game)
  (with-output-to-string (s) (dump-game-state game s)))

(defun dump-game-state-footer (game stream)
  (format stream "# pnts score ~D~%" (points (score game)))
  (format stream "# total score ~D~%" (points (score game)))
  (format stream "# raw pnts ~D~%" (raw-points (score game)))
  (format stream "# bonus earned FIXME~%"))

(defun report-game-state (game)
  (labels ((calculate-vdir (o1 o2)
             (let* ((dir (deg (atan (y (velocity o1)) (x (velocity o1)))))
                    (actual (angle-to (location o1) (location o2)))
                    (diff (- dir actual)))
               (cond
                 ((> diff 180) (- diff 360))
                 ((< diff -180) (+ diff 360))
                 (t diff)))))
    (with-slots (time ship missiles shells fortress score collisions events) game
      (list
       :screen-type "game"
       :game "TODO"
       :mode :events
       :time time
       :ship (list :alive (alive ship)
                   :x (x (location ship)) :y (y (location ship))
                   :vx (x (velocity ship)) :vy (y (velocity ship))
                   :orientation (angle ship)
                   :distance-from-fortress (distance (location ship) (location fortress))
                   :angle (angle-to (location ship) (location fortress))
                   :vdir (calculate-vdir ship fortress)
                   :speed (norm (velocity ship)))
       :mine nil
       :fortress (list :alive (alive fortress)
                       :x (x (location fortress))
                       :y (y (location fortress))
                       :orientation (angle fortress))
       :missiles (loop for m in missiles
                       collect (list :x (x (location m)) :y (y (location m))
                                     :vx (x (velocity m)) :vy (y (velocity m))
                                     :orientation (angle m)))
       :shells (loop for s in shells
                     collect (list :x (x (location s)) :y (y (location s))
                                   :vx (x (velocity s)) :vy (y (velocity s))
                                   :orientation (angle s)))
       :rawpnts (raw-points score)
       :pnts (points score)
       :vlner (vulnerability score)
       ;; :bonus nil
       ;; :iff nil
       ;; :intrvl nil
       ;; :speed (speed score)
       :collisions collisions
       :events events))))

(defun report-score (game)
  (with-slots (score) game
    (list :screen-type "score"
          :mode :events
          :pnts (points score)
          :raw-pnts (raw-points score)
          :bonus "TODO"
          :total-bonus "TODO"
          :base-stats (list "TODO")
          :event-stats (list "TODO")
          :input-stats (list "TODO"))))

(defun penalize (game slot amount)
  (with-slots (negative-points) game
    (if (eq slot 'points)
        (progn
          (decf (raw-points (score game)) amount)
          (decf (points (score game)) amount)
          (when (and (eq negative-points :prohibited)
                     (minusp (points (score game))))
            (setf (points (score game)) 0)))
        (decf (slot-value (score game) slot) amount))))

(defun reward (game slot amount)
  (when (eq slot 'points)
    (incf (raw-points (score game)) amount))
  (incf (slot-value (score game) slot) amount))

(defmethod reset ((obj game))
  (with-slots (mine missiles shells ship fortress) obj
    (reset ship)
    (reset fortress)
    (setf mine nil
          missiles nil
          shells nil)))

(defun outside-game-area-p (game obj)
  (with-slots (world) game
    (with-slots (location) obj
      (or (< (x location) 0)
          (< (y location) 0)
          (>= (x location) (x world))
          (>= (y location) (y world))))))

(defun update-score (game)
  (with-slots (score score-update-timer time big-hex ship min-speed-threshold max-speed-threshold velocity-increment control-increment score-update-interval) game
    (when (> score-update-timer score-update-interval)
      (setf score-update-timer 0)
      (if (< min-speed-threshold (distance (v 0 0) (velocity ship)) max-speed-threshold)
          (reward game 'velocity velocity-increment)
          (penalize game 'velocity velocity-increment))
      (if (inside-hexagon big-hex (location ship))
          (reward game 'control control-increment)
          (reward game 'control (truncate control-increment 2))))))

(defun tick-timers (game ms)
  (with-slots (fortress score-update-timer) game
    (incf (move-timer fortress) ms)
    (incf (hit-timer fortress) ms)
    (incf (destroy-timer fortress) ms)
    (incf score-update-timer ms)))

(defun update-ship (game)
  (with-slots (ship keys missile-penalty missile-speed fortress missiles big-hex small-hex hex-explode auto-turn world collisions events) game
    (if (fire keys)
        (unless (fired keys)
          (penalize game 'points missile-penalty)
          (push :missile-fired events)
          (setf (fired keys) t
                missiles (nconc missiles
                                (list
                                 (make-instance 'missile
                                                :location (copy-point (location ship))
                                                :velocity (v (* (cos (rad (angle ship))) missile-speed)
                                                             (* (sin (rad (angle ship))) missile-speed))
                                                :angle (angle ship))))))
        (setf (fired keys) nil))
    (if auto-turn
        (setf (angle ship) (truncate (angle-to (location ship) (location fortress))))
        (progn
          (when (left keys)
            (setf (angle ship) (mod (+ (angle ship) (turn-speed ship)) 360)))
          (when (right keys)
            (setf (angle ship) (mod (- (angle ship) (turn-speed ship)) 360)))))
    (when (thrust keys)
      (add-magnitude (velocity ship) (angle ship) (acceleration ship))
      (when (> (x (velocity ship)) (max-velocity ship))
        (setf (x (velocity ship)) (max-velocity ship)))
      (when (< (x (velocity ship)) (- (max-velocity ship)))
        (setf (x (velocity ship)) (- (max-velocity ship))))
      (when (> (y (velocity ship)) (max-velocity ship))
        (setf (y (velocity ship)) (max-velocity ship)))
      (when (< (y (velocity ship)) (- (max-velocity ship)))
        (setf (y (velocity ship)) (- (max-velocity ship)))))
    (add-cartesian (location ship) (velocity ship))
    (when (> (x (location ship)) (x world))
      (setf (x (location ship)) 0))
    (when (< (x (location ship)) 0)
      (setf (x (location ship)) (x world)))
    (when (> (y (location ship)) (y world))
      (setf (y (location ship)) 0))
    (when (< (y (location ship)) 0)
      (setf (y (location ship)) (y world)))
    ;; collide
    (if hex-explode
        (when (or (inside-hexagon small-hex (location ship))
                  (not (inside-hexagon big-hex (location ship))))
          (setf (alive ship) nil)
          (cond
            ((inside-hexagon small-hex (location ship))
             (push :explode-smallhex events)
             (push :small-hex collisions))
            ((not (inside-hexagon big-hex (location ship)))
             (push :explode-bighex events)
             (push :big-hex collisions))
            (t (error "unknown hex collision"))))
        (when (inside-hexagon small-hex (location ship))
          (setf (x (location ship)) (- (x (location ship)))
                (y (location ship)) (- (y (location ship))))
          (push :small-hex collisions)))))

(defun update-fortress (game)
  (with-slots (ship fortress shell-speed shells events) game
    (with-slots (sector-size) fortress
      (when (and (not (alive fortress))
                 (> (destroy-timer fortress) 1000))
        (push :fortress-respawn events)
        (setf (alive fortress) t))
      (when (alive ship)
        (setf (angle fortress) (* (truncate (angle-to (location fortress) (location ship))
                                            sector-size)
                                  sector-size)))
      (when (/= (angle fortress) (last-angle fortress))
        (setf (last-angle fortress) (angle fortress)
              (move-timer fortress) 0))
      (when (and (>= (move-timer fortress) (lock-time fortress))
                 (alive ship)
                 (alive fortress))
        (let ((a (angle-to (location fortress)
                           (location ship))))
          (push :fortress-fired events)
          (setf shells (nconc shells
                              (list
                               (make-instance 'shell
                                              :location (copy-point (location fortress))
                                              :velocity (v (* (cos (rad a)) shell-speed)
                                                           (* (sin (rad a)) shell-speed))
                                              :angle a)))
                (move-timer fortress) 0))))))

(defun update-missiles (game)
  (with-slots (missiles fortress vulnerability-time vulnerability-threshold destroy-fortress-points score collisions events) game
    ;; XXX: when PSF.py deletes a missile from the list (because it is
    ;; no longer alive), the next missile is skipped and not
    ;; updated. Emulate this bug using SKIP.
    (loop with skip = nil
          for i from 0
          for m in missiles
          do (if skip
                 (setf skip nil)
                 (progn
                   (add-cartesian (location m) (velocity m))
                   (cond
                     ((collide-p m fortress)
                      (setf (alive m) nil)
                      (when (alive fortress)
                        (push :hit-fortress events)
                        (push :fortress collisions)
                        (if (< (hit-timer fortress) vulnerability-time)
                            (progn
                              (if (>= (vulnerability score) (1+ vulnerability-threshold))
                                  (progn
                                    (push :fortress-destroyed events)
                                    (setf (alive fortress) nil
                                          (destroy-timer fortress) 0)
                                    (reward game 'points destroy-fortress-points))
                                  (push :vlner-reset events))
                              (setf (vulnerability score) 0))
                            (progn
                              (push :vlner-increased events)
                              (incf (vulnerability score)))))
                      (setf (hit-timer fortress) 0))
                     ((outside-game-area-p game m)
                      (setf (alive m) nil)))
                   (setf skip (not (alive m))))))
    (setf missiles (delete-if-not #'alive missiles))))

(defun update-shells (game)
  (with-slots (shells ship collisions events) game
    ;; XXX: when PSF.py deletes a shell from the list (because it is
    ;; no longer alive), the next shell is skipped and not
    ;; updated. Emulate this bug using SKIP.
    (loop with skip = nil
          for i from 0
          for s in shells
          do (if skip
                 (setf skip nil)
                 (progn
                   (add-cartesian (location s) (velocity s))
                   (cond
                     ((collide-p ship s)
                      (setf (alive s) nil
                            (alive ship) nil)
                      (push :shell-hit-ship events)
                      (push :ship-destroyed events)
                      (push :ship collisions))
                     ((outside-game-area-p game s)
                      (setf (alive s) nil)))
                   (setf skip (not (alive s))))))
    (setf shells (delete-if-not #'alive shells))))

(defun step-game-one-tick (game dumper-function)
  "Step the game engine one tick. Return .dat file line."
  (with-slots (tick time tick-time tick-list fortress ship ship-death-penalty reset-fortress collisions events) game
    (let ((stuffed (and tick-list t))
          (tickms (or (pop tick-list)
                      tick-time))
          (dat-line))
      ;;
      (incf tick)
      (incf time tickms)
      ;;
      (setf collisions nil
            events nil)
      (update-score game)
      (update-ship game)
      (update-fortress game)
      (update-missiles game)
      (update-shells game)
      (setf dat-line (funcall dumper-function game))
      ;;
      (tick-timers game tickms)
      (when reset-fortress
        (setf reset-fortress nil
              (move-timer fortress) 0
              (alive fortress) t))
      ;; respawn
      (when (not (alive ship))
        (penalize game 'points ship-death-penalty)
        (reset ship)
        (setf reset-fortress t)
        ;; When playing back a keyfile, the tick delays are stuffed
        ;; from the key file, which handled the ship death delay. So
        ;; skip the 1s delay for stuffed tick delays.
        (unless stuffed
          (incf time 1000)))
      dat-line)))

(defun game-key-down (game which)
  (with-slots (keys) game
    (setf (slot-value keys which) t)))

(defun game-key-up (game which)
  (with-slots (keys) game
    (setf (slot-value keys which) nil)))

;;;;; game creation

(defun make-explode-game ()
  (make-instance 'game
                 :ship (make-instance 'ship)
                 :fortress (make-instance 'fortress)
                 :mine nil
                 :negative-points :prohibited
                 :missile-penalty 2
                 :hex-explode t))

(defun make-explode-auto-turn-game ()
  (make-instance 'game
                 :ship (make-instance 'ship
                                      :start-angle 0
                                      :start-velocity (v 0.5 (cos (rad 30))))
                 :fortress (make-instance 'fortress)
                 :mine nil
                 :negative-points :prohibited
                 :missile-penalty 2
                 :auto-turn t
                 :hex-explode t))

;;;;; Testing

(defun python-array-reader (stream char)
  (declare (ignore char))
  (read-delimited-list #\] stream t))

(defun python-comment-reader (stream char)
  (declare (ignore char))
  (do () ((char= (read-char stream nil #\Newline t) #\Newline)))
  (values))

(defun python-comma-reader (stream char)
  (declare (ignore char))
  (read-char stream)
  (values))

(defun read-key-file (key-file)
  (with-open-file (ks key-file)
    ;; FIXME: I would prefer to start with a completely empty readtable
    ;; because key files are so basic. But I don't know how to create
    ;; one.
    (let ((*readtable* (copy-readtable)))
      (set-macro-character #\[ #'python-array-reader)
      (set-macro-character #\] (get-macro-character #\)))
      (set-macro-character #\# #'python-comment-reader)
      (set-macro-character #\, #'python-comma-reader)
      (loop for l = (read ks nil ks)
            until (eq l ks)
            collect l))))

(defun split (str char)
  (loop for last = 0 then (1+ dash)
        for dash = (position char str :start last)
        while dash
        collect (subseq str last dash) into ret
        finally (return (nconc ret (list (subseq str last))))))

;;;; Top level functions

(defun play-game-with-key-function (game key-function &key (id "ssf") (gnum 1) (snum 1) (datadir *default-data-dir*))
  (let* ((base-file (make-pathname :name (format nil "~A-~A-~A" id snum gnum)
                                   :directory (append (pathname-directory datadir) (list id))
                                   :defaults datadir))
         (dat-file (make-pathname :type "dat" :defaults base-file))
         (key-file (make-pathname :type "key" :defaults base-file)))
    (ensure-directories-exist dat-file)
    (with-open-file (dat-stream dat-file
                                :direction :output
                                :if-exists :supersede)
      (with-open-file (key-stream key-file
                                  :direction :output
                                  :if-exists :supersede)
        (reset game)
        (dump-game-state-header game dat-stream)
        (dump-key-changes-header key-stream)
        (with-slots (time max-time) game
          (loop for last-time = 0 then time
                for last-keys = (get-keys (keys game))
                while (< time max-time)
                do (set-keys (keys game) (funcall key-function game))
                   (let ((dat-line (step-game-one-tick game #'dump-game-state-to-string)))
                     (write-string dat-line dat-stream))
                   (dump-key-changes (- time last-time) last-keys (get-keys (keys game)) key-stream)))
        (dump-game-state-footer game dat-stream)))))

(defun replay-game-from-file (game key-file)
  (let* ((parts (split (pathname-name key-file) #\-))
         (id (elt parts 0))
         (snum (parse-integer (elt parts 1)))
         (gnum (parse-integer (elt parts 2)))
         (key-list (read-key-file key-file))
         (key-state nil))
    (labels ((get-keys (game)
               (declare (ignore game))
               (let ((keys (pop key-list)))
                 (loop for k in (second keys)
                       for sym = (case (second k)
                                   (97 :left)
                                   (100 :right)
                                   (119 :thrust)
                                   (32 :fire))
                       do (when sym
                            (ecase (first k)
                              (2 (push sym key-state))
                              (3 (setf key-state (delete sym key-state))))))
                 key-state)))
      (setf (slot-value game 'tick-list) (mapcar #'first key-list))
      (play-game-with-key-function game #'get-keys :id (format nil "~Akr" id) :gnum gnum :snum snum))))

(defun get-autoturn-step-function ()
  "Return a closure that takes actions as input, steps the AUTOTURN game one
tick, and reports game state as lisp data."
  (let ((game (make-explode-auto-turn-game)))
    (reset game)
    (lambda (actions)
      (set-keys (keys game) actions)
      (step-game-one-tick game #'report-game-state))))

(defun get-explode-step-function ()
  "Return a closure that takes actions as input, steps the EXPLODE game one
tick, and reports game state as lisp data."
  (let ((game (make-explode-game)))
    (reset game)
    (lambda (actions)
      (set-keys (keys game) actions)
      (step-game-one-tick game #'report-game-state))))

;;;;;;;;

(defclass connection ()
  ((game-type :initarg :game-type :accessor game-type)
   (game :initform nil :accessor game)
   (state :initform nil :accessor state)))

(defun connection-init (c)
  (print 'connection-init)
  (setf (game c) (ecase (game-type c)
                   (:explode (make-explode-game))
                   (:auto-turn (make-explode-auto-turn-game)))
        (state c) :game)
  (reset (game c)))

(defun connection-send-action (c action key)
  ;; (print (list 'action action key))
  (let ((sym (ecase key
               (119 'thrust)
               (97 'left)
               (100 'right)
               (32 'fire))))
  (cond
    ((string= (string action) (string :keydown)) (game-key-down (game c) sym))
    ((string= (string action) (string :keyup)) (game-key-up (game c) sym))
    (t (error "unknown action: ~S" action)))))

(defun connection-report (c)
  (ecase (state c)
    (:game (report-game-state (game c)))
    (:score (print (report-score (game c))))))

(defun connection-step (c)
  (ecase (state c)
    (:game
     (with-slots (time max-time) (game c)
       (step-game-one-tick (game c) (constantly nil))
       (when (>= time max-time)
         (print 'score)
         (setf (state c) :score))))
    (:score
     (connection-init c))))

;;;;;

(defun speed-test ()
  (let ((start (get-internal-real-time)))
    (format t "start~%")
    (dotimes (i 2000)
      (let ((g (make-explode-game)))
        (with-slots (time max-time) g
          (loop
            (step-game-one-tick g (constantly nil))
            (when (>= time max-time)
              (return))))))
    (format t "ellapsed: ~,2F~%" (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
