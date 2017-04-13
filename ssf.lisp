;; (defpackage #:ssf
;;   (:use :cl))

(in-package #:ssf-cffi)

(defun distance (o1 o2)
  (sqrt (+ (expt (- (getf (getf o1 'position) 'x)
                    (getf (getf o2 'position) 'x))
                 2)
           (expt (- (getf (getf o1 'position) 'y)
                    (getf (getf o2 'position) 'y))
                 2))))

(defun angle-to (o1 o2)
  (let ((a (atan (- (- (getf (getf o2 'position) 'y)
                       (getf (getf o1 'position) 'y)))
                 (- (getf (getf o2 'position) 'x)
                    (getf (getf o1 'position) 'x)))))
    (if (< a 0)
        (+ a (* PI 2))
        a)))

(defun vdir (s f)
  (let* ((vx (getf (getf s 'velocity) 'x))
         (vy (getf (getf s 'velocity) 'y))
         (dir (atan vy vx))
         (actual (angle-to s f))
         (diff (/ (* (- dir actual) 180) PI)))
    (if (> diff 180)
        (- diff 360)
      (if (< diff -180)
          (+ diff 360)
        diff))))


(defun obj-speed (o)
  (sqrt (+ (expt (getf (getf o 'velocity) 'x) 2)
           (expt (getf (getf o 'velocity) 'y) 2))))

(defun report-object (o)
  (list :alive (getf o 'alive)
        :x (getf (getf o 'position) 'x)
        :y (getf (getf o 'position) 'y)
        :vx (getf (getf o 'velocity) 'x)
        :vy (getf (getf o 'velocity) 'y)
        :orientation (getf o 'angle)))

(defun report-config (game)
  (print (cffi:foreign-slot-value game '(:struct game) 'config)))

(defun report-game-state (game)
  (cffi:with-foreign-slots ((time bighex smallhex score collisions ship fortress) game (:struct game))
    (let ((missiles-pointer (cffi:foreign-slot-pointer game '(:struct game) 'missiles))
          (shells-pointer (cffi:foreign-slot-pointer game '(:struct game) 'shells)))
      (list :screen-type "game"
            :mode "events"
            :game "explode"
            :time time
            :missiles (loop for i below +max-missiles+
                            for o = (getf (cffi:convert-from-foreign (cffi:mem-aptr missiles-pointer '(:struct missile) i) '(:struct missile)) 'o)
                            when (getf o 'alive)
                              collect (report-object o))
            :shells (loop for i below +max-shells+
                          for o = (getf (cffi:convert-from-foreign (cffi:mem-aptr shells-pointer '(:struct shell) i) '(:struct shell)) 'o)
                          when (getf o 'alive)
                            collect (report-object o))
            :ship (append (report-object (getf ship 'o))
                          (list :distance-from-fortress (distance (getf ship 'o) (getf fortress 'o))
                                :vdir (vdir (getf ship 'o) (getf fortress 'o))
                                :speed (obj-speed (getf ship 'o))))
            :fortress (report-object (getf fortress 'o))
            :bighex (getf bighex 'radius)
            :smallhex (getf smallhex 'radius)
            :bonus 0
            :pnts (getf score 'points)
            :rawpnts (getf score 'raw-points)
            :vlner (getf score 'vulnerability)
            :collisions (loop for slot in '(bighex smallhex shell-ship missile-fortress)
                              for name in '("bighex" "smallhex" "shell" "missile")
                              for v = (getf collisions slot)
                              when v
                                collect name)
            :active t
            :events (loop with events-pointer = (cffi:foreign-slot-pointer game '(:struct game) 'events)
                          for i below (cffi::foreign-slot-value events-pointer '(:struct events) 'count)
                          for e = (cffi:mem-aref (cffi:foreign-slot-pointer events-pointer '(:struct events) 'events) 'event i)
                          collect e)
            :keys nil))))

(defun report-game-score (game)
  (cffi:with-foreign-slots ((score) game (:struct game))
    (list :screen-type "score"
          :mode "events"
          :bonus 0
          :total-bonus 0
          :pnts (getf score 'points)
          :rawpnts (getf score 'raw-points))))
