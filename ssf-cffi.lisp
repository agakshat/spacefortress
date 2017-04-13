(defpackage #:ssf-cffi
  (:use :cl))

(in-package #:ssf-cffi)

(cffi:define-foreign-library ssf
  (t (:default "ssf")))

(cffi:use-foreign-library ssf)

(defconstant +max-events+ 50)
(defconstant +max-key-events+ 50)
(defconstant +max-missiles+ 50)
(defconstant +max-shells+ 50)

(cffi:defcenum keysym :no-key :fire-key :thrust-key :left-key :right-key)

(cffi:defcenum turn :no-turn :turn-left :turn-right)

(cffi:defcenum event :missile-fired :fortress-fired :shell-hit-ship :hit-fortress :vlner-increased
  :vlner-reset :fortress-destroyed :fortress-respawn :ship-respawn :explode-bighex :explode-smallhex)

;; (cffi:defctype event :int)

(cffi:defctype timer :int)

(cffi:defcstruct key
  ;; (key keysym)
  (key :int)
  (state :int))

(cffi:defcstruct point
  (x :double)
  (y :double))

(cffi:defcstruct object
  (position (:struct point))
  (velocity (:struct point))
  (angle :double)
  (collisionRadius :int)
  (alive :bool))

(cffi:defcstruct hexagon
  (points (:array (:struct point) 6))
  (radius :int))

(cffi:defcstruct ship
  (o (:struct object))
  (death-timer timer)
  (thrust-flag :bool)
  (turn-flag turn))

(cffi:defcstruct fortress
  (o (:struct object))
  (death-timer timer)
  (vlner-timer timer)
  (timer timer)
  (last-angle :double))

(cffi:defcstruct missile
  (o (:struct object)))

(cffi:defcstruct shell
  (o (:struct object)))

(cffi:defcstruct score
  (points :int)
  (raw-points :int)
  (vulnerability :int))

(cffi:defcstruct stats
  (ship-deaths :int))

(cffi:defcstruct keys
  (thrust :bool)
  (left :bool)
  (right :bool)
  (fire :bool)
  (event-count :int)
  (events (:array (:struct key) 50)))

(cffi:defcstruct events
  (events (:array event 50))
  (count :int))

(cffi:defcstruct collisions
  (big-hex :bool)
  (small-hex :bool)
  (missile-fortress :bool)
  (shell-ship :bool))

(cffi:defcstruct config-projectile
  (speed :int)
  (collision-radius :int))

(cffi:defcstruct config-fortress
  (sector-size :int)
  (lock-time :int)
  (vulnerability-time :int)
  (vulnerability-threshold :int)
  (collision-radius :int))

(cffi:defcstruct config-ship
  (explode-duration :int)
  (turn-speed :int)
  (collision-radius :int)
  (acceleration :double)
  (start-position (:struct point))
  (start-velocity (:struct point))
  (start-angle :int))

(cffi:defcstruct config
  (width :int)
  (height :int)
  (game-time :int)
  (destroy-fortress :int)
  (ship-death-penalty :int)
  (missile-penalty :int)
  (shell (:struct config-projectile))
  (fortress (:struct config-fortress))
  (big-hex :int)
  (small-hex :int)
  ;; (missile (:struct config-projectile))
  (missile-speed :int)
  (missile-collision-radius :int)
  ;; (next-thing :int)

  ;; (ship-explode-duration :int)
  ;; (ship-turn-speed :int)
  ;; (ship-collision-radius :int)
  ;; (ship-acceleration :double)
  ;; (ship-start-position (:struct point))
  ;; (ship-start-velocity (:struct point))
  ;; (ship-start-angle :int)

  (ship (:struct config-ship))
  (auto-turn :bool))

(cffi:defcstruct game
  (config (:struct config))
  (keys (:struct keys))
  (ship (:struct ship))
  (fortress (:struct fortress))
  (missiles (:array (:struct missile) 50))
  (shells (:array (:struct shell) 50))
  (bighex (:struct hexagon))
  (smallhex (:struct hexagon))
  (score (:struct score))
  (stats (:struct stats))
  (tick :int)
  (time :int)
  (collisions (:struct collisions))
  (events (:struct events)))

(cffi:defcfun ("makeExplodeGame" make-explode-game) :pointer)
(cffi:defcfun ("makeAutoTurnGame" make-auto-turn-game) :pointer)
(cffi:defcfun ("freeGame" free-game) :void (game :pointer (:struct game)))

(cffi:defcfun ("pressKey" press-key) :void
  (game :pointer (:struct game))
  (key keysym))

(cffi:defcfun ("releaseKey" release-key) :void
  (game :pointer (:struct game))
  (key keysym))

(cffi:defcfun ("stepOneTick" step-one-tick) :void
  (game :pointer (:struct game))
  (ms :int))

#-allegro
(cffi:defcfun ("isGameOver" game-over-p) :bool
  (game :pointer (:struct game)))

;; Allegro CL on OSX has a problem with a :bool return type. So do
;; this little dance.
#+allegro
(cffi:defcfun ("isGameOver" game-over-helper) :boolean
  (game :pointer (:struct game)))

#+allegro
(defun game-over-p (g) (/= (game-over-helper g) 0))
