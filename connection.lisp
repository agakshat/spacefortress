(defpackage #:ssf
  (:use :cl))

(in-package #:ssf)

;;;; sf-module API

(defclass connection ()
  ((game-type :initarg :game-type :accessor game-type)
   (game :initform nil :accessor game)
   (state :initform nil :accessor state)))

(defun connection-init (c)
  (print 'connection-init)
  (when (game c) (ssf-cffi::free-game (game c)))
  (setf (game c) (ecase (game-type c)
                   (:explode (ssf-cffi::make-explode-game))
                   (:auto-turn (ssf-cffi::make-auto-turn-game)))
        (state c) :game)
  (values))

(defun connection-send-action (c action key)
  (print (list 'action action key))
  (let ((sym (ecase key
               (119 :thrust-key)
               (97 :left-key)
               (100 :right-key)
               (32 :fire-key))))
    (cond
      ((string= (string action) (string :keydown)) (ssf-cffi::press-key (game c) sym))
      ((string= (string action) (string :keyup)) (ssf-cffi::release-key (game c) sym))
      (t (error "unknown action: ~S" action)))
    (values)))

(defun connection-report (c)
  (ecase (state c)
    (:game (ssf-cffi::report-game-state (game c)))
    (:score (print (ssf-cffi::report-game-score (game c))))))

(defun connection-step (c)
  (ecase (state c)
    (:game
     (print 'tick)
     (ssf-cffi::step-one-tick (game c) 33)
     (when (ssf-cffi::game-over-p (game c))
       (print 'score)
       (setf (state c) :score)))
    (:score
     (connection-init c)))
  (values))
