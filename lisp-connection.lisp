(defpackage #:sf-connection
  (:use :cl))

(in-package #:sf-connection)

;;;; sf-module API

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'lisp-connection *backends*))

(defclass lisp-connection (backend-connection)
  ((game :initform nil :accessor game)
   (state :initform nil :accessor state)))

(defun lisp-start-game (c)
  (setf (game c) (ecase (game-type c)
                   (:explode (ssf::make-explode-game))
                   (:auto-turn (ssf::make-explode-auto-turn-game)))
        (state c) :game))

(defmethod backend-connection-init ((c lisp-connection))
  (print 'connection-init)
  (lisp-start-game c))

(defmethod backend-connection-send-action ((c lisp-connection) action key)
  (let ((sym (ecase key
               (119 'thrust)
               (97 'left)
               (100 'right)
               (32 'fire))))
  (cond
    ((string= (string action) (string :keydown)) (ssf::game-key-down (game c) sym))
    ((string= (string action) (string :keyup)) (ssf::game-key-up (game c) sym))
    (t (error "unknown action: ~S" action)))))

(defmethod backend-connection-report ((c lisp-connection))
  (ecase (state c)
    (:game (ssf::report-game-state (game c)))
    (:score (print (ssf::report-score (game c))))))

(defmethod backend-connection-step ((c lisp-connection))
  (ecase (state c)
    (:game
     (with-slots (ssf::time ssf::max-time) (game c)
       (ssf::step-game-one-tick (game c) (constantly nil))
       (when (>= ssf::time ssf::max-time)
         (print 'score)
         (setf (state c) :score))))
    (:score
     (lisp-start-game c))))
