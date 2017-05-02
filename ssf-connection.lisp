(in-package #:sf-connection)

;;;; sf-module API

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'ssf-connection *backends*))

(defclass ssf-connection (backend-connection)
  ((game :initform nil :accessor game)
   (state :initform nil :accessor state)))

(defun ssf-start-game (c)
  (when (game c) (ssf-cffi::free-game (game c)))
  (setf (game c) (ecase (game-type c)
                   (:explode (ssf-cffi::make-explode-game))
                   (:auto-turn (ssf-cffi::make-auto-turn-game)))
        (state c) :game))

(defmethod backend-connection-init ((c ssf-connection))
  (print 'connection-init)
  (ssf-start-game c))

(defmethod backend-connection-send-action ((c ssf-connection) action key)
  ;; (print (list 'action action key))
  (let ((sym (ecase key
               (119 :thrust-key)
               (97 :left-key)
               (100 :right-key)
               (32 :fire-key))))
    (cond
      ((string= (string action) (string :keydown)) (ssf-cffi::press-key (game c) sym))
      ((string= (string action) (string :keyup)) (ssf-cffi::release-key (game c) sym))
      (t (error "unknown action: ~S" action)))))

(defmethod backend-connection-report ((c ssf-connection))
  (ecase (state c)
    (:game
     (cffi:with-foreign-pointer (state 10000)
       (ssf-cffi::dump-sexp-game-state (game c) state 10000)
       (read-from-string (cffi:convert-from-foreign state :string))))
    (:score (print (ssf-cffi::report-game-score (game c))))))

(defmethod backend-connection-step ((c ssf-connection))
  (ecase (state c)
    (:game
     (ssf-cffi::step-one-tick (game c) 33)
     (when (ssf-cffi::game-over-p (game c))
       (print 'score)
       (setf (state c) :score)))
    (:score
     (ssf-start-game c))))
