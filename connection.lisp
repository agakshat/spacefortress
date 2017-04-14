(defpackage #:sf-connection
  (:use :cl)
  (:export #:connection-init
           #:connection-send-action
           #:connection-report
           #:connection-step
           #:connection-error
           #:disconnected
           #:communication-error))
(in-package #:sf-connection)

(defparameter *backend* 'ssf-connection)

(defparameter *game-type* :auto-turn)

(defvar *backends* nil)

(define-condition connection-error (error)
  ((details :initform nil :initarg :details :accessor details)))

(define-condition disconnected (connection-error)
  ()
  (:report (lambda (c stream)
             (format stream "Disconnected from model server: ~A" (details c)))))

(define-condition communication-error (connection-error)
  ()
  (:report (lambda (c stream)
             (format stream "communication error with model server: ~A" (details c)))))

(defclass backend-connection ()
  ((game-type :initarg :game-type :accessor game-type)
   (subject-id :initarg :subject-id :initform nil :accessor subject-id)
   (draw :initarg :draw :initform t :accessor draw)))

(defgeneric backend-connection-init (connection))
(defgeneric backend-connection-send-action (connection action key))
(defgeneric backend-connection-report (connection))
(defgeneric backend-connection-step (connection))

;; The public API

(defun connection-init (subject-id draw)
  (let ((c (make-instance *backend* :game-type *game-type*
                                    :subject-id subject-id
                                    :draw draw)))
    (if (backend-connection-init c)
        c
        nil)))

(defun connection-send-action (c action key)
  (backend-connection-send-action c action key)
  (values))

(defun connection-report (c)
  (backend-connection-report c))

(defun connection-step (c)
  (backend-connection-step c)
  (values))

