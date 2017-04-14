(defpackage #:sf-connection
  (:use :cl))

(in-package #:sf-connection)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'python-connection *backends*))

(defparameter *host-address* "127.0.0.1")
(defparameter *host-port* 3000)

(defclass python-connection (backend-connection)
  ((host :initform *host-address* :accessor host)
   (port :initform *host-port* :accessor port)
   (socket :initform nil :accessor socket)))

(defun connection-read-line (c error-message)
  (let ((line (read-line (usocket:socket-stream (socket c)) nil (socket c))))
    (if (eq line (socket c))
        (error 'disconnected :details "Error reading from game socket.")
        (let ((sexp (read-from-string line nil (socket c))))
          (if (eq sexp (socket c))
              (progn
                (usocket:socket-close (socket c))
                (error 'communication-error :details "~A: ~S" error-message line))
              sexp)))))

(defun connection-write-line (c fmt &rest args)
  (write-line (apply #'format nil fmt args) (usocket:socket-stream (socket c)))
  (force-output (usocket:socket-stream (socket c))))

(defmethod backend-connection-init ((c python-connection))
  (labels ((bad-connection-exit (message)
             (usocket:socket-close (socket c))
             (error 'communication-error :details message)))
    (print 'connection-init)
    (setf (socket c) (usocket:socket-connect (host c) (port c) :nodelay :if-supported))
    (if (socket c)
        (let (data)
          ;; Sanity check
          (setf data (connection-read-line c "Failed to read initial config"))
          (unless (string= (getf data :screen-type) "config")
            (bad-connection-exit (format nil "Expected a config line but got ~s" data)))
          ;; Display Level
          (connection-write-line c "config display_level ~d" (if (draw c) (if (eq (draw c) :debug) 1 2) 0))
          (setf data (connection-read-line c "Failed reading response to display_level setting"))
          (unless (and (string= (getf data :screen-type) "config")
                       (getf data :result))
            (bad-connection-exit (format nil "Did not get a true config result from changing display_level ~s" data)))
          ;; Subject ID
          (connection-write-line c "id ~a" (subject-id c))
          (setf data (connection-read-line c "Failed reading response to id setting"))
          (unless (and (string= (getf data :screen-type) "config")
                       (or (getf data :result)
                           (string-equal (string (subject-id c)) (getf data :id))))
            (bad-connection-exit (format nil "Did not get a true config result from changing id ~s" data)))
          ;; Game Type
          (ecase (game-type c)
            (:explode (connection-write-line c "condition explode"))
            (:auto-turn (connection-write-line c "condition autoturn")))
          (setf data (connection-read-line c "Failed to read response to condition setting"))
          (unless (and (string= (getf data :screen-type) "config")
                       (getf data :result))
            (bad-connection-exit (format nil "Did not get a true config result from changing condition ~s" data)))
          (connection-write-line c "continue")
          t)
        (bad-connection-exit "Could not open socket connection."))))

(defmethod backend-connection-send-action ((c python-connection) action key)
  (connection-write-line c "~a ~d 0" action key))

(defmethod backend-connection-report ((c python-connection))
  (connection-read-line c "Failed to get game state report."))

(defmethod backend-connection-step ((c python-connection))
  (connection-write-line c "continue"))
