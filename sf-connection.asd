(in-package #:asdf)

(defsystem sf-connection
  :depends-on (cffi usocket)
  :serial t
  :components ((:file "ssf-cffi")
               (:file "ssf")
               (:file "simple-space-fortress")
               (:file "connection")
               (:file "ssf-connection")
               (:file "lisp-connection")
               (:file "python-connection")))
