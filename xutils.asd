(in-package :asdf-user)
(defsystem "xutils"
  :description "Utilities to work with clx"
  :version "0.0.1"
  :author "Johannes Martinez Calzada"
  :components ((:file "xwindows")
	       (:file "x11-image")
	       (:file "xevents")
	       (:file "x11screen")
	       (:file "xutils")))
