(defpackage :xutils
  (:nicknames :x)
  (:use :cl :xlib :xwindows :xevents)
  (:export #:*standard-view-events*
	   #:set-event-mask
	   client
	   client-display
	   client-handlers
	   client-vector
	   with-client
	   #:populate-vector
	   #:add-handler
	   #:print-event

	   #:get-window
	   #:*screen-width*
	   #:*screen-height*
	   #:*display*
	   #:*root-window*
	   #:*default-screen*
	   #:*foreground*
	   #:*background*
	   #:*gcontext*
	   #:*pixmap32*
	   #:*visual32*
	   #:*colormap32*
	   #:*gcontext32*

	   #:get-toplevel-windows
	   #:classify-window
	   #:modifier-keys))
(in-package :xutils)

;; Get list of top level windows
(defun get-toplevel-windows (&optional (root-window *root-window*))
  (mapcar #'classify-window (xlib:query-tree root-window)))

(defun classify-window (window)
  "Returns Name, Resource, Class."
  ;;this should be generic. make macro that takes args and only collects and returns those that are set
  ;;caller can then do with them what they will.
  (let* ((state (xlib:window-map-state window))
	(name (xlib:wm-name window))
	(protocols (xlib:wm-protocols window))
	 (size-hints (xlib:wm-hints window))
	 (wtype (xlib:get-property window :_net_wm_window_type :transform (lambda (int) (xlib:atom-name xwindows:*display* int))))
	 (id (when size-hints (xlib:wm-hints-window-group size-hints))))
    (multiple-value-bind (resource class) (xlib:get-wm-class window)
      (list state window name wtype resource class protocols size-hints))))

(defun modifier-keys ()
  (let ((list (multiple-value-list (xlib:modifier-mapping *display*)))
	(mods '(:shift :lock :control :mod1 :mod2 :mod3 :mod4 :mod5)))
    (mapcar (lambda (mod keycodes) (print mod) (terpri)
		    (loop for keycode in keycodes
		       do (let* ((keysym (xlib:keycode->keysym *display* keycode 0))
				 (char (xlib:keysym->character *display* keysym)))
			    (format t "keycode:~a ~20t keysym:~a ~35t char:~a~%" keycode keysym char))) ) mods list)))
