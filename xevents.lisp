(defpackage :xevents
  (:use :cl :xlib :xwindows)
  (:export #:*standard-view-events*
	   #:*window-events*
	   #:*key-events*
	   #:*buttons*
	   #:set-event-mask
	   client
	   client-display
	   client-handlers
	   client-vector
	   ;client-spark
	   with-client
	   #:populate-vector
	   #:add-handler
	   #:print-event))
(in-package :xevents)

;; Lets group events to be easier to manipulate
(defparameter *window-events* '(:enter-window :exposure	:focus-change :leave-window))
(defparameter *key-events* '(:key-press :key-release :keymap-state))
(defparameter *state-changes* '(:colormap-change :property-change
				:structure-notify :substructure-notify
				:substructure-redirect :visibility-change))
(defparameter *buttons* '(:button-1-motion :button-2-motion
			  :button-3-motion :button-4-motion
			  :button-5-motion :button-motion
			  :button-press    :button-release
			  :owner-grab-button :pointer-motion
			  :pointer-motion-hint))
(defparameter *wm-events* '(:substructure-redirect :substructure-notify))
(defparameter *standard-view-events* `(,@*buttons* :structure-notify :visibility-change))

;; now we need a way to use these definitions since we've broken up the events into lists
(defun set-event-mask (&rest rest )
  "Returns an event mask from one or more lists of x11 events. Use xlib:make-event-mask otherwise."
  (apply #'make-event-mask (apply #'concatenate 'list rest)))

;;clx has one event queue per DISPLAY that gets locked by PROCESS-EVENT et al.
;;we have to be careful to not use XLIB:WINDOW-DISPLAY if we want to handle multiple
;;connections.
(defstruct client
  (display (xlib:open-default-display) :type xlib:display) ; the server we're talking to.
  root 
  (handlers '()) ; ALIST of (eventtype . handler) e.g. (:key-press . #'print-event)			   
  vector)      ; vector to be handed directly to PROCESS-EVENT

;; lets make it a bit easier to access the most common things we want from our client.
;; What more should be added?  Why not just store these in the client?
(defmacro with-client (client &rest body)
  `(let* ((xwindows:*display* (client-display ,client))
	 (xwindows:*default-screen* (car (xlib:display-roots xwindows:*display*)))
	 (xwindows:*root-window* (xlib:screen-root xwindows:*default-screen*)))
     ,@body))

(defmacro with-new-handlers (client handlers &body body)
  (let ((orig-client-vector (client-vector client)))
    (unwind-protect
	  `(progn
	    (mapcar (lambda (handler) (push-handler ,client handler)),handlers)
	    ,@body)
      (;mapcar (lambda (handler) (pop-handler client (car handler))) handlers
       ))))

(defmacro with-client2 (client &rest body)
  (let ((things-to-bind (find-in body '(*display* *default-screen* *root-window*) body)))
    `(let* ,things-to-bind)))

;(defmacro with-structs (struct accessors &body))

(defun find-in-body (find-list body)
  (loop :for line :in body :nconc
     (loop :for symb :in line
	:when (member symb find-list :test #'equal :key #'car)
	:collect (cdr symb) :into result
	:finally (return result))))

;; (find-in-body ('((*display* . `(client-display ,client)) *root-window*) '((open-default-display *display*) (do-something with *root-window*))))

;;xlib::*event-key-vector* changes depending on extensions present.
(defun populate-vector(client &optional (default-handler nil))
  "Takes alist of handlers from client and slots them into correct slot in event vector."
  (let ((event-vector (copy-seq xlib::*event-key-vector*))	 
	(handlers (client-handlers client)))
    (loop :for key :across event-vector
       :do (let ((handler (assoc key handlers))
		 (pos (position key event-vector)))
	     (setf (aref event-vector pos) (if handler (cdr handler) default-handler))))
    (setf (client-vector client) event-vector)))

(defun replace-handler (client handler)
  "Adds handler to a client and slots it in the client vector."
  (setf (aref (client-vector client) (position (car handler) xlib::*event-key-vector* ))(cdr handler))
  (rplacd (assoc (car handler) (client-handlers client)) (cdr handler)))

(defun add-handler (client handler)
  "Adds handler to a client and slots it in the client vector."
  (setf (aref (client-vector client) (position (car handler) xlib::*event-key-vector* ))(cdr handler)))

(defun push-handler (client handler)
  "Adds handler to a client and slots it in the client vector."
  (setf (aref (client-vector client) (position (car handler) xlib::*event-key-vector* ))(cdr handler))
  (push (cdr handler) (cdr (assoc (car handler) (client-handlers client)))))

(defun pop-handler (client event-key)
  (let ((old (pop (cdr (assoc event-key (client-handlers client))))))
    (setf (aref (client-vector client) (position (car old) xlib::*event-key-vector* ))(cdr (assoc event-key (client-handlers client))))))
(defun update-handlers (client)
  )
;;events are simply plists, easy to print or destructure.
(defun print-event (&rest plist)
  (format nil "~{~a: ~a ~%~}~%" plist))


