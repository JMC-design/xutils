(defpackage :window-manager (:nicknames :wm)
  (:use :cl :xutils :surface :layout :bordeaux-threads #+life :life)
  (:export #:init
	   #:*manager*
	   #:journal-tail
	   #:search-journal-of
	   #:erase-journal))
(in-package :window-manager)

(defparameter *manager* () "The peace keeper.")
(define-symbol-macro *consentida* (focus-of *manager*))
(define-symbol-macro *client* (client-of *manager*))

;; We need somebody to keep track of everything.

(defclass window-manager (#+life manager)
  ((layouts :accessor layouts-of
	    :initform '())
   (clients :accessor clients-of
	    :initarg :clients
	    :initform '())
   (client :accessor client-of
	   :initarg :client
	   :initform nil)
   (bindings :accessor bindings
	     :initarg :bindings)
   #-life
   (journal :accessor journal-of
	    :initform '())
   #-life
   (units  :accessor units-of
	   :initform '()
	   :initarg :units)
   #-life
   (sparks :accessor sparks-of
	   :initarg :sparks
	   :initform '())))


;; We need to take control and optionally announce ourselves.
;; What happens if another window manager is running?
(defun take-control (client)
  (let ((event-mask (xlib:make-event-mask :substructure-notify :focus-change :button-press)))    
    (with-client client
      (setf (xlib:window-event-mask *root-window*) event-mask))))
#+life
(defmethod manage ((win xlib:window) (manager window-manager))
  (call-next-method win manager))

#-life
(defun journal (manager item)
  (push (cons (get-internal-real-time) item)(journal-of manager)))

#-life
(defun manage (window manager)
  (push window (units-of manager)))


;;;; BASIC EVENT HANDLERS

;;To know what we don't know, it's good to record it
(defun journal-event (&rest plist)
  (destructuring-bind (&key override-redirect-p &allow-other-keys)plist
    (declare (ignorable override-redirect-p))					
    (journal *manager* (print-event plist))))

;; We need to detect when a toplevel window is created and add it to the manager.
;; Here we just naively grab everything that doesn't have override-redirect set.
(defun map-response (&rest keys)
  (destructuring-bind (&key window override-redirect-p &allow-other-keys) keys
    (when (not override-redirect-p)
      (ignore-errors 
	(journal *manager* (cons :adding window))
	(manage window *manager*)))))

;; We need to know when to get rid of windows as well.
(defun unmap-response (&rest plist) ;why does this function not work?
  (destructuring-bind (&key window &allow-other-keys)plist
    (journal *manager* (cons :unmap (if (member window (units-of *manager*))
					(setf (units-of *manager*) (remove window (units-of *manager*)))
					window)))
    (xlib:display-force-output (client-display (car (clients-of *manager*))))))
(defun destroy-response (&rest plist)    
  (destructuring-bind (&key window &allow-other-keys)plist
    ;;xlib:drawable/window-equal MUST be used as a test
    (let ((test (member window (units-of *manager*) :test #'xlib:drawable-equal)))
      (if test
	  (progn
	    (journal *manager*
		     (format nil "We're in destroy. Window is ~a which is a member." window)) 
	    (setf (units-of *manager*) (remove window (units-of *manager*):test #'xlib:drawable-equal)))
	  ;;it's nice to know how much traffic is caused by windows we don't care about.
	  (journal *manager*
		   (cons :destroy ( format nil "~a is not a member of ~a. TEST: ~a"
					   (xlib:window-id window) (mapcar #'xlib:window-id (units-of *manager*)) test)))))))

;;; This destructuring-bind is getting tiresome and verbose, lets get rid of it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-keys (key-list plist &body body)
    `(destructuring-bind (&key ,@key-list &allow-other-keys),plist
       (declare (ignorable ,@key-list)) ,@body)))

;; the easiest way to deal with focus is to not. We use this anytime somebody grabs focus, then focus follows mouse.
(defun reset-focus (&rest plist)
  (with-keys (window display) plist ;yes, definitely better
    (xlib:set-input-focus display :pointer-root :pointer-root)))

;; it's nice to know what we're ignoring about whom and how many times.
(let ((counter 0))
  (declare (type fixnum counter))
  (defun ignore-event (&rest rest)
    (with-keys (event-key event-window window) rest
      (journal *manager* (cons :ignore (format nil "#~a ~w ~x" (incf counter) event-key
					       (let ((win (or window event-window)))
						 (when (xlib:drawable-p win) (xlib:drawable-id (or window event-window))))))))))

;; Some people type in more than one language, clx gives us a convenience function to update key mappings.
(defun mapping-response (&rest plist)
  (with-keys (window display request start count)plist
    (journal *manager* (cons request (xlib:mapping-notify display request start count)))))

;;; This plist stuff and constant journaling is getting verbose and repetitive let's get rid of it.
(eval-when (:compile-toplevel :load-toplevel :execute)    
  (defmacro define-handler (name keys type-decls &body body)
    `(defun ,name (&rest plist)
       (with-keys ,keys plist
	 (declare ,@(loop :for decl :in type-decls
		       :collect `(type ,(first decl) ,(second decl))))
	 (journal *manager* (cons ,(intern (symbol-name name) :keyword) (progn ,@body)))))))

;; Is anybody trying to tell us anything interesting?
(define-handler client-message-response (window data) () ;ah, much better
    (let ((atom-list (when data (loop for atom across data collect
				     (xlib:atom-name (xlib:window-display window) atom)))))
      (format nil "~x says ~a" (xlib:window-id window) atom-list)))

;; What's with all this keycode confusion?
(define-handler journal-key (display code state child) ()
  (let ((state-keys (when state (xlib::make-state-keys state))))
    (format nil "Key:~a Char:~a~@[ Child:~a~]~@[ state:~{~a~}~]"
	    code (when code (xlib:keycode->character *display* code state))
	    (when child (xlib:wm-name child))
	    (when state state-keys )))
  (xlib:allow-events display :replay-keyboard));in case we grab actively

;; Ok, so lets do something with a key
(define-handler process-key (code state display)
    ((integer code) (integer state))
  ;;23 is tab. trust me. clx not updated yet.
  (case code
    (23 (if (= 8 (logand state 8)) ;fast check for ctrl so we don't call expensive fn's all the time.
	    (format nil "Cycling ~a windows." (length (cycle-windows)))
	    (xlib:allow-events display :sync-keyboard)))))

(define-handler replay-keyboard (display) ()
  (xlib:allow-events display :async-keyboard))

;; We have everything being journaled.  How about some convenience when the journal gets long?
(defun journal-tail (obj &optional (tail-length 10))
  "Show the most recent events."(let ((journal (journal-of obj)))
    (subseq journal 0 (min tail-length (length journal)))))

(defun erase-journal (&optional (obj *manager*))
  (setf (journal-of obj)'()))

(defun search-journal-of (obj item &key (key #'cadr) (test #'eql))
  (remove-if-not (lambda (x) (funcall test item x)) (journal-of obj) :key key))


;;;; KEY GRABBING

;; Xlib sans xkb, requires us to grab every single combination  of LOCK keys if we want our bindings to work
;; when someone has numlock or caps on.
;; keycode 77, keysym 65407 #xff7f is numlock. WTF so complicated to find it?
(defun grab-key-ignore (window key &key (modifiers '())(owner-p t) (sync-pointer-p nil) (sync-keyboard-p nil) (ignore '(:lock :mod-2))) ;mod-2=numlock
  (values (xlib:grab-key window key :modifiers modifiers
			 :owner-p owner-p
			 :sync-pointer-p sync-pointer-p
			 :sync-keyboard-p sync-keyboard-p)	  
	  (dolist (vars (combos ignore))
	    (apply #'xlib:grab-key `(,window ,key :modifiers ,(append modifiers vars)
					      :owner-p owner-p
					      :sync-pointer-p sync-pointer-p
					      :sync-keyboard-p sync-keyboard-p)))))
;; here's a naive combo generator that doesn't work past 3 keys.
(defun combos (ignores)
  (let ((result '())
	(list ignores))
    (dotimes (x (length ignores))
      (setf result (concatenate 'list
				(list (list (car list)))
				(maplist (lambda (i) (concatenate 'list i (list (car list)))) (cdr list))
				result))
      (setf list (cycle-list list)))
    (remove-duplicates (mapcar (lambda (item) (if (listp item) (sort item #'string-lessp) item)) result) :test #'equal)))

;;;; WINDOW LAYOUT

(defmethod (setf units-of) :after (value (manager window-manager))
  (layout-windows manager)
  (mapcar #'xlib:add-to-save-set value)) ;In case we go down

(defun layout-windows (&optional (manager *manager*))
  (let* ((units (units-of manager))
	 (num (length units))
	 (layouts (assoc num (layouts-of manager)))
	 (layout (if layouts
		     (typecase layouts
		       ;(layout )	;put named window crap here
		       (cons (cdr layouts)))
		     (let ((layout (generate-layout num :type (if (< num 4) :phi :grid))))
		       (push (cons num layout) (layouts-of manager))
		       layout))))
    (loop :for win :in units
       :for set :in layout :do
       (destructuring-bind (a size location) set (declare (ignore a))
			   (move-surface win location)
			   (resize-surface win size)))))

(defun cycle-windows(&optional (manager *manager*))
  (setf (units-of *manager*) (cycle-list (units-of manager))))

(defun cycle-list (list)
  (let* ((temp list)
	 (tail (pop temp)))
    (append temp (list tail))))

(defparameter *handlers* (list
			  (cons :mapping-notify  #'mapping-response);keyboard map change
			  (cons :unmap-notify #'unmap-response)
			  (cons :configure-notify #'ignore-event)
			  (cons :map-notify #'map-response)
			  (cons :destroy-notify #'destroy-response)
			  (cons :map-request #'journal-event)
			  (cons :key-press #'replay-keyboard)
			  (cons :key-release #'process-key)
			  (cons :no-exposure #'ignore-event)
			  (cons :create-notify #'ignore-event)
			  (cons :client-message #'client-message-response)
			  (cons :graphics-exposure #'ignore-event)
			  (cons :focus-in #'ignore-event)
			  (cons :focus-out #'reset-focus)));save us from greedy apps

;;;; EVENT LOOP

(defmacro make-event-loop (manager)
  `(let* ((client (car (clients-of ,manager)))
	  (display (client-display client)))
     (setf (sparks-of ,manager)
	   (bt:make-thread
	    (lambda () (progn
			 (take-control client)
			 (with-client *client*
			   (grab-key-ignore *root-window* 23 :modifiers '(:mod-1) :owner-p t)
			   (loop (xlib:process-event display :handler (client-vector *client*) :discard-p t)))))))))

(defun init ()
  (let* ((display (xlib:open-default-display))
	 (root (xlib:screen-root (car (xlib:display-roots display))))
	 (client (xevents::make-client :display display :root root :handlers *handlers*))
	 (windows  (get-toplevel-windows (xlib:screen-root (car (xlib:display-roots (client-display client)))))))
    (setf *manager* (make-instance 'window-manager
				   :units (mapcar #'cadr (remove-if-not (lambda (unit) (member :viewable unit))windows))
				   :clients (list client)
				   :client client))
    (dolist (window (units-of *manager*))
      (manage window *manager*))
    (layout-windows *manager*)
    (populate-vector client #'journal-event)    
    (make-event-loop *manager*)))
    
;;;; Elements stuff
#+life
(defmethod manage ((win xlib:window) (manager window-manager))  
    (call-next-method win manager))
#+life
(defmethod eliminate ((win xlib:window)&key (god *god*))
  (declare (ignore god))
  (xlib:unmap-window win)
  (xlib:destroy-window win))

