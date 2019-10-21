(defpackage :xwindows
  (:use :cl :xlib)
  (:export #:get-window
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
	   #:*gcontext32*))
(in-package :xwindows)

;;;; Display defaults

(defparameter *display* nil) ;#.(open-default-display)
(defparameter *default-screen* nil) ;#.(display-default-screen *display*)
(defparameter *screen-width* nil) ;#.(xlib:screen-width *default-screen*)
(defparameter *screen-height*  nil) ;#.(xlib:screen-height *default-screen*)
(defparameter *root-window* nil) ;#.(xlib:screen-root *default-screen*)
(defparameter *default-colourmap* nil) ;#.(car (installed-colormaps *root-window*))
(defparameter *window-list* nil) ;#.(xlib:query-tree *root-window*)
(defparameter *visual* nil) ;#.(xlib:window-visual *root-window*)
(defparameter *background* nil) ;#.(screen-black-pixel *default-screen*)
(defparameter *foreground* nil) ;#.(screen-white-pixel *default-screen* )


;;;; Set Display defaults
(defun init-default-display ()
  (setf *display* (open-default-display)
	*default-screen* (display-default-screen *display*)
	*screen-width* (xlib:screen-width *default-screen*)
	*screen-height* (xlib:screen-height *default-screen*)
	*root-window* (xlib:screen-root *default-screen*)
	*default-colourmap* (car (installed-colormaps *root-window*))
	*window-list* (xlib:query-tree *root-window*)
	*visual* (xlib:window-visual *root-window*)
	*default-parent* *root-window*
	*gcontext* (get-gcontext)
	*pixmap32* (xlib:create-pixmap :width 1 :height 1 :depth 32 :drawable *root-window*)
	*visual32* (get-depth32)
	*colormap32* (create-colormap *visual32* *root-window*)
	*gcontext32* (get-gcontext *pixmap32*))
  (values))

;;;; Window defaults
(defparameter *default-height* 256)
(defparameter *default-width* 256)
(defparameter *default-parent* nil);*root-window*
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *depth* 24 ) ;#.(xlib:drawable-depth *root-window*)
(defparameter *default-class* :input-output) ; :input-only allows cursor, dnpm, event-mask, gravity, override
(defparameter *backing-pixel* 0)
(defparameter *border-width* 0)
;(defparameter *backing-planes*)
(defparameter *backing-store* :always)
(defparameter *border* 800000)
(defparameter *colormap* :copy)
(defparameter *cursor* :none)
(defparameter *do-not-propagate-mask* 0)
(defparameter *event-mask* 0)
(defparameter *gravity* :center)
(defparameter *override-redirect* :on)
(defparameter *save-under* :on)

(defparameter *gcontext* nil)

(defun get-gcontext (&optional (window *root-window*))
  "GC is usable on windows with same depth and screen-root as given WINDOW."
  (xlib:create-gcontext :drawable window
			:foreground *foreground*
			:background *background*
			:cache-p t
			:subwindow-mode :include-inferiors))

(defun get-depth32 ()
  "Return the first available 32 bit visual for the *DEFAULT-SCREEN*."
  (cadr (assoc 32 (screen-depths *default-screen*))))
(defparameter *visual32* nil   )
(defparameter *pixmap32* nil) 
(defparameter *colormap32* nil )
(defparameter *gcontext32* nil)


;;this may not be the right thing, but for convenience of development
;(setf (display-after-function *display*) #'display-force-output)

(defun get-window (&key (parent *default-parent*)
		     (x *x*) (y *y*)
		     (width *default-width*)
		     (height *default-height*)
		     (background *background*)
		     (border *border*)
		     (gravity *gravity*)
		     (backing-store *backing-store*)
		     (border-width *border-width*)
		   ; (backing-planes *backing-planes*)
		     (backing-pixel *backing-pixel*)
		     (save-under *save-under*)
		     (event-mask *event-mask*)
		     (do-not-propagate-mask *do-not-propagate-mask*)
		     (override-redirect *override-redirect*)
		     (colormap *colormap*) (cursor *cursor*)
		     (depth *depth*) (visual *visual*))
  (xlib:create-window :parent parent :x x :y y
		      :width width :height height
		      :background background
		      :border border
		      :bit-gravity gravity
		      :backing-store backing-store
		     ;:backing-planes backing-planes
		      :border-width border-width
		      :backing-pixel backing-pixel
		      :save-under save-under
		      :event-mask event-mask
		      :do-not-propagate-mask do-not-propagate-mask
		      :override-redirect override-redirect
		      :colormap colormap :cursor cursor
		      :depth depth :visual visual))

(defun get-32bpp-format ()
  (dolist (format (xlib:find-matching-picture-formats *display* :depth 32 :blue 8 :alpha 8))
    (when (equalp (picture-format-alpha-byte format) '(8 . 24))
      (return format))))




