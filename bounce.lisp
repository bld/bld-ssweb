(in-package :bld-ssweb)

(define-easy-handler (bounce :uri "/bounce.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "Photons Bouncing off of a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Photons Bouncing off of a Solar Sail"))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "Rotate the view by clicking or touching and dragging.")
		  (:li "To zoom:"
		       (:ul
			(:li "Roll a mouse wheel")
			(:li "Hold the middle mouse button and move up and down")
			(:li "Pinch and spread two fingers on a touch screen")))
		  (:li "To steer the sail:"
		       (:ul
			(:li "Up and down arrows to tilt")
			(:li "Right and left arrows to rotate")))))
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (defvar time)

	  (defun *photon ()
	    (let ((g (new (3fn *sphere-geometry 1 16 16)))
		  (m (new (3fn *mesh-basic-material (create color 0xffff00)))))
	      (new (3fn *mesh g m))))

	  (defun init()
  	    (setq scene (new (3fn *scene)))
	    (setq origin (new (3fn *vector3 0 0 0)))
	    (setq plot-div ((@ document get-element-by-id) "plot"))
	    (setq renderer (if (@ window *web-g-l-rendering-context)
			       (new (3fn *web-g-l-renderer (create antialias true)))
			       (new (3fn *canvas-renderer))))
	    ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
	    ((@ plot-div append-child) (@ renderer dom-element))
	    (setq camera (new (3fn *perspective-camera 75 (/ window.inner-width window.inner-height) 0.1 1000)))
	    ((@ camera position set) 10 -10 10)
	    ((@ camera look-at) origin)
	    ((@ scene add) camera)
	    (setq photon (new (*photon)))
	    ((@ scene add) photon)
	    (setq photon-start -10)
	    ((@ photon position set) 0 photon-start 0)
	    (setq photon-velocity 0.02))
	  
	  (defun render-bounce ()
	    ((@ renderer render) scene camera))

	  (defun update-bounce ()
	    (let* ((now ((@ (new (*date)) get-time)))
		   (dt (- now (or time now)))
		   (y (@ photon position y)))
	      (setq time now)
	      ((@ photon position set) 0 (+ y (* dt photon-velocity)) 0)
	      (when (and (> y 0) (> photon-velocity 0))
		(setq photon-velocity (- photon-velocity))
		((@ photon position set) 0 0 0))
	      (when (and (< y photon-start) (< photon-velocity 0))
		(setq photon-velocity (- photon-velocity))
		((@ photon position set) 0 photon-start 0))))
	  
	  (defun animate-bounce ()
	    (request-animation-frame animate-bounce)
	    (update-bounce)
	    (render-bounce))

	  (init)
	  (animate-bounce))))))))
