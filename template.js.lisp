(in-package :bld-ssweb)

(define-easy-handler (template-js :uri "/js/template.js") ()
  (setf (content-type*) "text/javascript")
  
  (ps

    ;; requestAnimationFrame support
    
    (lambda ()

      (let ((last-time 0)
	    (vendors (list "ms" "moz" "webkit" "o")))

	(loop for x = 0 then (incf x)
	   while (and (< x (@ vendors length))
		      (not (@ window request-animation-frame)))
	   do (setf (@ window request-animation-frame)
		    (aref window (concatenate 'string (aref vendors x) "RequestAnimationFrame")))
	     (setf (@ window cancel-animation-frame)
		   (or (aref window (concatenate 'string (aref vendors x) "CancelAnimationFrame"))
		       (aref window (concatenate 'string (aref vendors x) "CancelRequestAnimationFrame")))))

	(when (not (@ window request-animation-frame))
	  (setf (@ window request-animation-frame)
		(lambda (callback element)
		  (let* ((curr-time (new ((@ (*date) get-time))))
			 (time-to-call ((@ *math max) 0 (- 16 (- curr-time last-time))))
			 (id ((@ window set-timeout)
			      (lambda ()
				(callback (+ curr-time time-to-call)))
			      time-to-call)))
		    (setq last-time (+ curr-time time-to-call))
		    id))))

	(when (not (@ window cancel-animation-frame))
	  (setf (@ window cancel-animation-frame)
		(lambda (id)
		  (clear-timeout id))))))

    ;; Application object
    (defvar *application
      (lambda (id)
	(let ((self
	       (create
		;; layer id
		id id
		canvas undefined
		renderer undefined
		;; canvas size
		width 640
		height 480

		keys (create
		      up false
		      down false
		      left false
		      right false)
		clock undefined)))
	  (setf (@ self setup)
		(lambda ()
		  ;; Setup the scene
		  (setf (@ self canvas) ((@ document get-element-by-id) (@ self id)))
		  ;; detect webgl support
		  (when (@ window *web-g-l-rendering-context)
		    (with-slots (renderer scene world camera width height) self
		      (setf renderer (new ((@ *three* *web-g-l-renderer))))
		      ((@ renderer set-size) width height)
		      (setf scene (new ((@ *three* *scene))))
		      ((@ scene add) (new ((@ *three* *ambient-light) 0x222222)))
		      (setf world (new ((@ *three* *object-3-d))))

		      ;; generate your scene
		      (let ((obj (new ((@ *three* *mesh)
				       (new ((@ *three* *cube-geometry) 1 1 1))
				       (new ((@ *three* *mesh-lambert-material)
					     (create color 0xcc0000)))))))
			((@ world add) obj))
		      (let ((aspect (/ width height)))
			(setf camera (new ((@ *three* *perspective-camera) 45 aspect 0.01 100))))

		      ;; Make the light follow the camera
		      ((@ camera add) (new ((@ *three* *point-light) 0xffffff 1 100)))
		      ((@ camera position set) 0 0 4)
		      ((@ scene add) camera)
		      ((@ scene add) world)

		      ;; Finally add the renderer and the clock
		      ((@ canvas append-child) (@ renderer dom-element))
		      (setf clock (new ((@ *three* *clock))))
		      t)
		    false)))

	  (setf (@ self draw)
		(lambda ()
		  (with-slots (renderer scene camera) self
		    ((@ renderer render) scene camera))))

	  (setf (@ self update)
		(lambda (dt)
		  (with-slots (keys world) self
		    (with-slots (left right up down) keys
		      (when left )
		      (when right )
		      (when up )
		      (when down ))
		    (with-slots (rotation) world
		      (incf (@ rotation y) dt)
		      (incf (@ rotation z) (* dt 0.8))))))

	  (setf (@ self loop)
		(lambda ()
		  ;; application loop:
		  ;; request animation frame
		  ;; get time delta
		  ;; update and draw scene
		  (request-animation-frame (@ self loop))
		  (let ((dt ((@ self clock get-delta))))
		    ((@ self update) dt)
		    ((@ self draw)))))

	  (setf (@ self run)
		(lambda ()
		  ;; application entry point
		  (if ((@ self setup))
		      ((@ self loop))
		      (setf (@ self canvas inner-h-t-m-l)
			    "<p>This page requires <a href=\"http://get.webgl.org/\">WebGL</a>!</p>"))))

	  (setf (@ self key_down)
		(lambda (event)
		  (with-slots (keys) self
		    (with-slots (left right up down) keys
		      (case (@ event key-code)
			(37 (setf left t))
			(39 (setf right t))
			(38 (setf up t))
			(40 (setf down t)))))))

	  (setf (@ self key_up)
		(lambda (event)
		  (with-slots (keys) self
		    (with-slots (left right up down) keys
		      (case (@ event key-code)
			(37 (setf left false))
			(39 (setf right false))
			(38 (setf up false))
			(40 (setf down false)))))))

	  (setf (@ self init)
		(lambda ()
		  ;; Application object constructor, add here any app initialization
		  ;; that is webgl independent (ie set event listeners)
		  ((@ document add-event-listener) "keydown" (@ self key_down) false)
		  ((@ document add-event-listener) "keyup" (@ self key_up) false)))

	  ((@ self init))

	  self)))

    (setf (@ window onload)
	  (lambda ()
	    (let ((app (*application "target")))
	      ((@ app run)))))))
