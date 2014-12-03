;;; Library of common routines

(in-package :bld-ssweb)

(define-easy-handler (lib-js :uri "/js/lib.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))
    
    (defmacro 3xfn (fn &rest args)
      `((@ *t-h-r-e-ex ,fn) ,@args))

    (defvar scene)
    (defvar renderer)
    (defvar camera)
    (defvar plot-div)
    (defvar origin)
    (defvar sails)
    (defvar booms)
    (defvar vanes)
    (defvar bus)
    (defvar controls)
    (defvar light)
    (defvar alight)
    (defvar stars)
    (defvar sun)
    (defvar projector)
    (defvar target-list)
    (defvar mouse)
    (defvar part-name)

    (defun *stars ()
      (let ((g (new (3fn *geometry)))
	    (m (new (3fn *point-cloud-material
			 (create size 2))))
	    (r 500))
	(dotimes (i 10000)
	  (let ((v (new (3fn *vector3
			     (- ((@ *math random)) 0.5)
			     (- ((@ *math random)) 0.5)
			     (- ((@ *math random)) 0.5)))))
	    ((@ v set-length) (* (+ 1 ((@ *math random))) r))
	    ((@ g vertices push) v)))
	(new (3fn *point-cloud g m))))
    
    (defun *sun ()
      (let ((g (new (3fn *sphere-geometry 20 32 32)))
	    (m (new (3fn *mesh-basic-material (create color 0xf9ffd9)))))
	(new (3fn *mesh g m))))

    (defun *selection-box (name x y z)
      (let ((sbox
	     (new (3fn *mesh
		       (new (3fn *box-geometry x y z))
		       (new (3fn *mesh-basic-material
				 (create color 0xffffff
					 wireframe true)))))))
	(setf (@ sbox name) name)
	sbox))

    (defun *sails (scene target-list)
      (with-slots (loader file loadfn obj load mcam) this
	;; Mirror camera
	(let ((mirror-camera (new (3fn *cube-camera 1 5000 512))))
	  (setf mcam mirror-camera)
	  ((@ scene add) mcam)
	  ;; Sail geometry
	  (setf loader (new (3fn -j-s-o-n-loader t))
		file "js/sails.js"
		loadfn
		#'(lambda (geometry materials)
		    (let* ((mat
			    (new
			     (3fn *mesh-phong-material
				  (create
				   env-map (@ mirror-camera render-target)
				   reflectivity 0.9))))
			   (obj (new (3fn *mesh geometry mat))))
		      (setf (@ obj name) "sails")
		      (setf (@ sails obj) obj)
		      ((@ scene add) obj)
		      ((@ target-list push) obj)
		      (render)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      this)

    (defun *vane (scene yrot x z target-list)
      (with-slots (loader file loadfn obj load mcam select-obj) this
	;; Selection objects
	(setf select-obj (new (*selection-box "vanes" .5 1 1)))
	(setf (@ select-obj position x) (* 4.25 x)
	      (@ select-obj position z) (* 4.25 z)
	      (@ select-obj visible) false)
	((@ select-obj rotate-y) yrot)
	((@ scene add) select-obj)
	((@ target-list push) select-obj)
	;; Load vane geometry from file, with cube camera reflection
	(let ((vane-camera (new (3fn *cube-camera 1 1000 256)))
	      (vane this))
	  (setf mcam vane-camera)
	  (setf loader (new (3fn -j-s-o-n-loader t))
		file "js/vane.js"
		loadfn
		#'(lambda (geometry materials)
		    (let* ((mat
			    (new
			     (3fn *mesh-phong-material
				  (create
				   env-map (@ vane-camera render-target)
				   reflectivity 0.9
				   ;;side (@ *t-h-r-e-e *double-side)
				   ))))
			   (obj (new (3fn *mesh geometry mat))))
		      (setf (@ obj name) "vanes")
		      (setf (@ vane obj) obj)
		      ((@ obj rotate-y) yrot)
		      ((@ scene add) obj)
		      ((@ target-list push) obj)
		      (render)))
		load #'(lambda () ((@ loader load) file loadfn))))
	this))

    (defun *vanes (scene target-list)
      (loop for (x z) in '((1 0)(0 1)(-1 0)(0 -1))
	 for i = 0 then (incf i)
	 for yrot = (* i (/ pi 2))
	 for vane = (new (*vane scene yrot x z target-list))
	 collect vane))

    (defun *booms (scene target-list)
      (with-slots (loader file loadfn obj load select-obj1 select-obj2) this
	;; Selection objects
	;; 1st set of booms
	(setf select-obj1 (new (*selection-box "booms" .4 .2 8)))
	(setf (@ select-obj1 visible) false)
	((@ scene add) select-obj1)
	((@ target-list push) select-obj1)
	;; 2nd set of booms
	(setf select-obj2 (new (*selection-box "booms" 8 .2 .4)))
	(setf (@ select-obj2 visible) false)
	((@ scene add) select-obj2)
	((@ target-list push) select-obj2)
	;; Load booms geometry from file
	(setf loader (new (3fn -j-s-o-n-loader t))
	      file "js/booms.js"
	      loadfn
	      #'(lambda (geometry materials)
		  (let* ((mats (new (3fn *mesh-face-material materials)))
			 (obj (new (3fn *mesh geometry mats))))
		    (setf (@ booms obj) obj)
		    ((@ scene add) obj)
		    (render)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun *bus (scene target-list)
      (with-slots (loader file loadfn obj load select-obj) this
	;; Selection object
	(setf select-obj (new (*selection-box "bus" 1 1 1)))
	(setf (@ select-obj visible) false)
	((@ scene add) select-obj)
	((@ select-obj rotate-y) (/ pi 4))
	((@ target-list push) select-obj)
	;; Bus geometry loaded from file
	(setf loader (new (3fn -j-s-o-n-loader t))
	      file "js/bus.js"
	      loadfn
	      #'(lambda (geometry materials)
		  (let* ((mats (new (3fn *mesh-face-material materials)))
			 (obj (new (3fn *mesh geometry mats))))
		    (setf (@ bus obj) obj)
		    ((@ scene add) obj)
		    (render)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun *sail (scene target-list)
      (let ((o (new (3fn *object3-d))))
	((@ o add) (@ (new (*sails scene target-list)) obj))
	((@ o add) (@ (new (*bus scene target-list)) obj))
	((@ o add) (@ (new (*vanes scene target-list)) obj))
	((@ o add) (@ (new (*booms scene target-list)) obj))
	o))

    (defun on-window-resize ()
      (setf (@ camera aspect) (/ window.inner-width window.inner-height))
      ((@ camera update-projection-matrix))
      ((@ renderer set-size) window.inner-width window.inner-height)
      ((@ camera look-at) origin)
      (when controls ((@ controls handle-resize)))
      (render))

    (defun render ()
      ;; Make sail parts invisible
      (if (@ sails obj) (setf (@ sails obj visible) false))
      (if (@ booms obj) (setf (@ booms obj visible) false))
      (if (@ bus obj) (setf (@ bus obj visible) false))
      (dolist (vane vanes)
	(if (@ vane obj) (setf (@ vane obj visible) false)))
      ;; Update cube cameras
      ((@ sails mcam update-cube-map) renderer scene)
      (dolist (vane vanes)
	((@ vane mcam update-cube-map) renderer scene))
      ;; Reset visibility
      (if (@ sails obj) (setf (@ sails obj visible) true))
      (if (@ booms obj) (setf (@ booms obj visible) true))
      (if (@ bus obj) (setf (@ bus obj visible) true))
      (dolist (vane vanes)
	(if (@ vane obj) (setf (@ vane obj visible) true)))
      ;; Render scene
      ((@ renderer render) scene camera))

    (defun animate ()
      (request-animation-frame animate)
      ((@ controls update)))

    (defun init-orbit-controls ()
      ;; Orbit controls
      (setq controls (new (3fn *trackball-controls camera)))
      (setf 
       (@ controls rotate-speed) 2
       ;;(@ controls zoom-speed) 2
       ;;(@ controls pan-speed) 0.8
       (@ controls no-pan) true
       (@ controls no-zoom) false
       ;;(@ controls static-moving) true
       ;;(@ controls dynamic-damping-factor) 0.3
       ;;(@ controls keys) (list 65 83 68)
       )
      ((@ controls add-event-listener) "change" #'render))

    (defun init-sail-parts ()
      ;; Parts of the sail
      ;; Booms
      (setq booms (new (*booms scene target-list)))
      ((@ booms load))
      ;; Bus
      (setq bus (new (*bus scene target-list)))
      ((@ bus load))
      ;; Sail
      (setq sails (new (*sails scene target-list)))
      ((@ sails load))
      ;; Vanes
      (setq vanes (new (*vanes scene target-list)))
      (dolist (vane vanes) ((@ vane load))))

    (defun init (document window)
      ;; Scene
      (setq scene (new (3fn *scene)))
      ;; Center coordinate
      (setq origin (new (3fn *vector3 0 0 0)))
      ;; DOM element to put plot in
      (setq plot-div ((@ document get-element-by-id) "plot"))
      ;; Renderer
      (setq renderer (if (@ window *web-g-l-rendering-context)
			 (new (3fn *web-g-l-renderer (create antialias true)))
			 (new (3fn *canvas-renderer))))
      ((@ renderer set-size) window.inner-width window.inner-height)
      ((@ plot-div append-child) (@ renderer dom-element))
      ;; Scene camera
      (setq camera (new (3fn *perspective-camera 75 (/ window.inner-width window.inner-height) 0.1 1000)))
      ((@ camera position set) 2 -6 3)
      ((@ camera look-at) origin)
      ((@ scene add) camera)
      ;; Directional light
      (setq light (new (3fn *directional-light 0xffffff 2)))
      ((@ light position set) 0 -1 0)
      ((@ scene add) light)
      ;; Ambient light
      (setq alight (new (3fn *ambient-light 0xffffff)))
      ((@ scene add) alight)
      ;; Stars
      (setq stars (new (*stars)))
      ((@ scene add) stars)
      ;; Sun
      (setq sun (new (*sun)))
      ((@ sun translate-y) -500)
      ((@ scene add) sun)
      ;; Setup clicking parts of sail
      (setq target-list (list))
      (setq projector (new (3fn *projector)))
      (setq mouse (create x 0 y 0))
      ;;((@ document add-event-listener) "click" on-document-mouse-click false)
      ;; Automatically resize window
      ((@ window add-event-listener) "resize" on-window-resize false))

    (defun toggle-div (div-id)
      ((@ ($ (+ "#" div-id)) toggle)))

    ))
