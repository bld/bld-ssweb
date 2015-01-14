;;; Library of common routines

(in-package :bld-ssweb)

(define-easy-handler (lib-js :uri "/js/lib.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))
    
    (defmacro 3xfn (fn &rest args)
      `((@ *t-h-r-e-ex ,fn) ,@args))

    (defun toggle-div (div-id)
      ((@ ($ (+ "#" div-id)) toggle)))
    
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
    (defvar sail)
    ;; Used for absorption and reflection
    (defvar mirror)
    (defvar corners)
    (defvar projection)
    (defvar incidence)
    (defvar rotation)
    (defvar absorbed)
    ;;(defvar tcontrols)

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

    (defun *sails (scene target-list parent)
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
		      ((@ parent add) obj)
		      ;;((@ scene add) obj)
		      ((@ target-list push) obj)
		      (render)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      this)

    (defun *vane (scene yrot x z target-list parent)
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
		      ((@ parent add) obj)
		      ;;((@ scene add) obj)
		      ((@ target-list push) obj)
		      (render)))
		load #'(lambda () ((@ loader load) file loadfn))))
	this))

    (defun *vanes (scene target-list parent)
      (loop for (x z) in '((1 0)(0 1)(-1 0)(0 -1))
	 for i = 0 then (incf i)
	 for yrot = (* i (/ pi 2))
	 for vane = (new (*vane scene yrot x z target-list parent))
	 collect vane))

    (defun *booms (scene target-list parent)
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
		    ((@ parent add) obj)
		    ;;((@ scene add) obj)
		    (render)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun *bus (scene target-list parent)
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
		    ((@ parent add) obj)
		    ;;((@ scene add) obj)
		    (render)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun on-window-resize ()
      (setf (@ camera aspect) (/ (@ window inner-width) (@ window inner-height)))
      ((@ camera update-projection-matrix))
      ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
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
      (if projection (setf (@ projection visible) false))
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
      (if projection (setf (@ projection visible) true))
      ;; Render scene
      ((@ renderer render) scene camera))

    (defun animate ()
      (request-animation-frame animate)
      ((@ controls update)))

    (defun init-orbit-controls (&optional (rfun #'render))
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
      ((@ controls add-event-listener) "change" rfun))

    (defun init-sail-parts ()
      ;; Parts of the sail
      (setq sail (new (3fn *object3-d)))
      ;; Booms
      (setq booms (new (*booms scene target-list sail)))
      ((@ booms load))
      ;; Bus
      (setq bus (new (*bus scene target-list sail)))
      ((@ bus load))
      ;; Sail
      (setq sails (new (*sails scene target-list sail)))
      ((@ sails load))
      ;; Vanes
      (setq vanes (new (*vanes scene target-list sail)))
      (dolist (vane vanes) ((@ vane load)))
      ;; Add sail to scene
      ((@ scene add) sail))
    
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

    ;; Absorption and reflection controls
    
    (defun *mirror ()
      (let* ((w 8)
	     (w2 (/ w 2))
	     (m (new (3fn *mesh-basic-material
			  (create 
			   color 0x444444
			   side (@ *three* *double-side)
			   wireframe false))))
	     (s (new (3fn *geometry))))
	(with-slots (vertices faces) s
	  ((@ vertices push)
	   (new (3fn *vector3 0 0 0))
	   (new (3fn *vector3 w2 0 0))
	   (new (3fn *vector3 0 0 w2))
	   (new (3fn *vector3 (- w2) 0 0))
	   (new (3fn *vector3 0 0 (- w2))))
	  ((@ faces push)
	   (new (3fn *face3 0 1 2))
	   (new (3fn *face3 0 2 3))
	   (new (3fn *face3 0 3 4))
	   (new (3fn *face3 0 4 1))))
	((@ s compute-face-normals))
	((@ s compute-bounding-sphere))
	(let ((o (new (3fn *mesh s m))))
	  (setf (@ o visible) false)
	  o)))
    
    (defun *projection (m)
      (let ((mat (new (3fn *line-basic-material (create color 0xffff00))))
	    (pg (new (3fn *geometry)))
	    (ymin -500))
	(with-slots ((pv vertices)) pg
	  (with-slots ((mv vertices)) (@ m geometry)
	    (let ((mv1 ((@ m local-to-world) ((@ mv 1 clone))))
		  (mv2 ((@ m local-to-world) ((@ mv 2 clone))))
		  (mv3 ((@ m local-to-world) ((@ mv 3 clone))))
		  (mv4 ((@ m local-to-world) ((@ mv 4 clone)))))
	      (let ((mv1b (new (3fn *vector3 (@ mv1 x) ymin (@ mv1 z))))
		    (mv2b (new (3fn *vector3 (@ mv2 x) ymin (@ mv2 z))))
		    (mv3b (new (3fn *vector3 (@ mv3 x) ymin (@ mv3 z))))
		    (mv4b (new (3fn *vector3 (@ mv4 x) ymin (@ mv4 z)))))
		((@ pv push)
		 mv1 mv2 mv3 mv4 mv1
		 mv1b mv2b mv3b mv4b mv1b
		 mv1 mv2 mv2b mv3b mv3 mv4 mv4b)
		(new (3fn *line pg mat))))))))

    (defun *corners (m)
      (with-slots ((mv vertices)) (@ m geometry)
	(list
	 ((@ m local-to-world) ((@ mv 1 clone)))
	 ((@ m local-to-world) ((@ mv 2 clone)))
	 ((@ m local-to-world) ((@ mv 3 clone)))
	 ((@ m local-to-world) ((@ mv 4 clone))))))

    (defun rotate-global-y (object rad)
      (let* ((gy (new (3fn *vector3 0 1 0)))
	     (ly ((@ object world-to-local) gy)))
	((@ object rotate-on-axis) ly rad)))

    (defun update-tilt ()
      ;; Update rotation matrix of sail
      ((@ sail update-matrix-world))
      ;; Update corners of sail positions
      (setq corners (new (*corners mirror)))
      ;; Update projection object
      ((@ scene remove) projection)
      (setq projection (new (*projection mirror)))
      ((@ scene add) projection)
      ;; Update angle fields
      (setf (@ ($ "#incidence") 0 inner-h-t-m-l) ((@ incidence to-string)))
      (setf (@ ($ "#rotation") 0 inner-h-t-m-l) ((@ rotation to-string)))
      (setf (@ ($ "#absorbed") 0 inner-h-t-m-l) ((@ absorbed to-string)))
      ;; Re-render
      (render))

    (defun init-tilt-controls ()
      ;; Initialize values
      (setq incidence 0
	    rotation 0
	    absorbed 100)
      ;; Arrow key events
      ((@ ($ (@ document body)) on) "keydown"
       #'(lambda (e)
	   (case (@ e which)
	     ;; Left
	     (37 (rotate-global-y sail (/ pi 36))
		 (decf rotation 5)
		 (update-tilt))
	     ;; Up
	     (38 ((@ sail rotate-z) (/ pi 36))
		 (incf incidence 5)
		 (setq absorbed (round (* 100 (cos (* incidence (/ pi 180))))))
		 (update-tilt))
	     ;; Right
	     (39 (rotate-global-y sail (/ pi -36))
		 (incf rotation 5)
		 (update-tilt))
	     ;; Down
	     (40 ((@ sail rotate-z) (/ pi -36))
		 (decf incidence 5)
		 (setq absorbed (round (* 100 (cos (* incidence (/ pi 180))))))
		 (update-tilt)))))
      ;; Clicking on arrow images
      ((@ ($ "#up") click)
       #'(lambda (e)
	   ((@ sail rotate-z) (/ pi 36))
	   (incf incidence 5)
	   (setq absorbed (round (* 100 (cos (* incidence (/ pi 180))))))
	   (update-tilt)))
      ((@ ($ "#down") click)
       #'(lambda (e)
	   ((@ sail rotate-z) (/ pi -36))
	   (decf incidence 5)
	   (setq absorbed (round (* 100 (cos (* incidence (/ pi 180))))))
	   (update-tilt)))
      ((@ ($ "#left") click)
       #'(lambda (e)
	   (rotate-global-y sail (/ pi 36))
	   (decf rotation 5)
	   (update-tilt)))
      ((@ ($ "#right") click)
       #'(lambda (e)
	   (rotate-global-y sail (/ pi -36))
	   (incf rotation 5)
	   (update-tilt))))
    ))
