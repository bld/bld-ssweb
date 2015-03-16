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

    (setf (@ *function prototype method)
	  (lambda (name func)
	    (when (not (elt (@ this prototype) name))
	      (setf (elt (@ this prototype) name) func)
	      this)))

    ((@ *object method) "superior"
     (lambda (name)
       (let* ((that this)
	      (method (elt that name)))
	 (lambda ()
	   ((@ method apply) that arguments)))))

    (defun flightschool ()
      "Flight school app"
      (let ((that (create)))
	(with-slots (scene origin plot-div renderer camera light alight stars sun target-list
			   sail booms bus sails vanes
			   render init on-window-resize) that
	  
	  ;; Scene
	  (setf scene (new (3fn *scene)))

	  ;; Center coordinate
	  (setf origin (new (3fn *vector3 0 0 0)))

	  ;; DOM element to put plot in
	  (setf plot-div ((@ document get-element-by-id) "plot"))

	  ;; Renderer
	  (setf renderer (if (@ window *web-g-l-rendering-context)
			     (new (3fn *web-g-l-renderer (create antialias true)))
			     (new (3fn *canvas-renderer))))
	  ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
	  ((@ plot-div append-child) (@ renderer dom-element))

	  ;; Scene camera
	  (setf camera (new (3fn *perspective-camera 75 (/ (@ window inner-width) (@ window inner-height)) 0.1 1000)))
	  ((@ camera position set) 2 -6 3)
	  ((@ camera look-at) origin)
	  ((@ scene add) camera)

	  ;; Directional light
	  (setf light (new (3fn *directional-light 0xffffff 2)))
	  ((@ light position set) 0 -1 0)
	  ((@ scene add) light)

	  ;; Ambient light
	  (setf alight (new (3fn *ambient-light 0xffffff)))
	  ((@ scene add) alight)

	  ;; Stars
	  (setf stars (new (*stars)))
	  ((@ scene add) stars)

	  ;; Sun
	  (setf sun (new (*sun)))
	  ((@ sun translate-y) -500)
	  ((@ scene add) sun)

	  ;; Target list
	  (setf target-list (list))

	  ;; Parts of the sail
	  (setf sail (new (3fn *object3-d)))

	  ;; Toggle sail parts visibility
	  (setf sail-parts-visible
		(lambda (bool)
		  (dolist (p (@ sail children))
		    (if (@ p obj) (setf (@ p obj visible) bool)))))
	  
	  ;; Render method
	  (setf render
		(lambda ()
		  ;; Make sail parts invisible
		  (funcall sail-parts-visible false)
		  ;; Update mirror cube cameras
		  (when sails
		    ((@ sails mcam update-cube-map) renderer scene))
		  (when vanes
		    (dolist (vane vanes)
		      ((@ vane mcam update-cube-map) renderer scene)))
		  ;; Make sail parts visible
		  (funcall sail-parts-visible true)
		  ;; Render
		  ((@ renderer render) scene camera)))

	  ;; Booms
	  (setf booms (create))
	  (with-slots (loader file loadfn obj load select-obj1 select-obj2) booms
	    ;; Selection objects
	    ;; 1st set of booms
	    (setf select-obj1 (new (*selection-box "booms" .4 .2 8)))
	    (setf (@ select-obj1 visible) false)
	    ((@ scene add) select-obj1)
	    ;; 2nd set of booms
	    (setf select-obj2 (new (*selection-box "booms" 8 .2 .4)))
	    (setf (@ select-obj2 visible) false)
	    ((@ scene add) select-obj2)
	    ;; Load booms geometry from file
	    (setf loader (new (3fn -j-s-o-n-loader t))
		  file "js/booms.js"
		  loadfn
		  #'(lambda (geometry materials)
		      (let* ((mats (new (3fn *mesh-face-material materials)))
			     (obj (new (3fn *mesh geometry mats))))
			(setf (@ booms obj) obj)
			((@ sail add) obj)
			;;((@ scene add) obj)
			(funcall render)))
		  load #'(lambda () ((@ loader load) file loadfn))))

	  ;; Bus
	  (setf bus (create))
	  (with-slots (loader file loadfn obj load select-obj) bus
	    ;; Selection object
	    (setf select-obj (new (*selection-box "bus" 1 1 1)))
	    (setf (@ select-obj visible) false)
	    ((@ scene add) select-obj)
	    ((@ select-obj rotate-y) (/ pi 4))
	    ;; Bus geometry loaded from file
	    (setf loader (new (3fn -j-s-o-n-loader t))
		  file "js/bus.js"
		  loadfn
		  #'(lambda (geometry materials)
		      (let* ((mats (new (3fn *mesh-face-material materials)))
			     (obj (new (3fn *mesh geometry mats))))
			(setf (@ bus obj) obj)
			((@ sail add) obj)
			;;((@ scene add) obj)
			(funcall render)))
		  load #'(lambda () ((@ loader load) file loadfn))))
	  ((@ bus load))

	  ;; Sails
	  (setf sails (create))
	  (with-slots (loader file loadfn obj load mcam select-obj) sails
	    ;; Selection box
	    (setf select-obj (new (*selection-box "sails" (* 4 (sqrt 2)) 0 (* 4 (sqrt 2)))))
	    (setf (@ select-obj visible) false)
	    ((@ select-obj rotate-y) (/ pi 4))
	    ((@ scene add) select-obj)
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
			  ((@ sail add) obj)
			  ;;((@ scene add) obj)
			  (funcall render)))
		    load #'(lambda () ((@ loader load) file loadfn)))))

	  ;; Vanes
	  (setf
	   vanes
	   (loop for (x z) in '((1 0)(0 1)(-1 0)(0 -1))
	      for i = 0 then (incf i)
	      for yrot = (* i (/ pi 2))
	      collect
		(let ((vane (create)))
		  (with-slots (loader file loadfn obj load mcam select-obj) vane
		    ;; Selection objects
		    (setf select-obj (new (*selection-box "vanes" .5 1 1)))
		    (setf (@ select-obj position x) (* 4.25 x)
			  (@ select-obj position z) (* 4.25 z)
			  (@ select-obj visible) false)
		    ((@ select-obj rotate-y) yrot)
		    ((@ scene add) select-obj)
		    ;; Load vane geometry from file, with cube camera reflection
		    (setf mcam (new (3fn *cube-camera 1 1000 256)))
		    (setf loader (new (3fn -j-s-o-n-loader t))
			  file "js/vane.js"
			  loadfn
			  #'(lambda (geometry materials)
			      (let* ((mat (new (3fn *mesh-phong-material
						    (create
						     env-map (@ mcam render-target)
						     reflectivity 0.9)))))
				(setf obj (new (3fn *mesh geometry mat)))
				(setf (@ obj name) "vanes")
				((@ obj rotate-y) yrot)
				((@ sail add) obj)
				((@ scene add) obj)
				(funcall render)))
			  load #'(lambda () ((@ loader load) file loadfn))))
		  vane)))
	  
	  ;; Add sail to scene
	  ((@ scene add) sail)

	  ;; Window resize
	  (setf on-window-resize
		(lambda ()
		  (setf (@ camera aspect) (/ (@ window inner-width) (@ window inner-height)))
		  ((@ camera update-projection-matrix))
		  ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
		  ((@ camera look-at) origin)
		  (funcall render)))

	  ;; Init method
	  (setf init
		(lambda ()
		  ((@ window add-event-listener) "resize" on-window-resize false)
		  ((@ booms load))
		  ((@ sails load))
		  (dolist (vane vanes) ((@ vane load)))))
	  )
	that))

    (defun what ()
      "What is a Solar Sail app"
      (let ((that (flightschool)))
	(with-slots (controls camera render animate) that
	  (setf controls (new (3fn *trackball-controls camera)))
	  (setf
	   (@ controls no-pan) true
	   (@ controls no-zoom) false)
	  ((@ controls add-event-listener) "change" render)
	  (setf animate
		(lambda ()
		  (request-animation-frame animate)
		  ((@ controls update)))))
	that))

    (defun parts ()
      "Parts of a Solar Sail app"
      (let ((that (what)))
	(with-slots (init on-parts-click target-list booms sails bus vanes mouse part-name camera) that
	  ;; Initialization function: click event
	  (let ((super-init ((@ that superior) "init")))
	    (setf init
		  (lambda ()
		    (funcall super-init)
		    ((@ document add-event-listener) "click" on-parts-click false))))
	  ;; Populate target list for mouse clicking
	  (setf target-list
		(list (@ booms select-obj1)
		      (@ booms select-obj2)
		      (@ bus select-obj)
		      (@ sails select-obj)))
	  (dolist (vane vanes)
	    ((@ target-list push) (@ vane select-obj)))
	  ;; Mouse for clicking parts
	  (setf mouse (create x 0 y 0))
	  ;; Event function for mouse click
	  (setf on-parts-click
		(lambda (event)
		  (setf (@ mouse x) (- (* 2 (/ (@ event client-x) window.inner-width)) 1))
		  (setf (@ mouse y) (- 1 (* 2 (/ (@ event client-y) window.inner-height))))
		  (let ((vector (new (3fn *vector3 (@ mouse x) (@ mouse y) 1))))
		    ((@ vector unproject) camera)
		    (let* ((raycaster (new (3fn *raycaster (@ camera position) ((@ ((@ vector sub) (@ camera position)) normalize)))))
			   (intersects ((@ raycaster intersect-objects) target-list)))
		      (if (> (@ intersects length) 0)
			  (let ((part-name-select (@ intersects 0 object name)))
			    (unless (equal part-name-select part-name)
			      ;; Hide original part description
			      (when part-name (setf (@ ((@ document get-element-by-id) part-name) class-name) "hidden"))
			      ;; Un-hide select part description
			      (setf (@ ((@ document get-element-by-id) part-name-select) class-name) "")
			      ;; Set part name for next time
			      (setq part-name part-name-select)))
			  (progn
			    (when part-name (setf (@ ((@ document get-element-by-id) part-name) class-name) "hidden"))
			    (setf part-name ""))))))))
	that))

    (defun absorb ()
      "Absorbed light on a sail app"
      (let ((that (what)))
	(with-slots (camera mirror sail corners projection tilt-update-fn rotation incidence absorbed) that
	  (setf rotation 0 incidence 0 absorbed 100)
	  ((@ camera position set) -4 -6 6)
	  (setf mirror (new (*mirror)))
	  ((@ sail add) mirror)
	  (setf corners (new (*corners mirror)))
	  (setf projection (new (*projection mirror)))
	  ((@ scene add) projection)
	  (setf tilt-update-fn 
		(lambda ()
		  ;; Sail rotation matrix
		  ((@ sail update-matrix-world))
		  ;; Corners of the sail
		  (setf corners (new (*corners mirror)))
		  ;; Projection
		  ((@ scene remove) projection)
		  (setf projection (new (*projection mirror)))
		  ((@ scene add) projection)
		  ;; Angle fields
		  (setf (@ ($ "#incidence") 0 inner-h-t-m-l) ((@ incidence to-string)))
		  (setf (@ ($ "#rotation") 0 inner-h-t-m-l) ((@ rotation to-string)))
	that))
    
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
    (defvar reflect)
    (defvar incident)
    (defvar reflection)
    (defvar tilt-update-fn)
    (defvar target)
    (defvar absorb-arrow)
    (defvar reflection-arrow)
    ;; Direction arrows & variables
    (defvar normal-arrow)
    (defvar incident-arrow)
    (defvar tangential)
    (defvar tangential-arrow)
    ;; Acceleration
    (defvar time)
    (defvar elapsed)
    (defvar timefactor)
    (defvar pause)

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
      "Selection box to click parts of the sail"
      (let ((sbox
	     (new (3fn *mesh
		       (new (3fn *box-geometry x y z))
		       (new (3fn *mesh-basic-material
				 (create color 0xffffff
					 wireframe true)))))))
	(setf (@ sbox name) name)
	sbox))

    (defun *sails (scene target-list parent &optional (rfun #'render) (sails sails))
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
		      (funcall rfun)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      this)

    (defun *vane (scene yrot x z target-list parent &optional (rfun render))
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
		      (funcall rfun)))
		load #'(lambda () ((@ loader load) file loadfn))))
	this))

    (defun *vanes (scene target-list parent &optional (rfun #'render))
      "List of 4 vanes"
      (loop for (x z) in '((1 0)(0 1)(-1 0)(0 -1))
	 for i = 0 then (incf i)
	 for yrot = (* i (/ pi 2))
	 for vane = (new (*vane scene yrot x z target-list parent rfun))
	 collect vane))

    (defun *booms (scene target-list parent &optional (rfun #'render) (booms booms))
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
		    (funcall rfun)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun *bus (scene target-list parent &optional (rfun #'render) (bus bus))
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
		    (funcall rfun)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun on-window-resize ()
      "Update scene on window resize"
      (setf (@ camera aspect) (/ (@ window inner-width) (@ window inner-height)))
      ((@ camera update-projection-matrix))
      ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
      ((@ camera look-at) origin)
      (when controls ((@ controls handle-resize)))
      (render))

    (defun sail-parts-visible (bool)
      "Change visibility of sail parts"
      (when sail
	(dolist (p (@ sail children))
	  (if (@ p obj) (setf (@ p obj visible) bool)))))

    (defun render ()
      ;; Make sail parts invisible
      (sail-parts-visible false)
      ;; Make projection/reflection parts invisible
      (if projection (setf (@ projection visible) false))
      (if reflection
	  (dolist (r (@ reflection children))
	    (setf (@ r visible) false)))
      (arrows-visible false)
      ;; Update cube cameras
      (when sails
	((@ sails mcam update-cube-map) renderer scene))
      (when vanes
	(dolist (vane vanes)
	  ((@ vane mcam update-cube-map) renderer scene)))
      ;; Reset visibility
      (sail-parts-visible true)
      (if projection (setf (@ projection visible) true))
      (if reflection
	  (dolist (r (@ reflection children))
	    (setf (@ r visible) true)))
      (arrows-visible true)
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
      ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
      ((@ plot-div append-child) (@ renderer dom-element))
      ;; Scene camera
      (setq camera (new (3fn *perspective-camera 75 (/ (@ window inner-width) (@ window inner-height)) 0.1 1000)))
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
      ;; Automatically render when window resized
      ((@ window add-event-listener) "resize" on-window-resize false))

    ;; Absorption and reflection controls
    
    (defun *mirror ()
      "Invisible object representing the corners of the sail"
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
      "Projection of that portion of the sunlight that hits the sail"
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
      "List of corner points of a mirror object in world coordinates"
      (with-slots ((mv vertices)) (@ m geometry)
	(list
	 ((@ m local-to-world) ((@ mv 1 clone)))
	 ((@ m local-to-world) ((@ mv 2 clone)))
	 ((@ m local-to-world) ((@ mv 3 clone)))
	 ((@ m local-to-world) ((@ mv 4 clone))))))

    (defun rotate-global-y (object rad)
      "Rotate an object around the global Y axis by the given number of radians"
      (let* ((gy (new (3fn *vector3 0 1 0)))
	     (ly ((@ object world-to-local) gy)))
	((@ ly normalize))
	((@ object rotate-on-axis) ly rad)))

    (defun init-absorb ()
      "Initialize absorb lesson specific objects"
      ((@ camera position set) -4 -6 6)
      (setq mirror (new (*mirror)))
      ((@ sail add) mirror)
      (setq corners (new (*corners mirror)))
      (setq projection (new (*projection mirror)))
      ((@ scene add) projection)
      (setq tilt-update-fn #'update-tilt-absorb)
      (setq incident (new (*incident)))
      (setq absorb-arrow (new (3fn *arrow-helper incident origin 10 0xffff00)))
      ((@ scene add) absorb-arrow))
    
    (defun update-tilt ()
      "When the sail is tilted, call tilt-update-fn and re-render the scene"
      (funcall tilt-update-fn)
      (render))
    
    (defun update-tilt-absorb ()
      "Update the absorb lesson in response to tilting the sail"
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
      (let ((absorbed-html (@ ($ "#absorbed") 0)))
	(when absorbed-html
	  (setf (@ absorbed-html inner-h-t-m-l) ((@ absorbed to-string)))))
      ;; Update absorbed arrow
      ((@ absorb-arrow set-length) (* 10 (/ absorbed 100))))

    (defun calc-absorbed (incidence)
      "Calculate the absorbed percentage from the incidence angle"
      (abs ((@ *math round10) (* 100 (cos (* incidence (/ pi 180)))) -1)))

    (defun up ()
      "Increment incidence"
      ((@ sail rotate-z) (/ pi 36))
      (incf incidence 5)
      (when (> incidence 180)
	(setq incidence (- incidence 360)))
      (setq absorbed (calc-absorbed incidence))
      (update-tilt))

    (defun down ()
      "Decrement incidence"
      ((@ sail rotate-z) (/ pi -36))
      (decf incidence 5)
      (when (< incidence -180)
	(setq incidence (+ 360 incidence)))
      (setq absorbed (calc-absorbed incidence))
      (update-tilt))

    (defun left ()
      "Decrement rotation"
      (rotate-global-y sail (/ pi 36))
      (decf rotation 5)
      (when (< rotation -180)
	(setq rotation (+ 360 rotation)))
      (update-tilt))

    (defun right ()
      "Increment rotation"
      (rotate-global-y sail (/ pi -36))
      (incf rotation 5)
      (when (> rotation 180)
	(setq rotation (- rotation 360)))
      (update-tilt))

    (defun tilt-key-event (e)
      (case (@ e which)
	(37 (left))
	(38 (up))
	(39 (right))
	(40 (down))))
    
    (defun init-tilt-controls ()
      "Initialize tilt controls for tilting/rotating the sail"
      ;; Initialize values
      (setq incidence 0
	    rotation 0
	    absorbed 100)
      ;; Arrow key events
      ((@ ($ (@ document body)) keydown)
       #'(lambda (e)
	   (tilt-key-event e)))
      ;; Clicking on arrow images
      ((@ ($ "#up") click) #'(lambda (e) (up)))
      ((@ ($ "#down") click) #'(lambda (e) (down)))
      ((@ ($ "#left") click) #'(lambda (e) (left)))
      ((@ ($ "#right") click) #'(lambda (e) (right))))

    ;; Reflection

    (defun *normal (m)
      "Normal of mirror"
      (let ((n ((@ m local-to-world) (new (3fn *vector3 0 1 0)))))
	;; Test for pointing toward sun
	(when (< (@ n y) 0)
	  ((@ n negate)))
	((@ n normalize))
	n))

    (defun *incident ()
      "Solar incidence vector"
      (new (3fn *vector3 0 1 0)))
    
    (defun *reflect (incident normal)
      "Reflection vector from incident and normal vectors"
      (let ((v (new (3fn *vector3))))
	((@ v copy) normal)
	((@ v multiply-scalar)
	 (* -2 ((@ incident dot) normal)))
	((@ v add) incident)))

    (defun *reflection (corners)
      "Reflection object given list of corner vectors"
      (let ((refl (new (3fn *object3-d)))
	    (l 500)
	    (c 0xffff00)
	    (hl 20)
	    (hw 5))
	(dotimes (i 4)
	  ((@ refl add) (new (3fn *arrow-helper reflect ((@ (aref corners i) clone)) l c hl hw))))
	refl))

    (defun init-reflect ()
      "Initialize reflect lesson objects"
      (init-absorb)
      (setq normal (new (*normal mirror)))
      (setq reflect (new (*reflect incident normal)))
      (setq reflection (new (*reflection corners)))
      (let ((tinc (* 5 (random (/ 360 5)) (/ pi 180)))
	    (trot (* 5 (random (/ 360 5)) (/ pi 180)))
	    (tdist (+ 20 (random 180))))
	(setq target (new (*target tinc trot tdist 5))))
      ((@ scene add) reflection)
      ((@ scene add) target)
      ;; Reflection arrow
      (setq reflect-arrow (new (3fn *arrow-helper reflect origin 9 0xffff00)))
      ((@ scene add) reflect-arrow)
      ;; Tilt update function
      (setq tilt-update-fn #'update-tilt-reflect))
    
    (defun update-tilt-reflect ()
      "Update the reflect lesson objects in response to tilting the sail"
      (update-tilt-absorb)
      ;; Update reflection objects
      (setq normal (new (*normal mirror)))
      (setq reflect (new (*reflect incident normal)))
      ((@ scene remove) reflection)
      (setq reflection (new (*reflection corners)))
      ((@ scene add) reflection)
      ;; Update reflection arrow
      ((@ reflect-arrow set-direction) reflect)
      ((@ reflect-arrow set-length) (* 9 (/ absorbed 100))))
    
    (defun *target (inc rot dist rad)
      "Target to point reflection at given incidence, rotation, distance, and radius"
      (let* ((targ (new (3fn *mesh
			     (new (3fn *sphere-geometry rad 32 32))
			     (new (3fn *mesh-basic-material (create color 0xff0000))))))
	    (inc2 (* 2 inc)))
	((@ targ position set)
	 (* dist (sin inc2) (cos rot))
	 (- (* dist (cos inc2)))
	 (* dist (sin inc2) (sin rot)))
	targ))

    ;; Direction vectors
    (defun *normal-arrow (origin normal &optional (mag 1))
      "Draw arrow of a normal vector"
      (new (3fn *arrow-helper normal origin (* 10 mag) 0x888888)))

    (defun *incident-arrow (origin incident &optional (mag 1))
      "Draw arrow of an incident vector"
      (new (3fn *arrow-helper incident origin (* 10 mag) 0xff0000)))

    (defun *tangential (incident normal &optional (mag 1))
      "Generate tangent vector from incident and normal vectors"
      (let ((nxi (new (3fn *vector3)))
	    (tn (new (3fn *vector3))))
	((@ nxi cross-vectors) normal incident)
	(when (> ((@ nxi length)) 1d-6)
	  ((@ tn cross-vectors) incident nxi))
	((@ tn normalize))
	tn))

    (defun *tangential-arrow (origin tangential &optional (mag 1))
      "Draw arrow of tangential vector"
      (let ((c 0x0000ff)
	    (l 10))
	(new (3fn *arrow-helper tangential origin (* l mag) c))))

    (defun init-arrows ()
      (setq normal-arrow (new (*normal-arrow origin normal)))
      (setq incident-arrow (new (*incident-arrow origin normal)))
      (setq tangential (new (*tangential incident normal)))
      (setq tangential-arrow (new (*tangential-arrow origin tangential 0)))
      (setq reflection-arrow (new (3fn *arrow-helper reflect origin 10 0xffff00)))
      ((@ scene add) normal-arrow)
      ((@ scene add) incident-arrow)
      ((@ scene add) tangential-arrow)
      ((@ scene add) reflection-arrow))

    (defun arrow-visible (arrow bool)
      (when arrow
	(setf (@ arrow visible) bool)))
    
    (defun arrows-visible (bool)
      (dolist (a (list normal-arrow incident-arrow tangential-arrow reflection-arrow absorb-arrow))
	(arrow-visible a bool)))
    
    (defun update-tilt-direction ()
      (update-tilt-reflect)
      ;; Update arrows
      (setq tangential (new (*tangential incident normal)))
      ((@ normal-arrow set-direction) normal)
      ((@ tangential-arrow set-direction) tangential)
      ((@ reflection-arrow set-direction) reflect)
      ((@ tangential-arrow set-length)
       (if (> ((@ tangential length)) 0) 10 0)))

    (defun update-absorb-force ()
      "Integrate position during animation"
      ;; Update acceleration, velocity, position
      (let* ((accel (* 5d-5 (abs (cos (* incidence (/ pi 180))))))
	     (now ((@ (new (*date)) get-time)))
	     (dt (/ (* timefactor (- now (or time now))) 1000)))
	(setq time now)
	(incf elapsed dt)
	(incf vel (* dt accel))
	(incf pos (* dt vel))
	(setf (@ sail position y) pos)
	(setf (@ ($ "#accel") 0 inner-h-t-m-l) ((@ ((@ *math floor10) (* accel 1000) -4) to-string)))
	(setf (@ ($ "#speed") 0 inner-h-t-m-l) ((@ ((@ *math floor10) (* vel 1000) -2) to-string)))
	(setf (@ ($ "#distance") 0 inner-h-t-m-l) ((@ ((@ *math floor10) pos -3) to-string)))
	(let* ((minutes (floor (/ elapsed 60)))
	       (seconds (floor (- elapsed (* minutes 60)))))
	  (setf (@ ($ "#elapsed") 0 inner-h-t-m-l) (+ ((@ minutes to-string)) (if (> seconds 9) ":" ":0") ((@ seconds to-fixed))))))
      ;; Move absorbed arrow
      ((@ absorb-arrow position copy) (@ sail position))
      ;; Move sail mirror camera positions
      ((@ sails mcam position copy) (@ sail position))
      ;; Move vane mirrors camera positions
      (dolist (vane vanes)
	((@ vane mcam position copy) (@ sail position)))
      (update-tilt-absorb))

    (defun reset-absorb-force ()
      (setf pos 0)
      (setf vel 0)
      (setf elapsed 0))
    
    (defun toggle-pause (e)
      (let ((pause-html ($ "#pause")))
	(if pause
	    (progn
	      (setq pause false)
	      (setf (@ pause-html 0 inner-h-t-m-l) "Pause"))
	    (progn
	      (setq pause t)
	      (setf (@ pause-html 0 inner-h-t-m-l) "Continue")))))

    (defun space-pause-event (e)
      (when (= (@ e which) 32)
	(toggle-pause e)))
    
    (defun init-absorb-force ()
      (init-absorb)
      (setq vel 0)
      (setq pos 0)
      (setq elapsed 0)
      (setq timefactor 10)
      (setq pause false)
      ;; Pause button event
      (let ((pause-html ($ "#pause")))
	((@ pause-html click)
	 #'toggle-pause))
      ;; Pause with spacebar
      ((@ ($ (@ document body)) keydown)
       #'space-pause-event)
      ;; Reset button
      (let ((reset-html ($ "#reset")))
	((@ reset-html click)
	 #'(lambda (e)
	     (reset-absorb-force)
	     (update-absorb-force)))))
    
    (defun animate-force ()
      (request-animation-frame animate-force)
      (unless pause
	(when (> pos 100)
	  (reset-absorb-force))
	(update-absorb-force))
      (when pause (setq time false))
      ((@ controls update))
      (render))
  
    ))
