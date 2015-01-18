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
    (defvar reflection)
    (defvar tilt-update-fn)
    (defvar target)
    ;; Direction arrows & variables
    (defvar normal-arrow)
    (defvar incident-arrow)
    (defvar tangential)
    (defvar tangential-arrow)
    (defvar reflection-arrow)

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

    (defun sail-parts-visible (bool)
      "Change visibility of sail parts"
      (dolist (p (@ sail children))
	(if (@ p obj) (setf (@ p obj visible) bool))))

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
      ((@ sails mcam update-cube-map) renderer scene)
      (dolist (vane vanes)
	((@ vane mcam update-cube-map) renderer scene))
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
      (funcall tilt-update-fn)
      (render))
    
    (defun update-tilt-absorb ()
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
      (setf (@ ($ "#absorbed") 0 inner-h-t-m-l) ((@ absorbed to-string))))

    (defun calc-absorbed (incidence)
      (abs (round (* 100 (cos (* incidence (/ pi 180)))))))

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
    
    (defun init-tilt-controls ()
      ;; Initialize values
      (setq incidence 0
	    rotation 0
	    absorbed 100)
      ;; Arrow key events
      ((@ ($ (@ document body)) on) "keydown"
       #'(lambda (e)
	   (case (@ e which)
	     (37 (left))
	     (38 (up))
	     (39 (right))
	     (40 (down)))))
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

    (defun update-tilt-reflect ()
      (update-tilt-absorb)
      ;; Update reflection objects
      (setq normal (new (*normal mirror)))
      (setq reflect (new (*reflect incident normal)))
      ((@ scene remove) reflection)
      (setq reflection (new (*reflection corners)))
      ((@ scene add) reflection))
    
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
      (dolist (a (list normal-arrow incident-arrow tangential-arrow reflection-arrow))
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
    
    ;; Magnitudes
    (defun update-tilt-magnitudes ()
      "Update the arrows with the magnitide of light going in each direction"
      (update-tilt-direction)
      ;; Update arrow magnitudes
      (let* ((absmag ((@ incident dot) normal))
	     (mag (* absmag absmag))
	     (tmag (* mag ((@ tangential dot) normal)))
	     (incmag (* mag absmag)))
	((@ normal-arrow set-length) (* 10 mag))
	((@ tangential-arrow set-length) (* 10 tmag))
	((@ incident-arrow set-length) (* 10 incmag))
	((@ reflection-arrow set-length) (* 10 absmag))))
    ))
