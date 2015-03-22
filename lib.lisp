;;; Library of common routines

(in-package :bld-ssweb)

(define-easy-handler (lib-js :uri "/js/lib.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))
    
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

    (defun stars ()
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
    
    (defun sun ()
      (let ((g (new (3fn *sphere-geometry 20 32 32)))
	    (m (new (3fn *mesh-basic-material (create color 0xf9ffd9)))))
	(new (3fn *mesh g m))))

    (defun selection-box (name x y z)
      "Selection box to click parts of the sail"
      (let ((sbox
	     (new (3fn *mesh
		       (new (3fn *box-geometry x y z))
		       (new (3fn *mesh-basic-material
				 (create color 0xffffff
					 wireframe true)))))))
	(setf (@ sbox name) name)
	sbox))

    (defun mirror ()
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
    
    (defun projection (corners)
      "Projection of that portion of the sunlight that hits the sail"
      (let ((mat (new (3fn *line-basic-material (create color 0xffff00))))
	    (pg (new (3fn *geometry)))
	    (ymin -500))
	(with-slots ((pv vertices)) pg
	  (destructuring-bind (mv1 mv2 mv3 mv4) corners
	    (let ((mv1b (new (3fn *vector3 (@ mv1 x) ymin (@ mv1 z))))
		  (mv2b (new (3fn *vector3 (@ mv2 x) ymin (@ mv2 z))))
		  (mv3b (new (3fn *vector3 (@ mv3 x) ymin (@ mv3 z))))
		  (mv4b (new (3fn *vector3 (@ mv4 x) ymin (@ mv4 z)))))
	      ((@ pv push)
	       mv1 mv2 mv3 mv4 mv1
	       mv1b mv2b mv3b mv4b mv1b
	       mv1 mv2 mv2b mv3b mv3 mv4 mv4b)
	      (new (3fn *line pg mat)))))))

    (defun corners (m)
      "List of corner points of a mirror object in world coordinates"
      (with-slots ((mv vertices)) (@ m geometry)
	(list
	 ((@ m local-to-world) ((@ mv 1 clone)))
	 ((@ m local-to-world) ((@ mv 2 clone)))
	 ((@ m local-to-world) ((@ mv 3 clone)))
	 ((@ m local-to-world) ((@ mv 4 clone))))))

    (defun rotate-global-y (object rad)
      "Rotate an object around the global Y axis by the given number of radians"
      (let* ((q ((@ ((@ (@ object quaternion) clone)) inverse)))
	     (ly ((@ ((@ (incident) apply-quaternion) q) normalize))))
	((@ object rotate-on-axis) ly rad)))

    (defun calc-absorbed (incidence)
      "Calculate the absorbed percentage from the incidence angle"
      (abs ((@ *math round10) (* 100 (cos (* incidence (/ pi 180)))) -1)))

    (defun normal (sail)
      "Normal vector of a sail"
      (let ((n ((@ (incident) apply-quaternion) (@ sail quaternion))))
	;; Test for pointing toward sun
	(when (< (@ n y) 0)
	  ((@ n negate)))
	((@ n normalize))
	n))

    (defun incident ()
      "Solar incidence vector"
      (new (3fn *vector3 0 1 0)))
      
    (defun reflectv (incident normal)
      "Reflection vector from incident and normal vectors"
      (let ((v (new (3fn *vector3))))
	((@ v copy) normal)
	((@ v multiply-scalar)
	 (* -2 ((@ incident dot) normal)))
	((@ v add) incident)))

    (defun reflection (corners reflectv)
      "Reflection object given list of corner vectors"
      (let ((refl (new (3fn *object3-d)))
	    (l 500)
	    (c 0xffff00)
	    (hl 20)
	    (hw 5))
	(with-slots (update visibility children) refl
	  ;; Initial creation of object
	  (dotimes (i 4)
	    ((@ refl add) (new (3fn *arrow-helper reflectv ((@ (aref corners i) clone)) l c hl hw))))
	  (setf visibility ; Visibility method
		(lambda (bool)
		  (dolist (a children)
		    (setf (@ a visible) bool)))))
	refl))

    (defun template ()
      "Flight school app"
      (let ((app (create)))
	(with-slots (scene origin plot-div renderer camera light alight stars sun
			   sail booms bus sails vanes
			   render init on-window-resize) app
	  
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
	  (setf stars (stars))
	  ((@ scene add) stars)

	  ;; Sun
	  (setf sun (sun))
	  ((@ sun translate-y) -500)
	  ((@ scene add) sun)
	  
	  ;; Render method
	  (setf render
		(lambda ()
		  ((@ renderer render) scene camera)))

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
		  ((@ window add-event-listener) "resize" on-window-resize false)))
	  
	  )
	app))

    (defun add-booms (app)
      "Add booms"
      (with-slots (sail booms render) app
	(setf booms (create))
	(with-slots (loader file loadfn obj load select-obj1 select-obj2) booms
	  ;; Selection objects
	  ;; 1st set of booms
	  (setf select-obj1 (selection-box "booms" .4 .2 8))
	  (setf (@ select-obj1 visible) false)
	  ((@ sail add) select-obj1)
	  ;; 2nd set of booms
	  (setf select-obj2 (selection-box "booms" 8 .2 .4))
	  (setf (@ select-obj2 visible) false)
	  ((@ sail add) select-obj2)
	  ;; Load booms geometry from file
	  (setf loader (new (3fn -j-s-o-n-loader t))
		file "js/booms.js"
		loadfn
		#'(lambda (geometry materials)
		    (let* ((mats (new (3fn *mesh-face-material materials)))
			   (obj (new (3fn *mesh geometry mats))))
		      (setf (@ booms obj) obj)
		      ((@ sail add) obj)
		      (funcall render)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      app)

    (defun add-bus (app)
      "Add bus"
      (with-slots (sail bus render) app
	(setf bus (create))
	(with-slots (loader file loadfn obj load select-obj) bus
	  ;; Selection object
	  (setf select-obj (selection-box "bus" 1 1 1))
	  (setf (@ select-obj visible) false)
	  ((@ select-obj rotate-y) (/ pi 4))
	  ((@ sail add) select-obj)
	  ;; Bus geometry loaded from file
	  (setf loader (new (3fn -j-s-o-n-loader t))
		file "js/bus.js"
		loadfn
		#'(lambda (geometry materials)
		    (let* ((mats (new (3fn *mesh-face-material materials)))
			   (obj (new (3fn *mesh geometry mats))))
		      (setf (@ bus obj) obj)
		      ((@ sail add) obj)
		      (funcall render)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      app)

    (defun add-sails (app)
      "Add sails"
      (with-slots (sail sails render) app
	(setf sails (create))
	(with-slots (loader file loadfn obj load mcam select-obj) sails
	  ;; Selection box
	  (setf select-obj (selection-box "sails" (* 4 (sqrt 2)) 0 (* 4 (sqrt 2))))
	  (setf (@ select-obj visible) false)
	  ((@ select-obj rotate-y) (/ pi 4))
	  ((@ sail add) select-obj)
	  ;; Mirror camera
	  (let ((mirror-camera (new (3fn *cube-camera 1 5000 512))))
	    (setf mcam mirror-camera)
	    ((@ sail add) mcam)
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
			(funcall render)))
		  load #'(lambda () ((@ loader load) file loadfn))))))
      app)

    (defun add-vanes (app)
      "Add vanes"
      (with-slots (sail vanes render) app
	(setf
	 vanes
	 (loop for (x z) in '((1 0)(0 1)(-1 0)(0 -1))
	    for i = 0 then (incf i)
	    for yrot = (* i (/ pi 2))
	    collect
	      (let ((vane (create)))
		(with-slots (loader file loadfn obj load mcam select-obj) vane
		  ;; Selection objects
		  (setf select-obj (selection-box "vanes" .5 1 1))
		  (setf (@ select-obj position x) (* 4.25 x)
			(@ select-obj position z) (* 4.25 z)
			(@ select-obj visible) false)
		  ((@ select-obj rotate-y) yrot)
		  ((@ sail add) select-obj)
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
			      ((@ obj add) mcam)
			      (funcall render)))
			load #'(lambda () ((@ loader load) file loadfn))))
		vane))))
      app)

    (defun add-sail (app)
      "Add sail object with sub-parts"
      (with-slots (scene sail sails booms bus vanes renderer render init visibility) app
	;; Sail object
	(setf sail (new (3fn *object3-d)))
	;; Add sail components
	(add-vanes (add-bus (add-booms (add-sails app))))
	;; Put sail in scene for rendering
	((@ scene add) sail)
	(with-slots (load children) sail
	  (setf
	   ;; Load function to load sail parts
	   load (lambda ()
		  ((@ booms load))
		  ((@ bus load))
		  ((@ sails load))
		  (dolist (vane vanes) ((@ vane load))))
	   ;; Toggle sail parts visibility
	   visibility (lambda (bool)
			(dolist (part children)
			  (if (@ part obj)
			      (setf (@ part obj visible) bool))))
	   ;; Update render function
	   render (let ((render-super ((@ app superior) "render")))
		    (lambda ()
		      ;; Make sail parts invisible
		      (funcall visibility false)
		      ;; Update mirror cube cameras
		      (when sails
			((@ sails mcam update-cube-map) renderer scene))
		      (when vanes
			(dolist (vane vanes)
			  ((@ vane mcam update-cube-map) renderer scene)))
		      ;; Make sail parts visible
		      (funcall visibility true)
		      ;; Call prototype render
		      (funcall render-super)))
	   ;; Update init function
	   init (let ((init-super ((@ app superior) "init")))
		  (lambda ()
		    (funcall init-super)
		    (funcall load))))))
      app)
    
    (defun flightschool ()
      (add-sail (template)))

    (defun add-controls (app)
      "Add mouse/touchscreen controls to rotate & zoom"
      (with-slots (controls camera render animate) app
	(setf
	 controls (new (3fn *trackball-controls camera))
	 (@ controls no-pan) true
	 (@ controls no-zoom) false
	 animate (lambda ()
		   (request-animation-frame animate)
		   ((@ controls update))))
	((@ controls add-event-listener) "change" render))
      app)
    
    (defun what ()
      "What is a Solar Sail app"
      (add-controls (flightschool)))

    (defun add-clickparts (app)
      "Add clickable parts"
      (with-slots (init on-parts-click target-list booms sails bus vanes mouse part-name camera) app
	;; Init function
	(let ((super-init ((@ app superior) "init")))
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
		;; Mouse coordinates
		(setf (@ mouse x) (- (* 2 (/ (@ event client-x) (@ window inner-width))) 1))
		(setf (@ mouse y) (- 1 (* 2 (/ (@ event client-y) (@ window inner-height)))))
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
			    (setf part-name part-name-select)))
			(progn
			  (when part-name (setf (@ ((@ document get-element-by-id) part-name) class-name) "hidden"))
			  (setf part-name ""))))))))
      app)
    
    (defun parts ()
      "Parts of a sail app"
      (add-clickparts (what)))

    (defun add-tilt (app)
      "Add tilt controls"
      (with-slots (camera sail update-tilt rotation incidence absorbed up down left right render) app
	;; Set new slots
	(setf
	 rotation 0 incidence 0 absorbed 100
	 update-tilt
	 (lambda ()
	   ;; Sail rotation matrix
	   ((@ sail update-matrix-world))
	   ;; Angle fields
	   (let ((inc-html (@ ($ "#incidence") 0)))
	     (when inc-html
	       (setf (@ inc-html inner-h-t-m-l) ((@ incidence to-string)))))
	   (let ((rot-html (@ ($ "#rotation") 0)))
	     (when rot-html
	       (setf (@ rot-html inner-h-t-m-l) ((@ rotation to-string)))))
	   (let ((absorbed-html (@ ($ "#absorbed") 0)))
	     (when absorbed-html
	       (setf (@ absorbed-html inner-h-t-m-l) ((@ absorbed to-string))))))
	 ;; Tilt functions
	 up (lambda ()
	      ((@ sail rotate-z) (/ pi 36))
	      (incf incidence 5)
	      (when (> incidence 180)
		(setq incidence (- incidence 360)))
	      (setq absorbed (calc-absorbed incidence))
 	      (funcall update-tilt)
	      (funcall render))
	 down (lambda ()
		((@ sail rotate-z) (/ pi -36))
		(decf incidence 5)
		(when (< incidence -180)
		  (setq incidence (+ 360 incidence)))
		(setq absorbed (calc-absorbed incidence))
 	      (funcall update-tilt)
	      (funcall render))
	 left (lambda ()
		(rotate-global-y sail (/ pi 36))
		(decf rotation 5)
		(when (< rotation -180)
		  (setq rotation (+ 360 rotation)))
 	      (funcall update-tilt)
	      (funcall render))
	 right (lambda ()
		 (rotate-global-y sail (/ pi -36))
		 (incf rotation 5)
		 (when (> rotation 180)
		   (setq rotation (- rotation 360)))
 	      (funcall update-tilt)
	      (funcall render)))
	;; Arrow key events
	((@ ($ (@ document body)) keydown)
	 #'(lambda (e)
	     (case (@ e which)
	       (37 (funcall left))
	       (38 (funcall up))
	       (39 (funcall right))
	       (40 (funcall down)))))
	;; Click on arrow images
	((@ ($ "#up") click) #'(lambda (e) (funcall up)))
	((@ ($ "#down") click) #'(lambda (e) (funcall down)))
	((@ ($ "#left") click) #'(lambda (e) (funcall left)))
	((@ ($ "#right") click) #'(lambda (e) (funcall right)))
	)
      app)

    (defun add-mirror (app)
      (with-slots (sail mirror) app
	(setf mirror (mirror))
	((@ sail add) mirror))
      app)

    (defun add-corners (app)
      (with-slots (sail corners mirror) app
	(setf corners (corners mirror))
	app))
    
    (defun add-projection (app)
      "Add projection of absorbed light"
      (with-slots (mirror corners projection incident absorb-arrow visibility update-tilt origin scene sail absorbed) app
	(setf
	 app (add-corners (add-mirror app))
	 mirror (mirror)
	 corners (corners mirror)
	 projection (projection corners)
	 incident (incident)
	 absorb-arrow (new (3fn *arrow-helper incident origin 10 0xffff00))
	 visibility
	 (let ((visibility-super ((@ app superior) "visibility")))
	   (lambda (bool)
	     (setf (@ projection visible) bool
		   (@ absorb-arrow visible) bool)
	     (funcall visibility-super bool)))
	 update-tilt
	 (let ((tilt-super ((@ app superior) "updateTilt")))
	   (lambda ()
	     (funcall tilt-super)
	     ;; Corners of the sail
	     (setf corners (corners mirror))
	     ;; Projection
	     ((@ scene remove) projection)
	     (setf projection (projection corners))
	     ((@ scene add) projection)
	     ;; Update absorbed arrow
	     ((@ absorb-arrow set-length) (* 10 (/ absorbed 100)))))
	 )
	;; Add objects
	((@ sail add) mirror)
	((@ scene add) projection)
	((@ scene add) absorb-arrow)
	)
      app)
    
    (defun absorb ()
      "Absorbed light on a sail app"
      (let ((app (add-projection (add-tilt (what)))))
	;; Move the camera
	((@ app camera position set) -4 -6 6)
	app))

    (defun add-reflection (app)
      "Add a box showing the reflected sunlight"
      (with-slots (absorbed normal incident reflectv reflection reflect-arrow mirror corners update-tilt origin scene visibility sail) app
	(unless mirror (add-mirror app))
	(unless corners (add-corners app))
	(setf incident (incident))
	(setf normal (normal sail)
	      reflectv (reflectv incident normal)
	      reflection (reflection corners reflectv)
	      reflect-arrow (new (3fn *arrow-helper reflectv origin 10 0xffff00))
	      visibility
	      (let ((visibility-super ((@ app superior) "visibility")))
		(lambda (bool)
		  (funcall visibility-super bool)
		  (setf (@ reflection visible) bool
			(@ reflect-arrow visible) bool)))
	      update-tilt
	      (let ((tilt-super ((@ app superior) "updateTilt"))) 
		(lambda ()
		  (funcall tilt-super)
		  ;; Update corners
		  (setf corners (corners mirror))
		  ;; Update reflection objects
		  (setf normal (normal sail)
			reflectv (reflectv incident normal))
		  ((@ scene remove) reflection)
		  (setf reflection (reflection corners reflectv))
		  ((@ scene add) reflection)
		  ((@ reflect-arrow set-direction) reflectv)
		  ((@ reflect-arrow set-length) (* 10 (/ absorbed 100))))))
	((@ scene add) reflection)
	((@ scene add) reflect-arrow))
      app)

    (defun add-target (app)
      "Add a big red randomly positioned target"
      (with-slots (scene target) app
	(let ((inc2 (* 2 (* 5 (random (/ 360 5)) (/ pi 180))))
	      (rot (* 5 (random (/ 360 5)) (/ pi 180)))
	      (dist (+ 20 (random 80)))
	      (rad 5)
	      (color 0xff0000))
	  (setf target
		(new
		 (3fn *mesh
		      (new (3fn *sphere-geometry rad 32 32))
		      (new (3fn *mesh-basic-material (create color color))))))
	  ((@ target position set)
	   (* dist (sin inc2) (cos rot))
	   (- (* dist (cos inc2)))
	   (* dist (sin inc2) (sin rot)))
	  ((@ scene add) target)))
      app)
	
    (defun reflect ()
      "Reflected light on a sail app"
      (add-target (add-reflection (absorb))))

    (defun add-animation (app)
      (let ((vel (new (3fn *vector3 0 0 0)))
	    (pos (new (3fn *vector3 0 0 0)))
	    (elapsed 0)
	    (time false))
	(with-slots (timefactor pause toggle-pause init reset update-anim sail update-tilt render animate controls accelfn) app
	  (setf
	   timefactor 10
	   pause false
	   toggle-pause
	   (lambda (e)
	     (let ((pause-html ($ "#pause")))
	       (if pause
		   (progn
		     (setf pause false)
		     (setf (@ pause-html 0 inner-h-t-m-l) "Pause"))
		   (progn
		     (setf pause t)
		     (setf (@ pause-html 0 inner-h-t-m-l) "Continue")))))
	   reset
	   (lambda ()
	     ((@ pos set) 0 0 0)
	     ((@ vel set) 0 0 0)
	     (setf elapsed 0))
	   init
	   (let ((init-super ((@ app superior) "init")))
	     (lambda ()
	       ;; Absorb init
	       (funcall init-super)
	       ;; Pause button event
	       (let ((pause-html ($ "#pause")))
		 ((@ pause-html click)
		  #'toggle-pause))
	       ;; Pause with spacebar
	       ((@ ($ (@ document body)) keydown)
		(lambda (e)
		  (when (= (@ e which) 32)
		  (funcall toggle-pause e))))
	       ;; Reset button
	       (let ((reset-html ($ "#reset")))
		 ((@ reset-html click)
		  #'(lambda (e)
		      (funcall reset)
		      (funcall update-anim)
		      (funcall update-tilt)
		      (funcall render))))))
	   accelfn
	   (lambda () (new (3fn *vector3 0 0 0)))
	   update-anim
	   (lambda ()
	     (let* ((accel-v (funcall accelfn))
		    (now ((@ (new (*date)) get-time)))
		    (dt (/ (* timefactor (- now (or time now))) 1000)))
	       (setf time now)
	       (incf elapsed dt)
	       ((@ vel add) ((@ ((@ accel-v clone)) multiply-scalar) dt))
	       ((@ pos add) ((@ ((@ vel clone)) multiply-scalar) dt))
	       ((@ sail position copy) pos)
	       (setf (@ ($ "#accel") 0 inner-h-t-m-l) ((@ ((@ *math floor10) (* ((@ accel-v length)) 1000) -4) to-string)))
	       (setf (@ ($ "#speed") 0 inner-h-t-m-l) ((@ ((@ *math floor10) (* ((@ vel length)) 1000) -2) to-string)))
	       (setf (@ ($ "#distance") 0 inner-h-t-m-l) ((@ ((@ *math floor10) ((@ pos length)) -3) to-string)))
	       (let* ((minutes (floor (/ elapsed 60)))
		      (seconds (floor (- elapsed (* minutes 60)))))
		 (setf (@ ($ "#elapsed") 0 inner-h-t-m-l)
		       (+ ((@ minutes to-string)) (if (> seconds 9) ":" ":0") ((@ seconds to-fixed)))))))
	   animate
	   (lambda ()
	     (request-animation-frame animate)
	     (unless pause
	       (when (> ((@ pos length)) 100)
		 (funcall reset))
	       (funcall update-anim)
	       (funcall update-tilt)
	       (funcall render))
	     (when pause (setf time false))
	     ((@ controls update)))
	   
	   )))
      app)
    
    (defun absorb-force ()
      (let ((app (add-animation (absorb))))
	(with-slots (accelfn incidence update-anim absorb-arrow sail) app
	  (setf
	   timefactor 10
	   accelfn
	   (lambda ()
	     (let ((accel (* 5d-5 (abs (cos (* incidence (/ pi 180)))))))
	       (new (3fn *vector3 0 accel 0))))
	   update-anim
	   (let ((up-anim-super ((@ app superior) "updateAnim")))
	     (lambda ()
	       (funcall up-anim-super)
	       ;; Move absorbed arrow
	       ((@ absorb-arrow position copy) (@ sail position))))
	   ))
	app))
    
    (defun reflect-force ()
      (let ((app (add-animation (add-target (add-reflection (add-tilt (what)))))))
	(with-slots (sail camera reflectv reflect-arrow update-anim incidence accelfn timefactor) app
	  ((@ camera position set) -4 -6 6)
	  (setf
	   timefactor 100
	   accelfn
	   (lambda ()
	     (let ((accel (* 5d-5 (abs (cos (* incidence (/ pi 180)))))))
	       ((@ ((@ ((@ reflectv clone)) set-length) accel) negate))))
	   update-anim
	   (let ((up-anim-super ((@ app superior) "updateAnim")))
	     (lambda ()
	       (funcall up-anim-super)
	       ;; Move reflected arrow
	       ((@ reflect-arrow position copy) (@ sail position))))))
	app))

    (defun force ()
      (let ((app (add-animation (add-target (add-reflection (add-projection (add-tilt (what))))))))
	(with-slots (scene sail camera reflectv absorb-arrow reflect-arrow update-tilt update-anim incidence accelfn timefactor force-arrow normal) app
	  ((@ camera position set) -4 -6 6)
	  (setf
	   timefactor 10
	   force-arrow (new (3fn *arrow-helper
				 (new (3fn *vector3 0 1 0))
				 (new (3fn *vector3 0 0 0))
				 20 0xff0000))
	   accelfn
	   (lambda ()
	     (let* ((accel (* 5d-5 (abs (cos (* incidence (/ pi 180))))))
		    (accel-a-v (new (3fn *vector3 0 accel 0)))
		    (accel-r-v ((@ ((@ ((@ reflectv clone)) set-length) accel) negate))))
	       ((@ ((@ accel-a-v clone)) add) accel-r-v)))
	   update-tilt
	   (let ((tilt-super ((@ app superior) "updateTilt")))
	     (lambda ()
	       (funcall tilt-super)
	       ((@ force-arrow set-direction) normal)
	       ((@ force-arrow set-length) (* 20 (abs (expt (cos (* incidence (/ pi 180))) 2))))
	       ))
	   update-anim
	   (let ((up-anim-super ((@ app superior) "updateAnim")))
	     (lambda ()
	       (funcall up-anim-super)
	       ((@ absorb-arrow position copy) (@ sail position))
	       ((@ reflect-arrow position copy) (@ sail position))
	       ((@ force-arrow position copy) (@ sail position))))
	   visibility
	   (let ((vis-super ((@ app superior) "visibility")))
	     (lambda (bool)
	       (funcall vis-super bool)
	       (setf (@ force-arrow visible) bool))))
	  ((@ scene add) force-arrow))
	app))
    ))
