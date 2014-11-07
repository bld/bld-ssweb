(in-package :bld-ssweb)

(define-easy-handler (mirror1-js :uri "/mirror1.js") ()
  (setf (content-type*) "text/javascript")
  (ps
      
    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))
   
    (defmacro 3xfn (fn &rest args)
      `((@ *t-h-r-e-ex ,fn) ,@args))

    (defun webgl-available ()
      (try
       (let ((canvas ((@ document create-element) "canvas")))
	 (not
	  (and (@ window *wb-g-l-rendering-context)
	       (or ((@ canvas get-context) "webgl")
		   ((@ canvas get-context) "experimental-webgl")))))
       (:catch (e) false)))

    (let (scene mirror renderer camera origin projection controls loader light sail normal reflect incident reflection corners)

      (defun *mirror ()
	(let* ((w 8)
	       (w2 (/ w 2))
	       (m (new (3fn *mesh-basic-material
			    (create 
			     color 0x444444
			     side (@ *three* *double-side)
			     wireframe t))))
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
	  (new (3fn *mesh s m))))

      (defun *projection (m)
	(let ((mat (new (3fn *line-basic-material (create color 0xffff00))))
	      (pg (new (3fn *geometry)))
	      (ymin -10))
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

      (defun *normal (m)
	((@ m local-to-world) (new (3fn *vector3 0 1 0))))

      (defun *reflect (incident normal)
	(let ((v (new (3fn *vector3))))
	  ((@ v copy) normal)
	  ((@ v multiply-scalar)
	   (* -2 ((@ incident dot) normal)))
	  ((@ v add) incident)))

      (defun *reflection (corners)
	(list
	 (new (3fn *arrow-helper reflect ((@ corners 0 clone)) 20 0xffff00))
	 (new (3fn *arrow-helper reflect ((@ corners 1 clone)) 20 0xffff00))
	 (new (3fn *arrow-helper reflect ((@ corners 2 clone)) 20 0xffff00))
	 (new (3fn *arrow-helper reflect ((@ corners 3 clone)) 20 0xffff00))))

      (defun *corners (m)
	(with-slots ((mv vertices)) (@ m geometry)
	  (list
	   ((@ m local-to-world) ((@ mv 1 clone)))
	   ((@ m local-to-world) ((@ mv 2 clone)))
	   ((@ m local-to-world) ((@ mv 3 clone)))
	   ((@ m local-to-world) ((@ mv 4 clone))))))
      
      (defun init ()
	;; DOM element to plot in
	(setq plot-div ((@ document get-element-by-id) "plot"))
	;; Origin of coordinates
	(setq origin (new (3fn *vector3 0 0 0)))
	;; Renderer
	(setq renderer (if (webgl-available)
			   (new (3fn *web-g-l-renderer))
			   (new (3fn *canvas-renderer))))
	((@ renderer set-size) window.inner-width window.inner-height)
	((@ plot-div append-child) (@ renderer dom-element))
	;; Camera
	(setq camera (new (3fn *perspective-camera 75 (/ window.inner-width window.inner-height) 0.1 1000)))
	((@ camera position set) 15 -1 7)
	((@ camera look-at) origin)
	;; Scene
	(setq scene (new (3fn *scene)))
	;; Invisible mesh at points of sail
	(setq mirror (new *mirror))
	(setq corners (new (*corners mirror)))
	;; Incident vector
	(setq incident (new (3fn *vector3 0 1 0)))
	;; Normal vector
	(setq normal (new (*normal mirror)))
	;; Reflect vector
	(setq reflect (new (*reflect incident normal)))
	(setq reflection (new (*reflection corners)))
	;; Light
	(setq light (new (3fn *directional-light 0xffffff 2)))
	((@ light position set) 0 -1 0)
	;; Projection of light onto sail
	(setq projection (new (*projection mirror)))
	;; Orbit controls
	(setq controls (new (3fn *orbit-controls camera)))
	(setf (@ controls no-pan) true)
	((@ controls add-event-listener) "change" #'render)
	;; Add light, camera, & projection
	((@ scene add) light)
	((@ scene add) camera)
	((@ scene add) projection)
	(mapcar #'(lambda (r) ((@ scene add) r)) reflection)
	;; Load sail JSON file & add to scene
	(setq loader (new (3fn -j-s-o-n-loader t)))
	((@ loader load)
	 "js/sail.js"
	 #'(lambda (geometry materials)
	     (let ((mats (new (3fn *mesh-face-material materials))))
	       ;; Make sail & vane materials double-sided
	       (setf (@ mats materials 1 side) (@ *t-h-r-e-e *double-side))
	       (setf (@ mats materials 2 side) (@ *t-h-r-e-e *double-side))
	       (setq sail (new (3fn *mesh geometry mats)))
	       ((@ scene add) sail)
	       ;; Update normal vector
	       (render)))))

      (defun rotate-global-y (object rad)
	(let* ((gy (new (3fn *vector3 0 1 0)))
	       (ly ((@ object world-to-local) gy)))
	  ((@ object rotate-on-axis) ly rad)))

      (defun render ()
	((@ renderer render) scene camera))

      (defun update ()
	((@ mirror update-matrix-world))
	(setq corners (new (*corners mirror)))
	((@ scene remove) projection)
	(setq projection (new (*projection mirror)))
	(setq normal (new (*normal mirror)))
	(setq reflect (new (*reflect incident normal)))
	(mapcar #'(lambda (r) ((@ scene remove) r)) reflection)
	(setq reflection (new (*reflection corners)))
	(mapcar #'(lambda (r) ((@ scene add) r)) reflection)
	((@ scene add) projection)
	(render))

      ((@ ($ (@ document body)) on) "keydown"
       #'(lambda (e)
	   (case (@ e which)
	     ;; Left
	     (37 (rotate-global-y mirror (/ pi 36))
		 (rotate-global-y sail (/ pi 36))
		 (update))
	     ;; Up
	     (38 ((@ mirror rotate-z) (/ pi 36))
		 ((@ sail rotate-z) (/ pi 36))
		 (update))
	     ;; Right
	     (39 (rotate-global-y mirror (/ pi -36))
		 (rotate-global-y sail (/ pi -36))
		 (update))
	     ;; Down
	     (40 ((@ mirror rotate-z) (/ pi -36))
		 ((@ sail rotate-z) (/ pi -36))
		 (update)))))

      (init)
      (render))))

(define-easy-handler (mirror1 :uri "/mirror1") ()
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      :title "Light striking a solar sail"
      (:link :href "mirror1.css" :rel "stylesheet")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :src "js/OrbitControls.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*)))))
     (:body
      (:div :id "info"
	    (:h1 "Light striking a solar sail")
	    (:p "This shows what portion of the light coming from below hits a solar sail as you rotate and tilt it.")
	    (:p "Left and right arrow keys to rotate")
	    (:p "Up and down arrow keys to tilt")
	    (:p "Hold left mouse button to rotate view, middle mouse wheel to zoom"))
      (:div :id "plot")
      (:script :src "mirror1.js")))))

(define-easy-handler (mirror2 :uri "/mirror2") ()
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      :title "Light striking a solar sail"
      (:link :href "mirror1.css" :rel "stylesheet")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :src "js/OrbitControls.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*)))))
     (:body
      (:div :id "info"
	    (:h1 "Light reflecting off of a solar sail")
	    (:p "This shows what portion of the light coming from below hits a solar sail and reflects off of it as you rotate and tilt the sail.")
	    (:p "Left and right arrow keys to rotate")
	    (:p "Up and down arrow keys to tilt")
	    (:p "Hold left mouse button to rotate view, middle mouse wheel to zoom"))
      (:div :id "plot")
      (:script :src "mirror1.js")))))
