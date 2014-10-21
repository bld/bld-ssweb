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

    (let (scene mirror renderer camera origin projection controls)
      
      (defun *mirror ()
	(let* ((w 10)
	       (w2 (/ w 2))
	       (m (new (3fn *mesh-basic-material
			    (create 
			     color 0x444444
			     side (@ *three* *double-side)))))
	       (s (new (3fn *geometry))))
	  (with-slots (vertices faces) s
	    ((@ vertices push)
	     (new (3fn *vector3 0 0 0))
	     (new (3fn *vector3 w2 0 w2))
	     (new (3fn *vector3 w2 0 (- w2)))
	     (new (3fn *vector3 (- w2) 0 (- w2)))
	     (new (3fn *vector3 (- w2) 0 w2)))
	    ((@ faces push)
	     (new (3fn *face3 0 1 2))
	     (new (3fn *face3 0 2 3))
	     (new (3fn *face3 0 3 4))
	     (new (3fn *face3 0 4 1))))
	  ((@ s compute-face-normals))
	  ((@ s compute-bounding-sphere))
	  (new (3fn *mesh s m))))

      (defun *projection (m)
	(let ((mat (new (3fn *mesh-basic-material
			     (create
			      color 0xffff00
			      wireframe true))))
	      (pg (new (3fn *geometry)))
	      (ymin -100))
	  (with-slots ((pv vertices) (pf faces)) pg
	    (with-slots ((mv vertices)) (@ m geometry)
	      (let ((mv1 ((@ m local-to-world) ((@ mv 1 clone))))
		    (mv2 ((@ m local-to-world) ((@ mv 2 clone))))
		    (mv3 ((@ m local-to-world) ((@ mv 3 clone))))
		    (mv4 ((@ m local-to-world) ((@ mv 4 clone)))))
		((@ pv push)
		 mv1 mv2 mv3 mv4 ; Corners of sail
		 ;; Corners of sail at Y=-10
		 (new (3fn *vector3 (@ mv1 x) ymin (@ mv1 z)))
		 (new (3fn *vector3 (@ mv2 x) ymin (@ mv2 z)))
		 (new (3fn *vector3 (@ mv3 x) ymin (@ mv3 z)))
		 (new (3fn *vector3 (@ mv4 x) ymin (@ mv4 z))))
		((@ pf push)
		 ;; Top
		 (new (3fn *face3 0 1 2))
		 (new (3fn *face3 2 3 0))
		 ;; Bottom
		 (new (3fn *face3 6 5 4))
		 (new (3fn *face3 4 7 6))
		 ;; 1st face
		 (new (3fn *face3 0 4 1))
		 (new (3fn *face3 1 4 5))
		 ;; 2nd face
		 (new (3fn *face3 1 5 2))
		 (new (3fn *face3 2 5 6))
		 ;; 3rd face
		 (new (3fn *face3 2 6 3))
		 (new (3fn *face3 3 6 7))
		 ;; 4th face
		 (new (3fn *face3 3 7 0))
		 (new (3fn *face3 0 7 4))))))
	  (new (3fn *mesh pg mat))))

      (defun init ()
	(setq origin (new (3fn *vector3 0 0 0)))
	(setq renderer (if (webgl-available)
			   (new (3fn *web-g-l-renderer))
			   (new (3fn *canvas-renderer))))
	(setq plot-div ((@ document get-element-by-id) "plot"))
	(setq camera (new (3fn *perspective-camera 75 (/ window.inner-width window.inner-height) 0.1 1000)))
	(setq scene (new (3fn *scene)))
	(setq mirror (new *mirror))
	((@ mirror rotate-z) *cone)
	((@ mirror update-matrix-world))
	(setq projection (new (*projection mirror)))
	((@ renderer set-size) window.inner-width window.inner-height)
	((@ plot-div append-child) (@ renderer dom-element))
	((@ camera position set) 20 -1 10)
	((@ camera look-at) origin)
	(setq controls (new (3fn *orbit-controls camera)))
	(setf (@ controls no-pan) true)
	((@ controls add-event-listener) "change" #'render)
	((@ scene add) camera)
	((@ scene add) mirror)
	((@ scene add) projection))

      (defun rotate-global-y (object rad)
	(let* ((gy (new (3fn *vector3 0 1 0)))
	       (ly ((@ object world-to-local) gy)))
	  ((@ object rotate-on-axis) ly rad)))

      (defun render ()
	((@ renderer render) scene camera))

      (defun update ()
	((@ mirror update-matrix-world))
	((@ scene remove) projection)
	(setq projection (new (*projection mirror)))
	((@ scene add) projection)
	(render))

      ((@ ($ (@ document body)) on) "keydown"
       #'(lambda (e)
	   (case (@ e which)
	     ;; Left
	     (37 (rotate-global-y mirror (/ pi 36))
		 (update))
	     ;; Up
	     (38 ((@ mirror rotate-z) (/ pi 36))
		 (update))
	     ;; Right
	     (39 (rotate-global-y mirror (/ pi -36))
		 (update))
	     ;; Down
	     (40 ((@ mirror rotate-z) (/ pi -36))
		 (update)))))

      (init)
      (render))))

(define-easy-handler (mirror1 :uri "/mirror1") ((cone :init-form "0"))
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      :title "Light striking an object"
      (:link :href "mirror1.css" :rel "stylesheet")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :src "js/OrbitControls.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*))))
      (:script :type "text/javascript"
	       (str
		(ps
		  (defvar *cone
		    (lisp (* (/ pi 180)
			     (read-from-string cone nil 0))))))))
     (:body
      (:div :id "info"
	    (:h1 "Light striking an object")
	    (:p "This shows what portion of the light coming from below hits a solar sail as you rotate and tilt it.")
	    (:p "Left and right arrow keys to rotate")
	    (:p "Up and down arrow keys to tilt")
	    (:p "Hold left mouse button to rotate view, middle mouse wheel to zoom"))
      (:div :id "plot")
      (:script :src "mirror1.js")))))
