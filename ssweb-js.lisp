(in-package :bld-ssweb)

(define-easy-handler (ssweb-js :uri "/ssweb.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))

    (defun *planet (radius color)
      (let ((material (new (3fn *line-basic-material (create color color))))
	    (geometry (new (3fn *circle-geometry radius 100))))
	((@ geometry vertices shift))
	(new (3fn *line geometry material))))

    (defun webgl-available ()
      (try
       (let ((canvas ((@ document create-element) "canvas")))
	 (not
	  (and (@ window *wb-g-l-rendering-context)
	       (or ((@ canvas get-context) "webgl")
		   ((@ canvas get-context) "experimental-webgl")))))
       (:catch (e) false)))

    (defun plot (document)
      (let ((p
	     (create
	      width 640
	      height 640
	      fov 75
	      near 1
	      far 4000
	      origin (new (3fn *vector3 0 0 0))
	      renderer (if (@ window *web-g-l-rendering-context)
			   ;;(if (webgl-available)
			   (new (3fn *web-g-l-renderer))
			   (new (3fn *canvas-renderer)))
	      traj-div ((@ document get-element-by-id) "trajectory")
	      camera nil
	      scene (new (3fn *scene))
	      traj3d nil
	      trajseg nil
	      traj-final nil
	      geometry nil
	      material nil
	      geometry-final nil
	      material-final nil
	      node nil
	      nodes nil
	      planets
	      (create
	       sun (new (*planet 0.01 0xffff00))
	       mercury (new (*planet 0.387 0x888888))
	       venus (new (*planet 0.723 0xffff44))
	       earth (new (*planet 1 0x0000ff))
	       mars (new (*planet 1.5 0xff0000))
	       ceres (new (*planet 2.77 0x666666))
	       jupiter (new (*planet 5.2 0xff9900))))))
	(with-slots (width height fov near far
			   renderer traj-div camera scene planets origin) p
	  ((@ renderer set-size) width height)
	  ((@ traj-div append-child) (@ renderer dom-element))
	  (setf camera (new (3fn *perspective-camera fov (/ width height) near far)))
	  ((@ camera position set) 0 0 10)
	  ((@ camera look-at) origin)
	  ((@ scene add) camera)
	  (for-in (planet planets)
		  ((@ scene add) (aref planets planet))))
	p))
    
    (defvar ssplot (new (plot document)))
    
    (defun sail (w)
      (let ((g (new (3fn *geometry)))
	    (l (* w (sqrt 2)))
	    (ms (new (3fn *mesh-basic-material
			  (create 
			   color 0x444444
			   side (@ *three* *double-side)))))
	    (mb (new (3fn *mesh-basic-material
			  (create
			   wireframe t
			   color 0x444444)))))
	;; Update sail geometry
	(with-slots (vertices faces) g
	  ((@ vertices push)
	   (new (3fn *vector3 0 0 0))
	   (new (3fn *vector3 0 l 0))
	   (new (3fn *vector3 0 0 l))
	   (new (3fn *vector3 0 (- l) 0))
	   (new (3fn *vector3 0 0 (- l))))
	  ((@ faces push)
	   (new (3fn *face3 0 1 2))
	   (new (3fn *face3 0 2 3))
	   (new (3fn *face3 0 3 4))
	   (new (3fn *face3 0 4 1))))
	((@ g compute-face-normals))
	((@ g compute-bounding-sphere))
	;; Output object
	(list
	 (new (3fn *mesh g ms))
	 (new (3fn *mesh ((@ g clone)) mb)))))
    
    (defun update-trajectory (plot document)
      (let ((lightness (parse-float (@ (aref ((@ document get-elements-by-name) "lightness") 0) value)))
	    (angles
	     (mapcar #'(lambda (anglestring)
			 (* (parse-float (@ anglestring value)) (lisp *deg*)))
		     ((@ document get-elements-by-name) "angles[]")))
	    (durations
	     (mapcar #'(lambda (durationstring)
			 (* (parse-float (@ durationstring value)) (lisp *years*)))
		     ((@ document get-elements-by-name) "durations[]"))))
	((@ $ get-j-s-o-n) "ssprop" (create lightness lightness
					    angles angles
					    durations durations)
	 (lambda (trajectory)
	   (with-slots (traj3d traj-final scene nodes geometry material fov camera renderer) plot
	   (when traj3d ((@ scene remove) traj3d))
	   (when traj-final ((@ scene remove) traj-final))
	   (when nodes
	     (dolist (node nodes)
	       (dolist (o node)
		 ((@ scene remove) o))))
	   (setf nodes (list))
	   (setf geometry (new (3fn *geometry)))
	   (setf material (new (3fn *line-basic-material (create color 0x00ff00))))
	   (setf geometry-final (new (3fn *geometry)))
	   (setf (@ geometry vertices)
		 (loop with rmax = 0
		    with n = (length trajectory)
		    for i = 1 then (incf i)
		    for segment in trajectory
		    for vertices =
		      (loop for (tm x y) in segment
			 for v = (new (3fn *vector3 x y 0))
			 collect v
			 do (setq rmax (max rmax ((@ v length)))))
		    if (< i n) 
		    append vertices
		    else do (setf (@ geometry-final vertices) vertices)
		    finally
		      ;; Scale camera position to fit trajectory
		      (let ((rcam 
			     (* 1.1 (/ rmax (tan (/ (* fov (/ pi 180)) 2))))))
			((@ camera position set) 0 0 rcam)
			(setf material-final
			      (new (3fn *line-dashed-material
					(create color 0x333333
						gap-size (/ rcam 50)
						dash-size (/ rcam 50))))))
		      ;; Draw sails at control nodes
		      (loop for segment in trajectory
			 for angle in angles
			 for (tm0 x0 y0) = (aref segment 0)
			 for node = (new (sail (/ rmax 20)))
			 for theta = (atan (/ y0 x0))
			 do (dolist (o node)
			      ((@ o position set) x0 y0 0)
			      (setf (@ o rotation z) (+ angle theta))
			      ((@ scene add) o))
			   ((@ nodes push) node))))
	   ((@ geometry-final compute-line-distances))
	   (setq traj3d (new (3fn *line geometry material)))
	   (setq traj-final (new (3fn *line geometry-final material-final)))
	   ((@ scene add) traj3d)
	   ((@ scene add) traj-final)
	   ((@ renderer render) scene camera))))))
    
    (defvar control-count
      (@ ((@ document get-elements-by-name) "angles[]") length))
    (defun new-control (document plot)
      (incf control-count)
      (let ((div1 ((@ document create-element) "div"))
	    (template
	     (who-ps-html
	      (:table
	       (:tr
		(:td "Angle")
		(:td (:input :type "number" :min "-90" :max "90" :step "5" :value 0 :name "angles[]"))
		(:td "Duration")
		(:td (:input :type "number" :min "0" :max "10" :step "0.1" :value "1" :name "durations[]"))
		(:td (:div :class "delete" (:b "Delete"))))))))
	(setf (@ div1 id) control-count)
	(setf (@ div1 inner-h-t-m-l) template)
	((@ ((@ document get-element-by-id) "controls")
	    append-child) div1)
	(update-trajectory plot document)))

    (defun delete-control (element-id document plot)
      (let* ((d document)
	     (element ((@ d get-element-by-id) element-id))
	     (parent-element ((@ d get-element-by-id) "controls")))
	((@ parent-element remove-child) element)
	(update-trajectory plot document)
	(update-url)))

    ((@ ($ document) ready) (update-trajectory ssplot document))
    
    (defun update-input-events ()
      ((@ ($ "input") off) "change")
      ((@ ($ "input") change)
       (lambda ()
	 (update-trajectory ssplot document)
	 (update-url))))

    (defun update-delete-events ()
      ((@ ($ ".delete") off) "click")
      ((@ ($ ".delete") click)
       (lambda ()
	 (let ((element-id
		(@ this parent-element parent-element parent-element parent-element parent-element id)))
	   (delete-control element-id document ssplot)))))

    (defun update-url ()
      (let ((lightness
	     (@ (aref ((@ document get-elements-by-name) "lightness") 0)
		value))
	    (angles 
	     (mapcar
	      #'(lambda (as) (@ as value))
	      ((@ document get-elements-by-name) "angles[]")))
	    (durations
	     (mapcar
	      #'(lambda (ds) (@ ds value))
	      ((@ document get-elements-by-name) "durations[]"))))
	((@ window history push-state)
	 ""
	 ""
	 (+ (@ window location pathname) "?"
	    ((@ $ param)
	     (create
	      lightness lightness
	      angles angles
	      durations durations))))))

    (update-input-events)

    (update-delete-events)

    ((@ ($ "#addcontrol") click)
     (lambda ()
       (new-control document ssplot)
       (update-input-events)
       (update-delete-events)
       (update-url)))))
