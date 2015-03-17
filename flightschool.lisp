(in-package :bld-ssweb)

(define-easy-handler (flightschool-js :uri "/js/flightschool.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro with-three (&body body)
      "Use THREE library methods"
      `(with-slots (;; Scene
		    *scene
		    ;; Other
		    *vector3
		    *face3
		    -j-s-o-n-loader
		    *raycaster
		    *trackball-controls
		    *projector
		    ;; Cameras
		    *cube-camera
		    *perspective-camera
		    ;; Renderers
		    *web-g-l-renderer
		    *canvas-renderer
		    ;; Lights
		    *directional-light
		    *ambient-light
		    ;; Objects
		    *object3-d
		    *mesh
		    *arrow-helper
		    *point-cloud
		    *line
		    ;; Geometries
		    *geometry
		    *sphere-geometry
		    *box-geometry
		    ;; Materials
		    *mesh-face-material
		    *mesh-phong-material
		    *point-cloud-material
		    *mesh-basic-material
		    *line-basic-material) *three*
	 ,@body))

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
    
    (with-three
	
      (defvar ssfs
	(create

	 app
	 (lambda ()
	   (let ((that (create)))
	     (with-slots (scene origin plot-div renderer render camera) that
	       (setf scene (new (funcall *scene))
		     origin (new (funcall *vector3 0 0 0))
		     plot-div ((@ document get-element-by-id) "plot")
		     renderer (if (@ window *web-g-l-rendering-context)
				  (new (funcall *web-g-l-renderer (create antialias true)))
				  (new (funcall *canvas-renderer)))
		     camera (new (funcall *perspective-camera 75 (/ (@ window inner-width) (@ window inner-height)) 0.1 1000))
		     render (lambda ()
			      ((@ renderer render) scene camera)))
	       ((@ camera position set) 2 -6 3)
	       ((@ camera look-at) origin)
	       ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
	       ((@ plot-div append-child) (@ renderer dom-element))
	       ((@ window add-event-listener) "resize"
		(lambda ()
		  (setf (@ camera aspect) (/ (@ window inner-width) (@ window inner-height)))
		  ((@ camera update-projection-matrix))
		  ((@ renderer set-size) (@ window inner-width) (@ window inner-height))
		  ((@ camera look-at) origin)
		  (funcall render))))
	     that))

	 add-stars
	 (lambda (app)
	   (with-slots (scene stars) app
	     (setf stars
		   (let ((g (new (funcall *geometry)))
			 (m (new (funcall *point-cloud-material
				      (create size 2))))
			 (r 500))
		     (dotimes (i 10000)
		       (let ((v (new (funcall *vector3
					  (- ((@ *math random)) 0.5)
					  (- ((@ *math random)) 0.5)
					  (- ((@ *math random)) 0.5)))))
			 ((@ v set-length) (* (+ 1 ((@ *math random))) r))
			 ((@ g vertices push) v)))
		     (new (funcall *point-cloud g m))))
	     ((@ scene add) stars)))

	 add-sun
	 (lambda (app)
	   (with-slots (scene sun) app
	     (setf sun
		   (let ((g (new (funcall *sphere-geometry 20 32 32)))
			 (m (new (funcall *mesh-basic-material (create color 0xf9ffd9)))))
		     (new (funcall *mesh g m))))
	     ((@ scene add) sun)))

	 add-ambient
	 (lambda (app)
	   (with-slots (scene alight) app
	     (setf alight (new (funcall *ambient-light 0xffffff)))
	     ((@ scene add) alight)))

	 add-sails
	 (lambda (app)
	   (with-slots (sail renderer render scene camera) app
	     (let ((sails (create)))
	       (with-slots (loader file loadfn load mcam object) sails
		 (setf mcam (new (funcall *cube-camera 1 5000 512)))
		 ((@ scene add) mcam)
		 (setf loader (new (funcall -j-s-o-n-loader t)))
		 (setf file "js/sails.js")
		 (setf loadfn
		       (let ((material
			      (new (funcall *mesh-phong-material
					    (create
					     env-map (@ mcam render-target)
					     reflectivity 0.9)))))
			 (lambda (geometry m)
			   (setf object (new (funcall *mesh geometry material)))
			   ((@ sail add) object)
			   (funcall render))))
		 (setf load (lambda ()
			      ((@ loader load) file loadfn)))
		 (setf render
		       (lambda ()
			 (setf (@ object visible) false)
			 ((@ mcam update-cube-map) renderer scene)
			 ((@ app superior) "render")
			 (setf (@ object visible) true)))
		 (funcall load)))))
	 
	 add-sail
	 (lambda (app)
	   (with-slots (scene sail) app
	     (setf sail (new (funcall *object3-d)))
	     ((@ ssfs add-sails) app)
	     ((@ scene add) sail)))
	 )))))

(define-easy-handler (flightschool-html :uri "/flightschool.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:title "Solar Sail Flight School")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/flightschool.js")
     (:body
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (defvar flightschool ((@ ssfs app)))
	  ((@ ssfs add-stars) flightschool)
	  ((@ ssfs add-sun) flightschool)
	  ((@ ssfs add-ambient) flightschool)
	  ((@ ssfs add-sail) flightschool)
	  (with-slots (renderer scene camera) flightschool
	    ((@ renderer render) scene camera))
	  )))))))


