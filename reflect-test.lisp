(in-package :bld-ssweb)

(define-easy-handler (reflect-js :uri "/reflect.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))

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
    
    (defun *mirror-cube ()
      (with-slots (object mirror-camera) this
	(setf mirror-camera (new (3fn *cube-camera 0.1 5000 512)))
	(setf object
	      (new
	       (3fn
		*mesh 
		(new (3fn *cube-geometry 1000 1000 10 1 1 1))
		(new (3fn *mesh-basic-material
			  (create env-map (@ mirror-camera render-target)))))))
	((@ object position set) 0 0 0)
	(setf (@ mirror-camera position) (@ object position)))
      this)

    (defun *mirror-square ()
      (with-slots (object mirror-camera) this
	(setf mirror-camera (new (3fn *cube-camera 0.1 5000 512)))
	(let* ((m (new (3fn *mesh-basic-material (create env-map (@ mirror-camera render-target) side (@ *three* *double-side)))))
	;;(let* ((m (new (3fn *mesh-basic-material (create color 0x999999 side (@ *three* *double-side)))))
	       (v (list (new (3fn *vector3 500 500 0))
			(new (3fn *vector3 500 -500 0))
			(new (3fn *vector3 -500 -500 0))
			(new (3fn *vector3 -500 500 0))))
	       (f (list (new (3fn *face3 0 1 2))
			(new (3fn *face3 2 3 0))
			(new (3fn *face3 0 3 2))
			(new (3fn *face3 2 1 0))))
	       (g (new (3fn *geometry))))
	  (setf (@ g vertices) v
		(@ g faces) f)
	  ;;((@ g compute-face-normals))
	  ;;((@ g compute-bounding-sphere))
	  (setf object (new (3fn *mesh g m)))
	  ((@ object position set) 0 0 0)
	  (setf (@ mirror-camera position) (@ object position)))
	(console.log object))
      this)

    (defun *sun ()
      (let ((g (new (3fn *sphere-geometry 100 32 32)))
	    (m (new (3fn *mesh-basic-material (create color 0xf9ffd9)))))
	(new (3fn *mesh g m))))

    (let (origin plot-div renderer camera mirror-cube-camera scene alight controls mirror stars sun)
      
      (defun init (document window)
	;; Scene
	(setq scene (new (3fn *scene)))
	;; Camera
	(setq camera (new (3fn *perspective-camera 75 (/ (@ window inner-width) (@ window inner-height)) 0.1 5000)))
	((@ scene add) camera)
	((@ camera position set) 0 150 400)
	((@ camera look-at) (@ scene position))
	;; Renderer
	(setq renderer
	      (if (@ window *web-g-l-rendering-context)
		  (new (3fn *web-g-l-renderer (create antialias true)))
		  (new (3fn *canvas-renderer))))
	((@ renderer set-size) (@ window inner-width) (@ window inner-height))
	(setq plot-div ((@ document get-element-by-id) "ThreeJS"))
	((@ plot-div append-child) (@ renderer dom-element))
	;; Controls
	(setq controls (new (3fn *trackball-controls camera)))
	(setf 
	 ;;(@ controls rotate-speed) 2
	 ;;(@ controls zoom-speed) 2
	 (@ controls pan-speed) 0.8
	 (@ controls no-pan) true
	 (@ controls no-zoom) false
	 ;;(@ controls static-moving) true
	 ;;(@ controls dynamic-damping-factor) 0.3
	 ;;(@ controls keys) (list 65 83 68)
	 )
	;; Light
	;;(setq light (new (3fn *ambient-light 0xffffff)))
	(setq light (new (3fn *point-light 0xffffff)))
	;;((@ light position set) 0 250 0)
	((@ scene add) light)
	;; Stars
	(setq stars (new (*stars)))
	((@ scene add) stars)
	;; Sun
	(setq sun (new (*sun)))
	((@ sun translate-z) 1000)
	((@ scene add) sun)
	;; Mirror
	(setq mirror (new (*mirror-square)))
	((@ scene add) (@ mirror object))
	((@ scene add) (@ mirror mirror-camera)))

      (defun animate ()
	(request-animation-frame animate)
	(render)
	(update))

      (defun render ()
	(setf (@ mirror object visible) false)
	((@ mirror mirror-camera update-cube-map) renderer scene)
	(setf (@ mirror object visible) true)
	((@ renderer render) scene camera))

      (defun update ()
	((@ controls update))))))

(define-easy-handler (reflect :uri "/reflect") ()
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      :title "Stars Reflection Test"
      (:link :href "sail.css" :rel "stylesheet")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :src "js/TrackballControls.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*))))
      (:script :src "reflect.js"))
     (:body
      (:div :id "info"
	    (:h1 "Stars Reflection Test"))
      (:div :id "ThreeJS")
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (init document window)
	  (animate))))))))
