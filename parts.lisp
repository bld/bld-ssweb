;;; Parts of a solar sail

(in-package :bld-ssweb)

(define-easy-handler (parts-js :uri "/parts.js") ()
  (setf (content-type*) "text/javascript")
  (ps

    (defmacro 3fn (fn &rest args)
      `((@ *three* ,fn) ,@args))
    
    (defmacro 3xfn (fn &rest args)
      `((@ *t-h-r-e-ex ,fn) ,@args))

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

    (defun *sails-mirror (scene)
      (with-slots (loader file loadfn obj load mcam) this
	;; Mirror camera
	(let ((mirror-camera (new (3fn *cube-camera 1 5000 512)))
	      (sail this))
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
				   reflectivity 0.9
				   ;;side (@ *t-h-r-e-e *double-side)
				   ))))
			   (obj (new (3fn *mesh geometry mat))))
		      (setf (@ obj name) "sails")
		      (setf (@ sails obj) obj)
		      ((@ scene add) obj)
		      ((@ target-list push) obj)
		      (render)))
		load #'(lambda () ((@ loader load) file loadfn)))))
      this)

    (defun *vane-mirror (scene yrot)
      (with-slots (loader file loadfn obj load mcam select-objs) this
	;; Selection objects
	(setq select-objs (list))
	(let ((xzs '((1 0)(0 1)(-1 0)(0 -1))))
	  (dotimes (i 4)
	    (let ((sbox (new (*selection-box "vanes" .5 1 1)))
		  (x (@ (aref xzs i) 0))
		  (z (@ (aref xzs i) 1))
		  (angle (* i (/ pi 2))))
	      (setf (@ sbox position x) (* 4.25 x)
		    (@ sbox position z) (* 4.25 z))
	      (setf (@ sbox visible) false)
	      ((@ sbox rotate-y) angle)
	      ((@ select-objs push) sbox)
	      ((@ scene add) sbox)
	      ((@ target-list push) sbox))))
	;; Load vane geometry from file, with cube camera reflection
	(let ((vane-camera (new (3fn *cube-camera 1 1000 512)))
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

    (defun *booms (scene renderer camera)
      (with-slots (loader file loadfn obj load select-obj1 select-obj2) this
	;; Selection objects
	(setf select-obj1 (new (*selection-box "booms" .4 .4 8)))
	(setf (@ select-obj1 visible) false)
	((@ scene add) select-obj1)
	((@ target-list push) select-obj1)
	(setf select-obj2 (new (*selection-box "booms" 8 .4 .4)))
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
		    (setf (@ obj name) "booms")
		    (setf (@ booms obj) obj)
		    ((@ scene add) obj)
		    ((@ target-list push) obj)
		    ((@ renderer render) scene camera)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (defun *bus (scene renderer camera)
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
		    (setf (@ obj name) "bus")
		    (setf (@ bus obj) obj)
		    ((@ scene add) obj)
		    ;;((@ target-list push) obj)
		    ((@ renderer render) scene camera)))
	      load #'(lambda () ((@ loader load) file loadfn)))
	this))

    (let (origin plot-div renderer camera scene light alight controls
		 sails vanes vane-cameras booms bus
		 stars sun
		 projector target-list mouse
		 part-name)

      (defun on-document-mouse-click (event)
	(setf (@ mouse x) (- (* 2 (/ (@ event client-x) (div-width "plot"))) 1))
	(setf (@ mouse y) (- 1 (* 2 (/ (@ event client-y) (div-height "plot")))))
	(defvar vector (new (3fn *vector3 (@ mouse x) (@ mouse y) 1)))
	((@ projector unproject-vector) vector camera)
	(defvar raycaster (new (3fn *raycaster (@ camera position) ((@ ((@ vector sub) (@ camera position)) normalize)))))
	(defvar intersects ((@ raycaster intersect-objects) target-list))
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
	      (setq part-name ""))))	      

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
	((@ renderer set-size) (div-width "plot") (div-height "plot"))
	((@ plot-div append-child) (@ renderer dom-element))
	;; Scene camera
	(setq camera (new (3fn *perspective-camera 75 (/ (div-width "plot") (div-height "plot")) 0.1 1000)))
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
	((@ controls add-event-listener) "change" #'render)
	;; Setup clicking parts of sail
	(setq target-list (list))
	(setq projector (new (3fn *projector)))
	(setq mouse (create x 0 y 0))
	((@ document add-event-listener) "click" on-document-mouse-click false)
	;; Automatically resize window
	((@ window add-event-listener) "resize" on-window-resize false)
        ;; Parts of the sail
	;; Booms
	(setq booms (new (*booms scene renderer camera)))
	((@ booms load))
	;; Bus
	(setq bus (new (*bus scene renderer camera)))
	((@ bus load))
	;; Sail
	(setq sails (new (*sails-mirror scene)))
	((@ sails load))
	;; Vanes
	(setq vanes (list))
	(dotimes (i 4)
	  (let* ((yrot (* i (/ pi 2)))
		 (vane (new (*vane-mirror scene yrot))))
	    (setf (aref vanes i) vane)))
	(dolist (vane vanes) ((@ vane load))))
      
      (defun div-width (div-id)
	(if (< window.inner-width window.inner-height)
	    window.inner-width
	    window.inner-width))
      
      (defun div-height (div-id)
	(if (< window.inner-height window.inner-width)
	    window.inner-height
	    window.inner-height))

      (defun on-window-resize ()
	(setf (@ camera aspect) (/ (div-width "plot") (div-height "plot")))
	((@ camera update-projection-matrix))
	((@ renderer set-size) (div-width "plot") (div-height "plot"))
	((@ camera look-at) origin)
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
	((@ controls update))))))
  
(define-easy-handler (parts :uri "/parts") ()
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      :title "Parts of a Solar Sail"
      (:link :href "sail.css" :rel "stylesheet")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :src "js/TrackballControls.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*))))
      (:script :src "parts.js")
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (defun toggle-div (div-id)
	    ((@ ($ (concatenate 'string "#" div-id)) toggle)))))))
     (:body
      (:div :id "screen"
	    (:div :id "info"
		  (:h1 "Parts of a Solar Sail")
		  (:div :id "sails" :class "hidden"
			(:h2 "Sails")
			(:p "The sails are large sheets of a thin, reflective material that push the spacecraft by the pressure sunlight. This is typically a thin film of space qualified plastic like Kapton that is coated with a shiny, mirrored layer of aluminum. For more information see" (:a :href "http://wiki.solarsails.info/index.php/Category:Sail_Film" "Sail Film")))
		  (:div :id "booms" :class "hidden"
			(:h2 "Booms")
			(:p "The booms are structural members that hold the sail out and keep it flat against the gentle pressure of sunlight. For more information see: " (:a :href "http://wiki.solarsails.info/index.php/Three-Axis_Stabilized" "Three-Axis Stabilized")))
		  (:div :id "bus" :class "hidden"
			(:h2 "Spacecraft bus")
			(:p "The spacecraft bus houses all the systems required to operate the sail and carry out the mission. It may contain:")
			(:ul
			 (:li "Flight computer")
			 (:li "Solar arrays and power system")
			 (:li "Radios and antennas for communication")
			 (:li "Scientific instruments and other payloads")))
		  (:div :id "vanes" :class "hidden"
			(:h2 "Steering vanes")
			(:p "Steering vanes are small sails that can be steered to change the direction the sails point, and thus the direction of thrust that propels the spacecraft. Vanes are one of several different ways to steer a sail. IKAROS used electronic reflective control devices on the edges of the sail. For more information see " (:a :href "http://wiki.solarsails.info/index.php/Category:Attitude_Control" "Attitude Control"))))
	    (:div :id "help"
		  (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
		  (:div :id "helpText"
			(:ul
			 (:li "Click or tap on the parts of the sail for a description.")
			 (:li "Move the view by clicking or touching and dragging.")
			 (:li "Zoom by rolling a mouse wheel or pinching/spreading two fingers on a touch screen."))))
	    (:div :id "plot"))
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (init document window)
	  (animate))))))))
