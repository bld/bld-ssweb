;;; Parts of a solar sail

(in-package :bld-ssweb)

(define-easy-handler (parts-js :uri "/js/parts.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    
    (defun init-parts (document window)
      (init document window)
      (init-orbit-controls)
      (init-sail-parts)
      ((@ document add-event-listener) "click" on-parts-click false))
    
    (defun on-parts-click (event)
      (setf (@ mouse x) (- (* 2 (/ (@ event client-x) window.inner-width)) 1))
      (setf (@ mouse y) (- 1 (* 2 (/ (@ event client-y) window.inner-height))))
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
    ))
  
(define-easy-handler (parts :uri "/parts.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/parts.html")
     (:meta :property "og:title" :content "Parts of a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/solar_sail_flight_school.png")
     (:meta :property "og:description" :content "Lesson 2: Learn about the parts of a solar sail spacecraft.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Parts of a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:script :src "js/parts.js")
     (:body
      (:div :id "info"
	    (:h1 "Parts of a Solar Sail")
	    (:ul (:li (:b "Challenge:") "Click on and read about the 4 major parts of the sail."))
	    (:div :id "sails" :class "hidden"
		  (:h2 "Sails")
		  (:ul
		   (:li "The sails are large, thin, reflective sheets that are pushed by sunlight.")
		   (:li "Current designs use a thin film of space qualified plastic like Kapton that is coated with a mirrored layer of aluminum.")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Sail_Film" "Sail Film"))))
	    (:div :id "booms" :class "hidden"
		  (:h2 "Booms")
		  (:ul
		   (:li "Something needs to hold the sails flat so they don't collapse under the gentle pressure of sunlight.")
		   (:li "Many sail designs use structural booms in a kite-shape to keep the sail flat against the gentle pressure of sunlight.")
		   (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/IKAROS" "IKAROS") " spins, using centripetal acceleration to hold the sail flat.")
		   (:li "For more information see: "
			(:ul
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Three-Axis_Stabilized" "Three-Axis Stabilized"))
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Spin_Stabilized" "Spin Stabilized"))))))
	    (:div :id "bus" :class "hidden"
		  (:h2 "Spacecraft bus")
		  (:p "The spacecraft bus houses all the systems required to operate the sail and carry out the mission. It typically contains:")
		  (:ul
		   (:li "Flight computer")
		   (:li "Solar arrays and power system")
		   (:li "Radios and antennas for communication")
		   (:li "Scientific instruments and other payloads")
		   (:li "Navigation and steering sensors")))
	    (:div :id "vanes" :class "hidden"
		  (:h2 "Steering vanes")
		  (:ul
		   (:li "Steering vanes are small sails at the boom tips that can rotate.")
		   (:li "They push the main sail to point in the direction you want to go.")
		   (:li "They work like airplane ailerons and trim tabs to steer and hold the orientation of the sail. ")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Attitude_Control" "Attitude Control")))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:ul
		   (:li "Click or tap on the parts of the sail for a description.")
		   (:li "Rotate the view by clicking or touching and dragging.")
		   (:li "To zoom:"
			(:ul
			 (:li "Roll the mouse wheel")
			 (:li "Hold the middle mouse button and move up and down")
			 (:li "Pinch and spread two fingers on a touch screen"))))))
      (:div :id "nav" 
	    (:h2 (:a :href "/absorb.html" "Next: Sunlight Absorbed by a Sail"))
	    (:h2 (:a :href "/what.html" "Previous: What is a Solar Sail?"))
	    (:h2 (:a :href "/index.html" "Home")))
      (:div :id "plot")
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  #+null(init-parts document window)
	  #+null(animate)
	  (defvar app (parts))
	  ((@ app init))
	  ((@ app animate))
	  )))
      (:script :src "js/googleanalytics.js")))))
