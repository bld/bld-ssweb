(in-package :bld-ssweb)

(define-easy-handler (directions :uri "/directions.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/directions.html")
     (:meta :property "og:title" :content "Directions of the Sunlight")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/solar_sail_flight_school.png")
     (:meta :property "og:description" :content "Lesson 5: Learn the directions that light is absorbed and reflected off of a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Directions of the Sunlight")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Directions of the Sunlight")
	    (:ul
	     (:li "To sail, we need directions to point the reflected sunlight.")
	     (:li (:em :id "normalcolor" "Normal:") " the direction the sail points - away from the sun.")
	     (:li (:em :id "incidentcolor" "Incident:") " the direction from the sun to the sail.")
	     (:li (:em :id "tangentialcolor" "Tangential:") " the direction at a right angle (90 degrees) to the sunlight and aligned with the sail rotation."
		  (:ul (:li "Doesn't exist when the sail point straight at the sun.")))
	     (:li (:em :id "reflectedcolor" "Reflected:") " the direction the reflected sunlight shines.")
	     (:li (:b "Challenge:")
		  (:ul (:li "Point the sail " (:em :id "normalcolor" "normal") " at an angle to the sun.")
		       (:li "Rotate it through 360 degrees."))))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td (:b "Absorbed")) (:td :id "absorbed" "100") (:td "%"))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "Rotate the view by clicking or touching and dragging.")
		  (:li "To zoom:"
		       (:ul
			(:li "Roll a mouse wheel")
			(:li "Hold the middle mouse button and move up and down")
			(:li "Pinch and spread two fingers on a touch screen")))
		  (:li "To steer the sail:"
		       (:ul
			(:li "Up and down arrows to tilt")
			(:li "Right and left arrows to rotate")))))
      (:div :id "nav"
	    (:h2 (:a :href "/magnitudes.html" "Next: Magnitude of Sunlight in each Direction"))
	    (:h2 (:a :href "/reflect.html" "Previous: Sunlight Absorbed by a Sail"))
	    (:h2 (:a :href "/index.html" "Home")))
      (:img :id "up" :src "img/arrow_up.svg")
      (:img :id "down" :src "img/arrow_down.svg")
      (:img :id "left" :src "img/arrow_left.svg")
      (:img :id "right" :src "img/arrow_right.svg")
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  ;; Turn off help text
	  (toggle-div "helpText")
	  ;; Initialize scene
	  (init document window)
	  ((@ camera position multiply-scalar) 2)
	  (init-orbit-controls)
	  (init-sail-parts)
	  ;; Add mirror geometry to sail for absorb/reflect
	  (setq mirror (new (*mirror)))
	  ((@ sail add) mirror)
	  (setq corners (new (*corners mirror)))
	  (setq projection (new (*projection mirror)))
	  (setq incident (new (*incident)))
	  (setq normal (new (*normal mirror)))
	  (setq reflect (new (*reflect incident normal)))
	  (setq reflection (new (*reflection corners)))
	  ((@ scene add) projection)
	  ((@ scene add) reflection)
	  ;; Direction arrows
	  (init-arrows)
	  ;; Tilt controls
	  (setq tilt-update-fn #'update-tilt-direction)
	  (init-tilt-controls)
	  ;; Start animation
	  (animate))))))))
