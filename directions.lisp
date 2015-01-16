(in-package :bld-ssweb)

(define-easy-handler (directions :uri "/directions.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/directions.html")
     (:meta :property "og:title" :content "Directions of the Light Absorbed and Reflecting off of a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/solar_sail_flight_school.png")
     (:meta :property "og:description" :content "Lesson 5: Learn how to distinguish between the directions that light reflects off of a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Directions of the Light Absorbed and Reflecting off of a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Directions of the Light Absorbed and Reflecting off of a Solar Sail")
	    (:ul
	     (:li "To sail, we need directions to point the reflected sunlight.")
	     (:li (:em :id "normalcolor" "Normal:") " the direction the sail points - away from the sun.")
	     (:li (:em :id "incidentcolor" "Incident:") " the direction of the sunlight striking the sail.")
	     (:li (:em :id "tangentialcolor" "Tangential:") " the direction at a right angle (90 degrees) to the sunlight."
		  (:ul (:li "Doesn't exist when the sail is aligned with the sunlight.")))
	     (:li (:em :id "reflectedcolor" "Reflected:") " the direction the reflected sunlight shines.")
	     (:li (:b "Challenge:") "TBD"))
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
	    (:h2 (:a :href "http://www.patreon.com/bld" "Next: Help build it at Patreon"))
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
	  (defvar normal-arrow (new (*normal-arrow origin normal)))
	  (defvar incident-arrow (new (*incident-arrow origin normal)))
	  (defvar tangential (new (*tangential incident normal)))
	  (defvar tangential-arrow (new (*tangential-arrow origin tangential)))
	  (defvar reflection-arrow (new (3fn *arrow-helper reflect origin 10 0xffff00)))
	  ((@ scene add) normal-arrow)
	  ((@ scene add) incident-arrow)
	  ((@ scene add) tangential-arrow)
	  ((@ scene add) reflection-arrow)
	  ;; Tilt controls
	  (setq tilt-update-fn #'update-tilt-direction)
	  (init-tilt-controls)
	  ;; Start animation
	  (animate))))))))
