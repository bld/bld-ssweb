;;; Force from reflected sunlight

(in-package :bld-ssweb)

(define-easy-handler (reflect-force :uri "/reflect-force.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/reflect-force.html")
     (:meta :property "og:title" :content "Force from Reflected Sunlight on a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/reflect_image.png")
     (:meta :property "og:description" :content "Lesson 6: Learn how reflected sunlight pushes a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Force from Reflected Sunlight on a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/decround.js")
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Force from Reflected Sunlight on a Solar Sail")
	    (:ul
	     (:li "Reflected light pushes back on the sail in the opposite direction it shines, like rocket and jet exhaust")
	     (:li "The arrows show the direction and amount of:"
		  (:ul (:li (:span :class "light" "Sunlight"))
		       (:li (:span :class "force" "Acceleration & force"))
		       (:li (:span :class "velocity" "Velocity & speed"))))
	     (:li (:b "Challenge:") "Fly the sail into the red target.")
	     (:li "Hit the spacebar or click Continue to start."))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td (:b "Acceleration")) (:td :id "accel" "1") (:td "mm/s" (:sup "2")))
	     (:tr (:td (:b "Speed")) (:td :id "speed" "0") (:td "mm/s"))
	     (:tr (:td (:b "Distance")) (:td :id "distance" "0") (:td "m"))
	     (:tr (:td (:b "Time") "(100X)") (:td :id "elapsed" "0") (:td "min:sec"))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "The sail resets after travelling 100 m, or hit \"Reset\" button.")
		  (:li "Click \"Pause\" or hit spacebar to pause.")
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
	    (:button :id "pause" "Pause")
	    (:button :id "reset" "Reset")
	    (:p (:b (:a :href "/force.html" "Next: Force from Sunlight on a Solar Sail")))
	    (:p (:b (:a :href "/absorb-force.html" "Previous: Force from Absorbed Sunlight on a Solar Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))
      (:img :id "up" :src "img/arrow_up.svg")
      (:img :id "down" :src "img/arrow_down.svg")
      (:img :id "left" :src "img/arrow_left.svg")
      (:img :id "right" :src "img/arrow_right.svg")
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (defvar app (reflect-force))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
