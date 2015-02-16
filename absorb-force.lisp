;;; Sunlight absorbed by a sail

(in-package :bld-ssweb)

(define-easy-handler (absorb-force :uri "/absorb-force.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info:8080/absorb-force.html")
     (:meta :property "og:title" :content "Force from Absorbed Sunlight on a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/absorb_image.png")
     (:meta :property "og:description" :content "Lesson 5: Learn how absorbed sunlight pushes a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Force from Absorbed Sunlight on a Solar Sail")
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
	    (:h1 "Force from Absorbed Sunlight on a Solar Sail")
	    (:ul
	     (:li "Sunlight pushes a sail in the direction it shines. The force is proportional to the light absorbed.")
	     (:li (:b "Challenge:") "At what sun incidence angles is the acceleration maximum, half, and zero?"))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     ;;(:tr (:td (:b "Absorbed")) (:td :id "absorbed" "100") (:td "%"))
	     (:tr (:td (:b "Acceleration")) (:td :id "accel" "1") (:td "mm/s^2"))
	     (:tr (:td (:b "Speed")) (:td :id "speed" "0") (:td "mm/s"))
	     (:tr (:td (:b "Distance")) (:td :id "distance" "0") (:td "m"))
	     (:tr (:td (:b "Time")) (:td :id "elapsed" "0") (:td "min:sec"))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "The scene resets after the sail travels 5 m.")
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
	    (:button :id "pause" :name "pause" "Pause")
	    (:p (:b (:a :href "http://www.patreon.com/bld" "Next: Help develop it at Patreon")))
	    (:p (:b (:a :href "/reflect.html" "Previous: Sunlight reflecting off of a solar sail")))
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
	  (init document window)
	  (init-orbit-controls)
	  (init-sail-parts)
	  (init-absorb-force)
	  (init-tilt-controls)
	  (animate-force))))
      (:script :src "js/googleanalytics.js")))))
