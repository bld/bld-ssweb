;;; Sunlight absorbed by a sail

(in-package :bld-ssweb)

(define-easy-handler (absorb :uri "/absorb.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/absorb.html")
     (:meta :property "og:title" :content "Sunlight Absorbed by a Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/absorb_image.png")
     (:meta :property "og:description" :content "Lesson 3: Learn how light strikes a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Sunlight Absorbed by a Sail")
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
	    (:h1 "Sunlight Absorbed by a Sail")
	    (:ul
	     (:li "The yellow box shows the sunlight that is absorbed by the sail.")
	     (:li "The arrow shows the direction of the sunlight, and its length shows how much hits the sail.")
	     (:li "Observe how the volume of the box, length of the arrow, and absorbed % change as the sail tilts " (:em "(sun incidence)."))
	     (:li (:b "Challenge:") "Find the sun incidence angles where the sunlight absorbed is 100%, 50%, and 0%.")
	     (:li (:b "Extra credit:") "Calculate the " (:em "cosine") " of the sun incidence angle and compare to the absorbed %."))
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
	    (:h2 (:a :href "/reflect.html" "Next: Sunlight Reflected by a Sail"))
	    (:h2 (:a :href "/parts.html" "Previous: Parts of a Solar Sail"))
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
	  (toggle-div "helpText")
	  (init document window)
	  (init-orbit-controls)
	  (init-sail-parts)
	  (init-absorb)
	  (init-tilt-controls)
	  (animate))))
      (:script :src "js/googleanalytics.js")))))
    
